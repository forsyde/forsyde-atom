{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SDF.Process
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF.Process  where

import ForSyDe.Core
import ForSyDe.MoC.SDF.Signal

-- | The `combSDF` take a combinatorial function as argument and returns a process with one input signals and one output signal.
combSDF :: Int -> Int -> ([a] -> [b]) -- ^ combinatorial function
       -> SignalSDF a -- ^ input signal
       -> SignalSDF b -- ^ output signal


-- | Behaves like 'combSDF', but the process takes 2 input signals.
comb2SDF :: (Int, Int) -> Int -> ([a] -> [b] -> [c]) -> SignalSDF a -> SignalSDF b -> SignalSDF c

-- | Behaves like 'combSDF', but the process takes 3 input signals.
comb3SDF :: (Int, Int, Int) -> Int -> ([a] -> [b] -> [c] -> [d]) -> SignalSDF a -> SignalSDF b -> SignalSDF c -> SignalSDF d

-- | Behaves like 'combSDF', but the process takes 4 input signals.
comb4SDF :: (Int, Int, Int, Int) -> Int -> ([a] -> [b] -> [c] -> [d] -> [e]) -> SignalSDF a -> SignalSDF b -> SignalSDF c -> SignalSDF d -> SignalSDF e

-- | The process constructor 'delaySDF' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delaySDF :: a -- ^Initial state 
         -> SignalSDF a -- ^Input signal
         -> SignalSDF a -- ^Output signal

-- | The process constructor 'delaynSDF' delays the signal n events by introducing n identical default values.   
delaynSDF :: a -- ^Initial state
          -> Int -- ^ Delay cycles 
          -> SignalSDF a -- ^Input signal
          -> SignalSDF a -- ^Output signal


-- | The process constructor 'filterSDF' discards the values who do not fulfill a predicate given by a predicate function and replaces them with absent events.
filterSDF :: (a -> Bool) -- Predicate function
         -> SignalSDF a -- Input signal
         -> SignalSDF a -- Output signal

-- | The process 'zipSDF' \"zips\" two incoming signals into one signal of tuples.
zipSDF  :: (Int,Int) -> SignalSDF a -> SignalSDF b -> SignalSDF ([a],[b])

-- | Works as 'zipSDF', but takes three input signals.
zip3SDF :: (Int,Int,Int) -> SignalSDF a -> SignalSDF b -> SignalSDF c -> SignalSDF ([a],[b],[c])

-- | Works as 'zipSDF', but takes four input signals.
zip4SDF :: (Int,Int,Int,Int) -> SignalSDF a -> SignalSDF b -> SignalSDF c -> SignalSDF d -> SignalSDF ([a],[b],[c],[d])

-- | Works as 'zipSDF', but takes four input signals.
zip5SDF :: (Int,Int,Int,Int,Int) -> SignalSDF a -> SignalSDF b -> SignalSDF c -> SignalSDF d -> SignalSDF e -> SignalSDF ([a],[b],[c],[d],[e])

-- | Works as 'zipSDF', but takes four input signals.
zip6SDF :: (Int,Int,Int,Int,Int,Int) -> SignalSDF a -> SignalSDF b -> SignalSDF c -> SignalSDF d -> SignalSDF e -> SignalSDF f -> SignalSDF ([a],[b],[c],[d],[e],[f])

-- | The process 'unzipSDF' \"unzips\" a signal of tuples into two signals.
unzipSDF  :: (Int,Int) -> SignalSDF ([a],[b]) -> (SignalSDF a, SignalSDF b)

-- | Works as 'unzipSDF', but has three output signals.
unzip3SDF :: (Int,Int,Int) -> SignalSDF ([a],[b],[c]) -> (SignalSDF a, SignalSDF b, SignalSDF c)

-- | Works as 'unzipSDF', but has four output signals.
unzip4SDF :: (Int,Int,Int,Int) -> SignalSDF ([a],[b],[c],[d]) -> (SignalSDF a,SignalSDF b,SignalSDF c,SignalSDF d)

-- | Works as 'unzipSDF', but has four output signals.
unzip5SDF :: (Int,Int,Int,Int,Int) -> SignalSDF ([a],[b],[c],[d],[e])  -> (SignalSDF a,SignalSDF b,SignalSDF c,SignalSDF d,SignalSDF e)

-- | Works as 'unzipSDF', but has four output signals.
unzip6SDF :: (Int,Int,Int,Int,Int,Int) -> SignalSDF ([a],[b],[c],[d],[e],[f]) -> (SignalSDF a,SignalSDF b,SignalSDF c,SignalSDF d,SignalSDF e,SignalSDF f)


combSDF c p f x 
    | c <= 0         = error "Number of consumed tokens must be positive integer"
    | otherwise      = checkout p $ f §- (c,x)

comb2SDF (c1,c2) p f x y 
    | c1 <= 0         = error "Number of consumed tokens must be positive integer"
    | c2 <= 0         = error "Number of consumed tokens must be positive integer"
    | otherwise      = checkout p $ f §- (c1,x) -§- (c2,y)

comb3SDF (c1,c2,c3) p f x y z
    | c1 <= 0         = error "Number of consumed tokens must be positive integer"
    | c2 <= 0         = error "Number of consumed tokens must be positive integer"
    | c3 <= 0         = error "Number of consumed tokens must be positive integer"
    | otherwise      = checkout p $ f §- (c1,x) -§- (c2,y) -§- (c3,z)

comb4SDF (c1,c2,c3,c4) p f x y z q
    | c1 <= 0         = error "Number of consumed tokens must be positive integer"
    | c2 <= 0         = error "Number of consumed tokens must be positive integer"
    | c3 <= 0         = error "Number of consumed tokens must be positive integer"
    | c4 <= 0         = error "Number of consumed tokens must be positive integer"
    | otherwise      = checkout p $ f §- (c1,x) -§- (c2,y) -§- (c3,z) -§- (c4,q)


delaySDF x xs = xs ->- x
delaynSDF x n xs | n <= 0    = xs
                 | otherwise = delaynSDF x (n-1) xs ->- x

filterSDF p xs = xs -#- p

zipSDF (c1,c2) xs ys                          = (,) §- (c1,xs) -§- (c2,ys)
zip3SDF (c1,c2,c3) xs ys zs                   = (,,) §- (c1,xs) -§- (c2,ys) -§- (c3,zs)
zip4SDF (c1,c2,c3,c4) xs ys zs as             = (,,,) §- (c1,xs) -§- (c2,ys) -§- (c3,zs) -§- (c4,as)
zip5SDF (c1,c2,c3,c4,c5) xs ys zs as bs       = (,,,,) §- (c1,xs) -§- (c2,ys) -§- (c3,zs) -§- (c4,as) -§- (c5,bs)
zip6SDF (c1,c2,c3,c4,c5,c6) xs ys zs as bs cs = (,,,,,) §- (c1,xs) -§- (c2,ys) -§- (c3,zs) -§- (c4,as) -§- (c5,bs) -§- (c6,cs)


unzipSDF (p1,p2) xs  =            (checkout p1 $ fst <$> xs, checkout p2 $ snd <$> xs)
unzip3SDF (p1,p2,p3) xs =         (checkout p1 $ (\(x,_,_) -> x) <$> xs,
                                   checkout p2 $ (\(_,x,_) -> x) <$> xs,
                                   checkout p3 $ (\(_,_,x) -> x) <$> xs)
unzip4SDF (p1,p2,p3,p4) xs =      (checkout p1 $ (\(x,_,_,_) -> x) <$> xs,
                                   checkout p2 $ (\(_,x,_,_) -> x) <$> xs,
                                   checkout p3 $ (\(_,_,x,_) -> x) <$> xs,
                                   checkout p4 $ (\(_,_,_,x) -> x) <$> xs)
unzip5SDF (p1,p2,p3,p4,p5) xs =   (checkout p1 $ (\(x,_,_,_,_) -> x) <$> xs,
                                   checkout p2 $ (\(_,x,_,_,_) -> x) <$> xs,
                                   checkout p3 $ (\(_,_,x,_,_) -> x) <$> xs,
                                   checkout p4 $ (\(_,_,_,x,_) -> x) <$> xs,
                                   checkout p5 $ (\(_,_,_,_,x) -> x) <$> xs)
unzip6SDF (p1,p2,p3,p4,p5,p6) xs =(checkout p1 $ (\(x,_,_,_,_,_) -> x) <$> xs,
                                   checkout p2 $ (\(_,x,_,_,_,_) -> x) <$> xs,
                                   checkout p3 $ (\(_,_,x,_,_,_) -> x) <$> xs,
                                   checkout p4 $ (\(_,_,_,x,_,_) -> x) <$> xs,
                                   checkout p5 $ (\(_,_,_,_,x,_) -> x) <$> xs,
                                   checkout p6 $ (\(_,_,_,_,_,x) -> x) <$> xs)

--------------- HELPER FUNCTIONS (not exported) -------------------------

checkout p out = if anyS (\x -> not $ length x == p) $ toS out then 
                   error "Function does not produce correct number of tokens" 
                 else tokenize out

