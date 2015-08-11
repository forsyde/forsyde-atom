{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC..Process
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF.Process (
  -- ** combinatorial process constructors
  comb, comb2, comb3, comb4,
  -- ** sequential process constructors
  delay, delayn,
  -- ** specific process constructors
  filter,
  -- ** zip\/unzip
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,
) where

import Prelude hiding (zip, zip3, filter, unzip, unzip3)
import ForSyDe.Core
import ForSyDe.MoC.SDF.Signal

-- | The `comb` take a combinatorial function and production and consumption rates as arguments and returns a process with one input signals and one output signal.
comb :: Int -- ^ production rate, 
     -> Int -- ^ consumption rate, 
     -> ([a] -> [b]) -- ^ combinatorial function on lists 
     -> Signal a -- ^ input signal
     -> Signal b -- ^ output signal


-- | Behaves like 'comb', but the process takes 2 input signals, and the production rate is presented as a tuple.
comb2 :: (Int, Int) -> Int -> ([a] -> [b] -> [c]) -> Signal a -> Signal b -> Signal c

-- | Behaves like 'comb', but the process takes 3 input signals, and the production rate is presented as a 3-tuple.
comb3 :: (Int, Int, Int) -> Int -> ([a] -> [b] -> [c] -> [d]) -> Signal a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'comb', but the process takes 4 input signals, and the production rate is presented as a 4-tuple.
comb4 :: (Int, Int, Int, Int) -> Int -> ([a] -> [b] -> [c] -> [d] -> [e]) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e

-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delay :: a -- ^Initial state 
         -> Signal a -- ^Input signal
         -> Signal a -- ^Output signal

-- | The process constructor 'delayn' delays the signal n events by introducing n identical default values.   
delayn :: a -- ^Initial state
          -> Int -- ^ Delay cycles 
          -> Signal a -- ^Input signal
          -> Signal a -- ^Output signal


-- | The process constructor 'filter' discards the /tokens/ whose values do not fulfill a predicate given by a predicate function.
filter :: (a -> Bool) -- Predicate function
         -> Signal a -- Input signal
         -> Signal a -- Output signal

-- | The process 'zip' \"zips\" two incoming signals into one signal of tuples.
zip  :: (Int,Int) -- ^ prodution rates
     -> Signal a -- ^ first signal
     -> Signal b -- ^ second signal
     -> Signal ([a],[b]) -- ^ output signal of tuples

-- | Works as 'zip', but takes three input signals.
zip3 :: (Int,Int,Int) -> Signal a -> Signal b -> Signal c -> Signal ([a],[b],[c])

-- | Works as 'zip', but takes four input signals.
zip4 :: (Int,Int,Int,Int) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal ([a],[b],[c],[d])

-- | Works as 'zip', but takes four input signals.
zip5 :: (Int,Int,Int,Int,Int) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal ([a],[b],[c],[d],[e])

-- | Works as 'zip', but takes four input signals.
zip6 :: (Int,Int,Int,Int,Int,Int) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal ([a],[b],[c],[d],[e],[f])

-- | The process 'unzip' \"unzips\" a signal of tuples into two signals.
unzip  :: (Int,Int) -- ^ consumption rates 
       -> Signal ([a],[b]) -- ^ signal of tuples
       -> (Signal a, Signal b) -- ^ the two output signals

-- | Works as 'unzip', but has three output signals.
unzip3 :: (Int,Int,Int) -> Signal ([a],[b],[c]) -> (Signal a, Signal b, Signal c)

-- | Works as 'unzip', but has four output signals.
unzip4 :: (Int,Int,Int,Int) -> Signal ([a],[b],[c],[d]) -> (Signal a,Signal b,Signal c,Signal d)

-- | Works as 'unzip', but has four output signals.
unzip5 :: (Int,Int,Int,Int,Int) -> Signal ([a],[b],[c],[d],[e])  -> (Signal a,Signal b,Signal c,Signal d,Signal e)

-- | Works as 'unzip', but has four output signals.
unzip6 :: (Int,Int,Int,Int,Int,Int) -> Signal ([a],[b],[c],[d],[e],[f]) -> (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f)


comb c p f x                    = checkout p $ f §!- (c,x)
comb2 (c1,c2) p f x y           = checkout p $ f §!- (c1,x) -§!- (c2,y)
comb3 (c1,c2,c3) p f x y z      = checkout p $ f §!- (c1,x) -§!- (c2,y) -§!- (c3,z)
comb4 (c1,c2,c3,c4) p f x y z q = checkout p $ f §!- (c1,x) -§!- (c2,y) -§!- (c3,z) -§!- (c4,q)


delay x xs = xs ->- x
delayn x n xs | n <= 0    = xs
              | otherwise = delayn x (n-1) xs ->- x

filter = filt

zip (c1,c2) xs ys                          = fromS $ (,) §!- (c1,xs) -§!- (c2,ys)
zip3 (c1,c2,c3) xs ys zs                   = fromS $ (,,) §!- (c1,xs) -§!- (c2,ys) -§!- (c3,zs)
zip4 (c1,c2,c3,c4) xs ys zs as             = fromS $ (,,,) §!- (c1,xs) -§!- (c2,ys) -§!- (c3,zs) -§!- (c4,as)
zip5 (c1,c2,c3,c4,c5) xs ys zs as bs       = fromS $ (,,,,) §!- (c1,xs) -§!- (c2,ys) -§!- (c3,zs) -§!- (c4,as) -§!- (c5,bs)
zip6 (c1,c2,c3,c4,c5,c6) xs ys zs as bs cs = fromS $ (,,,,,) §!- (c1,xs) -§!- (c2,ys) -§!- (c3,zs) -§!- (c4,as) -§!- (c5,bs) -§!- (c6,cs)


unzip (p1,p2) xs  =            (checkout' p1 $ fst §§- xs, checkout' p2 $ snd §§- xs)
unzip3 (p1,p2,p3) xs =         (checkout' p1 $ (\(x,_,_) -> x) §§- xs,
                                checkout' p2 $ (\(_,x,_) -> x) §§- xs,
                                checkout' p3 $ (\(_,_,x) -> x) §§- xs)
unzip4 (p1,p2,p3,p4) xs =      (checkout' p1 $ (\(x,_,_,_) -> x) §§- xs,
                                checkout' p2 $ (\(_,x,_,_) -> x) §§- xs,
                                checkout' p3 $ (\(_,_,x,_) -> x) §§- xs,
                                checkout' p4 $ (\(_,_,_,x) -> x) §§- xs)
unzip5 (p1,p2,p3,p4,p5) xs =   (checkout' p1 $ (\(x,_,_,_,_) -> x) §§- xs,
                                checkout' p2 $ (\(_,x,_,_,_) -> x) §§- xs,
                                checkout' p3 $ (\(_,_,x,_,_) -> x) §§- xs,
                                checkout' p4 $ (\(_,_,_,x,_) -> x) §§- xs,
                                checkout' p5 $ (\(_,_,_,_,x) -> x) §§- xs)
unzip6 (p1,p2,p3,p4,p5,p6) xs =(checkout' p1 $ (\(x,_,_,_,_,_) -> x) §§- xs,
                                checkout' p2 $ (\(_,x,_,_,_,_) -> x) §§- xs,
                                checkout' p3 $ (\(_,_,x,_,_,_) -> x) §§- xs,
                                checkout' p4 $ (\(_,_,_,x,_,_) -> x) §§- xs,
                                checkout' p5 $ (\(_,_,_,_,x,_) -> x) §§- xs,
                                checkout' p6 $ (\(_,_,_,_,_,x) -> x) §§- xs)


--------------- HELPER FUNCTIONS (not exported) -------------------------

checkout p out = if anyS (\x -> not $ length x == p) out then 
                   error "Function does not produce correct number of tokens" 
                 else tokenize out

checkout' p out = if anyS (\x -> not $ length x == p) $ toS out then 
                    error "Function does not produce correct number of tokens" 
                  else (signal . concat . fromSignal) out

