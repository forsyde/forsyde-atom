-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SDF
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronuous library defines process constructors, processes and a signal conduit
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process. 
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF (
  -- ** combinatorial process constructors
  comb, comb2, comb3, comb4,
  -- ** sequential process constructors
  delay, delayn,
  -- ** zip\/unzip
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,
  -- ** actors
  actor11, actor12, actor13, actor14,
  actor21, actor22, actor23, actor24,
  actor31, actor32, actor33, actor34,
  actor41, actor42, actor43, actor44,
) where

import Prelude hiding (zip, zip3, filter, unzip, unzip3)
import ForSyDe.Core

import qualified Data.Param.FSVec as V
import Data.TypeLevel.Num hiding ((-),(+),(*),(>),(<),(>=),(<=),(==))

funnyVector :: Nat s => s -> [a] -> V.FSVec s a
funnyVector l xs = V.FSVec xs


infixl 5 §-, -§-
-- | this is the basic functor operator used in process constructors. It takes a function on a list and applies it on a tuple of form ('consumption_rate', 'Signal'). It returns a 'Stream' (the key for applying functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(§-) :: (Nat consumption) => (V.FSVec consumption a -> b) -> Signal a -> Signal b
f §- sig = let x  = (funnyVector c . take (toInt c) . fromSignal) sig
               xs = dropS (toInt c) sig
               c  = V.lengthT x
           in if toInt c == length (V.fromVector x) then f x :- (f §- xs) else NullS

-- | this is the basic lifting operator used in process constructors. It takes a stream of functions on lists and applies it on a tuple of form ('consumption_rate', 'Signal'). It returns a 'Stream' (the key for applying functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(-§-) :: (Nat consumption) => Signal (V.FSVec consumption a -> b) -> Signal a -> Signal b
NullS   -§- _   = NullS
_   -§- NullS   = NullS
(f:-fs) -§- sig = let x  = (funnyVector c . take (toInt c) . fromSignal) sig
                      xs = dropS (toInt c) sig
                      c  = V.lengthT x
                  in if toInt c == length (V.fromVector x) then f x :- (fs -§- xs) else NullS

-- | operator for the default delay function
(->-) :: Signal a -> a -> Signal a
xs ->- i = i :- xs

tok :: (Nat production) => Signal (V.FSVec production a) -> Signal a
tok (x:-xs) = foldr ((+-+) . signal . V.fromVector) (signal $ V.fromVector $ x) xs

----------------------
---- CONSTRUCTORS ----
----------------------

-- | The `comb` take a combinatorial function and production and consumption rates as arguments and returns a process with one input signals and one output signal.
comb :: (Nat c, Nat p) => 
     (V.FSVec c a -> V.FSVec p b) -- ^ combinatorial function on lists 
     -> Signal a -- ^ input signal
     -> Signal b -- ^ output signal

-- | Behaves like 'comb', but the process takes 2 input signals, and the production rate is presented as a tuple.
comb2 :: (Nat c1, Nat c2, Nat p) => 
      (V.FSVec c1 a -> V.FSVec c2 b -> V.FSVec p c)
      -> Signal a -> Signal b -> Signal c

-- | Behaves like 'comb', but the process takes 3 input signals, and the production rate is presented as a 3-tuple.
comb3 :: (Nat c1, Nat c2, Nat c3, Nat p) => 
      (V.FSVec c1 a -> V.FSVec c2 b -> V.FSVec c3 c -> V.FSVec p d)
      -> Signal a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'comb', but the process takes 4 input signals, and the production rate is presented as a 4-tuple.
comb4 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p) => 
      (V.FSVec c1 a -> V.FSVec c2 b -> V.FSVec c3 c -> V.FSVec c4 d -> V.FSVec p e)
      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e

comb  f x       = tok $ f §- x
comb2 f x y     = tok $ f §- x -§- y
comb3 f x y z   = tok $ f §- x -§- y -§- z
comb4 f x y z q = tok $ f §- x -§- y -§- z -§- q


-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delay :: a -- ^Initial state 
         -> Signal a -- ^Input signal
         -> Signal a -- ^Output signal


-- | The process constructor 'delayn' delays the signal n events by introducing n identical default values.   
delayn :: a -- ^Initial state
          -> Int -- ^ Delay cycles 
          -> Signal a -- ^Input signal
          -> Signal a -- ^Output signal

delay x xs = xs ->- x
delayn x n xs | n <= 0    = xs
              | otherwise = delayn x (n-1) xs ->- x


-- | The process 'zip' \"zips\" two incoming signals into one signal of tuples.
zip  :: Signal a -> Signal b -> Signal (a,b)

-- | Works as 'zip', but takes three input signals.
zip3 :: Signal a -> Signal b -> Signal c -> Signal (a,b,c)

-- | Works as 'zip', but takes four input signals.
zip4 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal (a,b,c,d)

-- | Works as 'zip', but takes four input signals.
zip5 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal (a,b,c,d,e)

-- | Works as 'zip', but takes four input signals.
zip6 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal (a,b,c,d,e,f)

zip xs ys              = (,)     <$> xs <*> ys
zip3 xs ys zs          = (,,)    <$> xs <*> ys <*> zs
zip4 xs ys zs as       = (,,,)   <$> xs <*> ys <*> zs <*> as
zip5 xs ys zs as bs    = (,,,,)  <$> xs <*> ys <*> zs <*> as <*> bs
zip6 xs ys zs as bs cs = (,,,,,) <$> xs <*> ys <*> zs <*> as <*> bs <*> cs

--UNZIP

-- | The process 'unzip' \"unzips\" a signal of tuples into two signals.
unzip  :: Signal (a,b) -> (Signal a,Signal b)

-- | Works as 'unzip', but has three output signals.
unzip3 :: Signal (a, b, c) -> (Signal a, Signal b, Signal c)

-- | Works as 'unzip', but has four output signals.
unzip4 :: Signal (a,b,c,d) -> (Signal a,Signal b,Signal c,Signal d)

-- | Works as 'unzip', but has four output signals.
unzip5 :: Signal (a,b,c,d,e)  -> (Signal a,Signal b,Signal c,Signal d,Signal e)

-- | Works as 'unzip', but has four output signals.
unzip6 :: Signal (a,b,c,d,e,f) -> (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f)

unzip xs  = (fst <$> xs,snd <$> xs)
unzip3 xs = ((\(x,_,_) -> x) <$> xs,
              (\(_,x,_) -> x) <$> xs,
              (\(_,_,x) -> x) <$> xs)
unzip4 xs = ((\(x,_,_,_) -> x) <$> xs,
              (\(_,x,_,_) -> x) <$> xs,
              (\(_,_,x,_) -> x) <$> xs,
              (\(_,_,_,x) -> x) <$> xs)
unzip5 xs = ((\(x,_,_,_,_) -> x) <$> xs,
              (\(_,x,_,_,_) -> x) <$> xs,
              (\(_,_,x,_,_) -> x) <$> xs,
              (\(_,_,_,x,_) -> x) <$> xs,
              (\(_,_,_,_,x) -> x) <$> xs)
unzip6 xs = ((\(x,_,_,_,_,_) -> x) <$> xs,
              (\(_,x,_,_,_,_) -> x) <$> xs,
              (\(_,_,x,_,_,_) -> x) <$> xs,
              (\(_,_,_,x,_,_) -> x) <$> xs,
              (\(_,_,_,_,x,_) -> x) <$> xs,
              (\(_,_,_,_,_,x) -> x) <$> xs)

------------------------------------------------------------------------
--
-- SDF ACTORS
--
------------------------------------------------------------------------

-- > Actors with one output

-- | The process constructor 'actor11' constructs an  actor with
-- one input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor11 :: (Nat c, Nat p) => 
     (V.FSVec c i1 -> V.FSVec p o1) -- ^ combinatorial function on lists 
     -> Signal i1 -- ^ input signal
     -> Signal o1 -- ^ output signal

-- | The process constructor 'actor21' constructs an  actor with
-- two input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor21 :: (Nat c1, Nat c2, Nat p) => 
      (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec p o1)
      -> Signal i1 -> Signal i2 -> Signal o1


-- | The process constructor 'actor31' constructs an  actor with
-- three input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor31 :: (Nat c1, Nat c2, Nat c3, Nat p) => 
      (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> V.FSVec p o1)
      -> Signal i1 -> Signal i2 -> Signal i3 -> Signal o1 

-- | The process constructor 'actor41' constructs an  actor with
-- four input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor41 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p) => 
      (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> V.FSVec c4 i4 -> V.FSVec p o1)
      -> Signal i1 -> Signal i2 -> Signal i3 -> Signal i4 -> Signal o1

actor12 :: (Nat c, Nat p1, Nat p2) => 
     (V.FSVec c i1 -> (V.FSVec p1 o1, V.FSVec p2 o2))
     -> Signal i1
     -> (Signal o1, Signal o2) 

actor22 :: (Nat c1, Nat c2, Nat p1, Nat p2) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> (V.FSVec p1 o1, V.FSVec p2 o2))
     -> Signal i1 -> Signal i2  
     -> (Signal o1, Signal o2) 

actor32 :: (Nat c1, Nat c2, Nat c3, Nat p1, Nat p2) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> (V.FSVec p1 o1, V.FSVec p2 o2))  
     -> Signal i1 -> Signal i2 -> Signal i3
     -> (Signal o1, Signal o2)

actor42 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> V.FSVec c4 i4 -> (V.FSVec p1 o1, V.FSVec p2 o2)) 
     -> Signal i1 -> Signal i2 -> Signal i3 -> Signal i4 
     -> (Signal o1, Signal o2)

actor13 :: (Nat c, Nat p1, Nat p2, Nat p3) => 
     (V.FSVec c i1 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3)) 
     -> Signal i1 
     -> (Signal o1, Signal o2, Signal o3) 

actor23 :: (Nat c1, Nat c2, Nat p1, Nat p2, Nat p3) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3)) 
     -> Signal i1 -> Signal i2
     -> (Signal o1, Signal o2, Signal o3)

actor33 :: (Nat c1, Nat c2, Nat c3, Nat p1, Nat p2, Nat p3) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3)) 
     -> Signal i1 -> Signal i2 -> Signal i3
     -> (Signal o1, Signal o2, Signal o3)

actor43 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2, Nat p3) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> V.FSVec c4 i4 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3)) 
     -> Signal i1 -> Signal i2 -> Signal i3 -> Signal i4 
     -> (Signal o1, Signal o2, Signal o3) 

actor14 :: (Nat c, Nat p1, Nat p2, Nat p3, Nat p4) => 
     (V.FSVec c i1 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3, V.FSVec p4 o4))  
     -> Signal i1 -- ^ input signal
     -> (Signal o1, Signal o2, Signal o3, Signal o4)

actor24 :: (Nat c1, Nat c2, Nat p1, Nat p2, Nat p3, Nat p4) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3, V.FSVec p4 o4))  
     -> Signal i1 -> Signal i2
     -> (Signal o1, Signal o2, Signal o3, Signal o4)

actor34 :: (Nat c1, Nat c2, Nat c3, Nat p1, Nat p2, Nat p3, Nat p4) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3, V.FSVec p4 o4)) 
     -> Signal i1 -> Signal i2 -> Signal i3
     -> (Signal o1, Signal o2, Signal o3, Signal o4)

actor44 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2, Nat p3, Nat p4) => 
     (V.FSVec c1 i1 -> V.FSVec c2 i2 -> V.FSVec c3 i3 -> V.FSVec c4 i4 -> (V.FSVec p1 o1, V.FSVec p2 o2, V.FSVec p3 o3, V.FSVec p4 o4))
     -> Signal i1 -> Signal i2 -> Signal i3 -> Signal i4
     -> (Signal o1, Signal o2, Signal o3, Signal o4) 

actor11 = comb 
actor21 = comb2    
actor31 = comb3
actor41 = comb4

actor12 f x       = (tok $ fst <$> f §- x, 
                     tok $ snd <$> f §- x)  
actor22 f x y     = (tok $ fst <$> f §- x -§- y, 
                     tok $ snd <$> f §- x -§- y)  
actor32 f x y z   = (tok $ fst <$> f §- x -§- y -§- z, 
                     tok $ snd <$> f §- x -§- y -§- z)  
actor42 f x y z q = (tok $ fst <$> f §- x -§- y -§- z -§- q, 
                     tok $ snd <$> f §- x -§- y -§- z -§- q)  

actor13 f x       = (tok $ (\(x,_,_) -> x) <$> f §- x, 
                     tok $ (\(_,x,_) -> x) <$> f §- x,
                     tok $ (\(_,_,x) -> x) <$> f §- x)  
actor23 f x y     = (tok $ (\(x,_,_) -> x) <$> f §- x -§- y, 
                     tok $ (\(_,x,_) -> x) <$> f §- x -§- y,
                     tok $ (\(_,_,x) -> x) <$> f §- x -§- y)  
actor33 f x y z   = (tok $ (\(x,_,_) -> x) <$> f §- x -§- y -§- z, 
                     tok $ (\(_,x,_) -> x) <$> f §- x -§- y -§- z,
                     tok $ (\(_,_,x) -> x) <$> f §- x -§- y -§- z)  
actor43 f x y z q = (tok $ (\(x,_,_) -> x) <$> f §- x -§- y -§- z -§- q, 
                     tok $ (\(_,x,_) -> x) <$> f §- x -§- y -§- z -§- q,
                     tok $ (\(_,_,x) -> x) <$> f §- x -§- y -§- z -§- q)  

actor14 f x       = (tok $ (\(x,_,_,_) -> x) <$> f §- x, 
                     tok $ (\(_,x,_,_) -> x) <$> f §- x,
                     tok $ (\(_,_,x,_) -> x) <$> f §- x,
                     tok $ (\(_,_,_,x) -> x) <$> f §- x)  
actor24 f x y     = (tok $ (\(x,_,_,_) -> x) <$> f §- x -§- y, 
                     tok $ (\(_,x,_,_) -> x) <$> f §- x -§- y,
                     tok $ (\(_,_,x,_) -> x) <$> f §- x -§- y,
                     tok $ (\(_,_,_,x) -> x) <$> f §- x -§- y)  
actor34 f x y z   = (tok $ (\(x,_,_,_) -> x) <$> f §- x -§- y -§- z, 
                     tok $ (\(_,x,_,_) -> x) <$> f §- x -§- y -§- z ,
                     tok $ (\(_,_,x,_) -> x) <$> f §- x -§- y -§- z,
                     tok $ (\(_,_,_,x) -> x) <$> f §- x -§- y -§- z)  
actor44 f x y z q = (tok $ (\(x,_,_,_) -> x) <$> f §- x -§- y -§- z -§- q, 
                     tok $ (\(_,x,_,_) -> x) <$> f §- x -§- y -§- z -§- q,
                     tok $ (\(_,_,x,_) -> x) <$> f §- x -§- y -§- z -§- q,
                     tok $ (\(_,_,_,x) -> x) <$> f §- x -§- y -§- z -§- q)  

