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

module ForSyDe.MoC.SY.Process (
  -- ** Non-strict process constructors
  -- *** Combinatorial process constructors
  comb, comb2, comb3, comb4,
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,
  -- *** Sequential process constructors
  delay, delayn,
  moore, moore2, moore3,
  mealy, mealy2, mealy3,
  -- ***  specific processes
  filter, fill, hold,
  -- ** Strict process constuctors
  -- *** Combinatorial process constructors
  scomb, scomb2, scomb3, scomb4,
  szip, szip3, szip4, szip5, szip6,
  sunzip, sunzip3, sunzip4, sunzip5, sunzip6,
  -- *** Sequential process constructors
  sdelay, sdelayn,
  smoore, smoore2, smoore3,
  smealy, smealy2, smealy3,
) where

import Prelude hiding (zip, zip3, filter, unzip, unzip3)
import ForSyDe.Core
import ForSyDe.MoC.SY.Signal


-- COMB

-- | The `comb` take a combinatorial function as argument and returns a process with one input signals and one output signal.
comb :: (AbstExt a -> AbstExt b) -- ^ combinatorial function
       -> Signal a -- ^ input signal
       -> Signal b -- ^ output signal

-- | Behaves like 'comb', but the process takes 2 input signals.
comb2 :: (AbstExt a -> AbstExt b -> AbstExt c) -> Signal a -> Signal b -> Signal c

-- | Behaves like 'comb', but the process takes 3 input signals.
comb3 :: (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d) -> Signal a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'comb', but the process takes 4 input signals.
comb4 :: (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d -> AbstExt e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e

-- | The `comb` take a combinatorial function as argument and returns a process with one input signals and one output signal.
scomb :: (a -> b) -- ^ combinatorial function
       -> Signal a -- ^ input signal
       -> Signal b -- ^ output signal

-- | Behaves like 'comb', but the process takes 2 input signals.
scomb2 :: (a -> b -> c) -> Signal a -> Signal b -> Signal c

-- | Behaves like 'comb', but the process takes 3 input signals.
scomb3 :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'comb', but the process takes 4 input signals.
scomb4 :: (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e

comb  f x        = f §-  x
comb2 f x y      = f §-  x -§-  y
comb3 f x y z    = f §-  x -§-  y -§-  z
comb4 f x y z q  = f §-  x -§-  y -§-  z -§-  q
scomb  f x       = f §§- x
scomb2 f x y     = f §§- x -§§- y
scomb3 f x y z   = f §§- x -§§- y -§§- z
scomb4 f x y z q = f §§- x -§§- y -§§- z -§§- q


-- DELAY

-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delay :: AbstExt a -- ^Initial state 
        -> Signal a -- ^Input signal
        -> Signal a -- ^Output signal

-- | The process constructor 'delayn' delays the signal n events by introducing n identical default values.   
delayn :: AbstExt a -- ^Initial state
         -> Int -- ^ Delay cycles 
         -> Signal a -- ^Input signal
         -> Signal a -- ^Output signal

-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
sdelay :: a -- ^Initial state 
        -> Signal a -- ^Input signal
        -> Signal a -- ^Output signal

-- | The process constructor 'delayn' delays the signal n events by introducing n identical default values.   
sdelayn :: a -- ^Initial state
         -> Int -- ^ Delay cycles 
         -> Signal a -- ^Input signal
         -> Signal a -- ^Output signal

delay x xs = xs ->- x
delayn x n xs 
  | n <= 0    = xs
  | otherwise = delayn x (n-1) xs ->- x
sdelay x xs = xs ->- pure x
sdelayn x n xs 
  | n <= 0    = xs
  | otherwise = sdelayn x (n-1) xs ->- pure x

-- MOORE

-- | The process constructor 'moore' is used to model state machines of \"Moore\" type, where the output only depends on the current state. The process constructors takes a function to calculate the next state, another function to calculate the output and a value for the initial state. 
--
-- In contrast the output of a process created by the process constructor 'mealy' depends not only on the state, but also on the input values.
moore :: (AbstExt a -> AbstExt b -> AbstExt a) -- ^ Combinational function for next state decoder 
        -> (AbstExt a -> AbstExt c) -- ^ Combinational function for od decoder
        -> AbstExt a -- ^ Initial state
        -> Signal b -- ^ Input signal
        -> Signal c -- ^ Output signal

-- | Behaves like 'moore', but has 2 input signals.
moore2 :: (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt a) -> (AbstExt a -> AbstExt d) -> AbstExt a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'moore', but has 3 input signals.
moore3 :: (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d -> AbstExt a) -> (AbstExt a -> AbstExt e) -> AbstExt a -> Signal b -> Signal c -> Signal d -> Signal e

-- | The process constructor 'moore' is used to model state machines of \"Moore\" type, where the output only depends on the current state. The process constructors takes a function to calculate the next state, another function to calculate the output and a value for the initial state. 
--
-- In contrast the output of a process created by the process constructor 'mealy' depends not only on the state, but also on the input values.
smoore :: (a -> b -> a) -- ^ Combinational function for next state decoder 
        -> (a -> c) -- ^ Combinational function for od decoder
        -> a -- ^ Initial state
        -> Signal b -- ^ Input signal
        -> Signal c -- ^ Output signal

-- | Behaves like 'moore', but has 2 input signals.
smoore2 :: (a -> b -> c -> a) -> (a -> d) -> a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'moore', but has 3 input signals.
smoore3 :: (a -> b -> c -> d -> a) -> (a -> e) -> a -> Signal b -> Signal c -> Signal d -> Signal e

moore ns od mem xs = od §- s
  where s = ns §- s -§- xs ->- mem
moore2 ns od mem xs ys = od §- s
  where s = ns §- s -§- xs -§- ys ->- mem
moore3 ns od mem xs ys zs = od §- s
  where s = ns §- s -§- xs -§- ys -§- zs ->- mem
smoore ns od mem xs = od §§- s
  where s = ns §§- s -§§- xs ->- pure mem
smoore2 ns od mem xs ys = od §§- s
  where s = ns §§- s -§§- xs -§§- ys ->- pure mem
smoore3 ns od mem xs ys zs = od §§- s
  where s = ns §§- s -§§- xs -§§- ys -§§- zs ->- pure mem

-- MEALY

-- | The process constructor 'mealy' is used to model state machines of \"Mealy\" type, where the od only depends on the current state and the input values. The process constructor is based on the process constructor The process constructors takes a function to calculate the next state, another function to calculate the od and a value for the initial state. 
--
-- In contrast the od of a process created by the process constructor 'moore' depends only on the state, but not on the input values.
mealy :: (AbstExt a -> AbstExt b -> AbstExt a) -- ^Combinational function for next state decoder  
        -> (AbstExt a -> AbstExt b -> AbstExt c) -- ^Combinational function for od decoder
        -> AbstExt a -- ^Initial state
        -> Signal b -- ^Input signal 
        -> Signal c -- ^Output signal

-- | Behaves like 'mealy', but has two input signals.
mealy2 :: (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt a) -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d) -> AbstExt a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'mealy', but has three input signals.
mealy3 :: (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d -> AbstExt a) -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d -> AbstExt e) -> AbstExt a -> Signal b -> Signal c -> Signal d -> Signal e

-- | The process constructor 'mealy' is used to model state machines of \"Mealy\" type, where the od only depends on the current state and the input values. The process constructor is based on the process constructor The process constructors takes a function to calculate the next state, another function to calculate the od and a value for the initial state. 
--
-- In contrast the od of a process created by the process constructor 'moore' depends only on the state, but not on the input values.
smealy :: (a -> b -> a) -- ^Combinational function for next state decoder  
        -> (a -> b -> c) -- ^Combinational function for od decoder
        -> a -- ^Initial state
        -> Signal b -- ^Input signal 
        -> Signal c -- ^Output signal

-- | Behaves like 'mealy', but has two input signals.
smealy2 :: (a -> b -> c -> a) -> (a -> b -> c -> d) -> a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'mealy', but has three input signals.
smealy3 :: (a -> b -> c -> d -> a) -> (a -> b -> c -> d -> e) -> a -> Signal b -> Signal c -> Signal d -> Signal e

mealy ns od mem xs = od §- s -§- xs
  where s = ns §- s -§- xs ->- mem
mealy2 ns od mem xs ys = od §- s -§- xs -§- ys
  where s = ns §- s -§- xs -§- ys ->- mem
mealy3 ns od mem xs ys zs = od §- s -§- xs -§- ys -§- zs
  where s = ns §- s -§- xs -§- ys -§- zs ->- mem
smealy ns od mem xs = od §§- s -§§- xs
  where s = ns §§- s -§§- xs ->- pure mem
smealy2 ns od mem xs ys = od §§- s -§§- xs -§§- ys
  where s = ns §§- s -§§- xs -§§- ys ->- pure mem
smealy3 ns od mem xs ys zs = od §§- s -§§- xs -§§- ys -§§- zs
  where s = ns §§- s -§§- xs -§§- ys -§§- zs ->- pure mem

-- FILTER, FILL, HOLD

-- | The process constructor 'filter' discards the values who do not fulfill a predicate given by a predicate function and replaces them with absent events.
filter :: (a -> Bool) -- Predicate function
         -> Signal a -- Input signal
         -> Signal a -- Output signal

-- | The process constructor 'fill' creates a process that 'fills' a signal with present values by replacing absent values with a given value. The output signal is not any more of the type 'AbstExt'.
fill :: a -- ^Default value  
       -> Signal a -- ^Absent extended input signal 
       -> Signal a -- ^Output signal

-- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing absent values by the preceding present value. Only in cases, where no preceding value exists, the absent value is replaced by a default value. The output signal is not any more of the type 'AbstExt'.
hold :: a -- ^Default value 
       -> Signal a -- ^Absent extended input signal 
       -> Signal a -- ^Output signa

filter p xs = xs -#- p
fill   a = fmap (replaceAbst a)
  where replaceAbst a' Abst     = pure a'
        replaceAbst _  (Prst x) = pure x
hold   a xs = s
  where s = holdf §- (s ->- pure a) -§- xs
        holdf (Prst a') Abst     = pure a'
        holdf _         (Prst x) = pure x

-- ZIP

-- | The process 'zip' \"zips\" two incoming signals into one signal of tuples.
zip  :: Signal a -> Signal b -> Signal (AbstExt a,AbstExt b)

-- | Works as 'zip', but takes three input signals.
zip3 :: Signal a -> Signal b -> Signal c -> Signal (AbstExt a,AbstExt b,AbstExt c)

-- | Works as 'zip', but takes four input signals.
zip4 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal (AbstExt a,AbstExt b,AbstExt c,AbstExt d)

-- | Works as 'zip', but takes four input signals.
zip5 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal (AbstExt a,AbstExt b,AbstExt c,AbstExt d,AbstExt e)

-- | Works as 'zip', but takes four input signals.
zip6 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal (AbstExt a,AbstExt b,AbstExt c,AbstExt d,AbstExt e,AbstExt f)

-- | The process 'zip' \"zips\" two incoming signals into one signal of tuples.
szip  :: Signal a -> Signal b -> Signal (a,b)

-- | Works as 'zip', but takes three input signals.
szip3 :: Signal a -> Signal b -> Signal c -> Signal (a,b,c)

-- | Works as 'zip', but takes four input signals.
szip4 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal (a,b,c,d)

-- | Works as 'zip', but takes four input signals.
szip5 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal (a,b,c,d,e)

-- | Works as 'zip', but takes four input signals.
szip6 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal (a,b,c,d,e,f)

zip xs ys              = (\x y -> pure (x,y))                 §- xs -§- ys
zip3 xs ys zs          = (\x y z -> pure (x,y,z))             §- xs -§- ys -§- zs
zip4 xs ys zs as       = (\x y z a -> pure (x,y,z,a))         §- xs -§- ys -§- zs -§- as
zip5 xs ys zs as bs    = (\x y z a b -> pure (x,y,z,a,b))     §- xs -§- ys -§- zs -§- as -§- bs
zip6 xs ys zs as bs cs = (\x y z a b c -> pure (x,y,z,a,b,c)) §- xs -§- ys -§- zs -§- as -§- bs -§- cs
szip xs ys              = (,)     §§!- xs -§§!- ys
szip3 xs ys zs          = (,,)    §§!- xs -§§!- ys -§§!- zs
szip4 xs ys zs as       = (,,,)   §§!- xs -§§!- ys -§§!- zs -§§!- as
szip5 xs ys zs as bs    = (,,,,)  §§!- xs -§§!- ys -§§!- zs -§§!- as -§§!- bs
szip6 xs ys zs as bs cs = (,,,,,) §§!- xs -§§!- ys -§§!- zs -§§!- as -§§!- bs -§§!- cs

--UNZIP

-- | The process 'unzip' \"unzips\" a signal of tuples into two signals.
unzip  :: Signal (AbstExt a,AbstExt b) -> (Signal a,Signal b)

-- | Works as 'unzip', but has three output signals.
unzip3 :: Signal (AbstExt a, AbstExt b, AbstExt c) -> (Signal a, Signal b, Signal c)

-- | Works as 'unzip', but has four output signals.
unzip4 :: Signal (AbstExt a,AbstExt b,AbstExt c,AbstExt d) -> (Signal a,Signal b,Signal c,Signal d)

-- | Works as 'unzip', but has four output signals.
unzip5 :: Signal (AbstExt a,AbstExt b,AbstExt c,AbstExt d,AbstExt e)  -> (Signal a,Signal b,Signal c,Signal d,Signal e)

-- | Works as 'unzip', but has four output signals.
unzip6 :: Signal (AbstExt a,AbstExt b,AbstExt c,AbstExt d,AbstExt e,AbstExt f) -> (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f)

-- | The process 'unzip' \"unzips\" a signal of tuples into two signals.
sunzip  :: Signal (a,b) -> (Signal a,Signal b)

-- | Works as 'unzip', but has three output signals.
sunzip3 :: Signal (a, b, c) -> (Signal a, Signal b, Signal c)

-- | Works as 'unzip', but has four output signals.
sunzip4 :: Signal (a,b,c,d) -> (Signal a,Signal b,Signal c,Signal d)

-- | Works as 'unzip', but has four output signals.
sunzip5 :: Signal (a,b,c,d,e)  -> (Signal a,Signal b,Signal c,Signal d,Signal e)

-- | Works as 'unzip', but has four output signals.
sunzip6 :: Signal (a,b,c,d,e,f) -> (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f)

unzip xs  = (get fst §- xs, get snd §- xs)
unzip3 xs = (get (\(x,_,_) -> x) §- xs,
             get (\(_,x,_) -> x) §- xs,
             get (\(_,_,x) -> x) §- xs)
unzip4 xs = (get (\(x,_,_,_) -> x) §- xs,
             get (\(_,x,_,_) -> x) §- xs,
             get (\(_,_,x,_) -> x) §- xs,
             get (\(_,_,_,x) -> x) §- xs)
unzip5 xs = (get (\(x,_,_,_,_) -> x) §- xs,
             get (\(_,x,_,_,_) -> x) §- xs,
             get (\(_,_,x,_,_) -> x) §- xs,
             get (\(_,_,_,x,_) -> x) §- xs,
             get (\(_,_,_,_,x) -> x) §- xs)
unzip6 xs = (get (\(x,_,_,_,_,_) -> x) §- xs,
             get (\(_,x,_,_,_,_) -> x) §- xs,
             get (\(_,_,x,_,_,_) -> x) §- xs,
             get (\(_,_,_,x,_,_) -> x) §- xs,
             get (\(_,_,_,_,x,_) -> x) §- xs,
             get (\(_,_,_,_,_,x) -> x) §- xs)
sunzip xs  = (fst §§- xs,snd §§- xs)
sunzip3 xs = ((\(x,_,_) -> x) §§- xs,
              (\(_,x,_) -> x) §§- xs,
              (\(_,_,x) -> x) §§- xs)
sunzip4 xs = ((\(x,_,_,_) -> x) §§- xs,
              (\(_,x,_,_) -> x) §§- xs,
              (\(_,_,x,_) -> x) §§- xs,
              (\(_,_,_,x) -> x) §§- xs)
sunzip5 xs = ((\(x,_,_,_,_) -> x) §§- xs,
              (\(_,x,_,_,_) -> x) §§- xs,
              (\(_,_,x,_,_) -> x) §§- xs,
              (\(_,_,_,x,_) -> x) §§- xs,
              (\(_,_,_,_,x) -> x) §§- xs)
sunzip6 xs = ((\(x,_,_,_,_,_) -> x) §§- xs,
              (\(_,x,_,_,_,_) -> x) §§- xs,
              (\(_,_,x,_,_,_) -> x) §§- xs,
              (\(_,_,_,x,_,_) -> x) §§- xs,
              (\(_,_,_,_,x,_) -> x) §§- xs,
              (\(_,_,_,_,_,x) -> x) §§- xs)


-- HELPER FUNCTIONS
get f x = flatten $ f <$> x
  where flatten (Prst (Prst x)) = Prst x
        flatten _               = Abst
 

