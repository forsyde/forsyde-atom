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
  -- | Non-strict process constructor build processes based on functions on 'AbstExt' values.

  -- *** Combinatorial process constructors
  comb, comb2, comb3, comb4,
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,
  -- *** Sequential process constructors
  delay, delayn,
  moore, moore2, moore3,
  mealy, mealy2, mealy3,
  -- ***  specific processes
  filter, fill, hold
) where

import Prelude hiding (zip, zip3, filter, unzip, unzip3)
import ForSyDe.Core
import ForSyDe.MoC.SY.Signal


-- COMB

-- | The `comb` take a combinatorial function as argument and returns a process with one input signals and one output signal.
comb :: (a -> b) -- ^ combinatorial function
       -> Signal a -- ^ input signal
       -> Signal b -- ^ output signal

-- | Behaves like 'comb', but the process takes 2 input signals.
comb2 :: (a -> b -> c) -> Signal a -> Signal b -> Signal c

-- | Behaves like 'comb', but the process takes 3 input signals.
comb3 :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'comb', but the process takes 4 input signals.
comb4 :: (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e

comb  f x       = f §- x
comb2 f x y     = f §- x -§- y
comb3 f x y z   = f §- x -§- y -§- z
comb4 f x y z q = f §- x -§- y -§- z -§- q


-- DELAY

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
delayn x n xs 
  | n <= 0    = xs
  | otherwise = delayn x (n-1) xs ->- x

-- MOORE

-- | The process constructor 'moore' is used to model state machines of \"Moore\" type, where the output only depends on the current state. The process constructors takes a function to calculate the next state, another function to calculate the output and a value for the initial state. 
--
-- In contrast the output of a process created by the process constructor 'mealy' depends not only on the state, but also on the input values.
moore :: (a -> b -> a) -- ^ Combinational function for next state decoder 
        -> (a -> c) -- ^ Combinational function for od decoder
        -> a -- ^ Initial state
        -> Signal b -- ^ Input signal
        -> Signal c -- ^ Output signal

-- | Behaves like 'moore', but has 2 input signals.
moore2 :: (a -> b -> c -> a) -> (a -> d) -> a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'moore', but has 3 input signals.
moore3 :: (a -> b -> c -> d -> a) -> (a -> e) -> a -> Signal b -> Signal c -> Signal d -> Signal e

moore ns od mem xs = od §- s
  where s = ns §- s -§- xs ->- mem
moore2 ns od mem xs ys = od §- s
  where s = ns §- s -§- xs -§- ys ->- mem
moore3 ns od mem xs ys zs = od §- s
  where s = ns §- s -§- xs -§- ys -§- zs ->- mem

-- MEALY

-- | The process constructor 'mealy' is used to model state machines of \"Mealy\" type, where the od only depends on the current state and the input values. The process constructor is based on the process constructor The process constructors takes a function to calculate the next state, another function to calculate the od and a value for the initial state. 
--
-- In contrast the od of a process created by the process constructor 'moore' depends only on the state, but not on the input values.
mealy :: (a -> b -> a) -- ^Combinational function for next state decoder  
        -> (a -> b -> c) -- ^Combinational function for od decoder
        -> a -- ^Initial state
        -> Signal b -- ^Input signal 
        -> Signal c -- ^Output signal

-- | Behaves like 'mealy', but has two input signals.
mealy2 :: (a -> b -> c -> a) -> (a -> b -> c -> d) -> a -> Signal b -> Signal c -> Signal d

-- | Behaves like 'mealy', but has three input signals.
mealy3 :: (a -> b -> c -> d -> a) -> (a -> b -> c -> d -> e) -> a -> Signal b -> Signal c -> Signal d -> Signal e

mealy ns od mem xs = od §- s -§- xs
  where s = ns §- s -§- xs ->- mem
mealy2 ns od mem xs ys = od §- s -§- xs -§- ys
  where s = ns §- s -§- xs -§- ys ->- mem
mealy3 ns od mem xs ys zs = od §- s -§- xs -§- ys -§- zs
  where s = ns §- s -§- xs -§- ys -§- zs ->- mem

-- FILTER, FILL, HOLD

-- | The process constructor 'filter' discards the values who do not fulfill a predicate given by a predicate function and replaces them with absent events.
filter :: (a -> Bool) -- Predicate function
         -> Signal a -- Input signal
         -> Signal (AbstExt a) -- Output signal

-- | The process constructor 'fill' creates a process that 'fills' a signal with present values by replacing absent values with a given value. The output signal is not any more of the type 'AbstExt'.
fill :: a -- ^Default value  
       -> Signal (AbstExt a) -- ^Absent extended input signal 
       -> Signal a -- ^Output signal

-- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing absent values by the preceding present value. Only in cases, where no preceding value exists, the absent value is replaced by a default value. The output signal is not any more of the type 'AbstExt'.
hold :: a -- ^Default value 
       -> Signal (AbstExt a) -- ^Absent extended input signal 
       -> Signal a -- ^Output signa

filter = filt
fill   a = liftS $ fmap (replaceAbst a)
  where replaceAbst a' Abst     = pure a'
        replaceAbst _  (Prst x) = pure x
hold   a xs = s
  where s = tokenize $ holdf §- (s ->- a) -§- xs
        holdf (Prst a') Abst     = pure a'
        holdf _         (Prst x) = pure x

-- ZIP


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

zip xs ys              = (,)     §- xs -§- ys
zip3 xs ys zs          = (,,)    §- xs -§- ys -§- zs
zip4 xs ys zs as       = (,,,)   §- xs -§- ys -§- zs -§- as
zip5 xs ys zs as bs    = (,,,,)  §- xs -§- ys -§- zs -§- as -§- bs
zip6 xs ys zs as bs cs = (,,,,,) §- xs -§- ys -§- zs -§- as -§- bs -§- cs

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

unzip xs  = (fst §- xs,snd §- xs)
unzip3 xs = ((\(x,_,_) -> x) §- xs,
              (\(_,x,_) -> x) §- xs,
              (\(_,_,x) -> x) §- xs)
unzip4 xs = ((\(x,_,_,_) -> x) §- xs,
              (\(_,x,_,_) -> x) §- xs,
              (\(_,_,x,_) -> x) §- xs,
              (\(_,_,_,x) -> x) §- xs)
unzip5 xs = ((\(x,_,_,_,_) -> x) §- xs,
              (\(_,x,_,_,_) -> x) §- xs,
              (\(_,_,x,_,_) -> x) §- xs,
              (\(_,_,_,x,_) -> x) §- xs,
              (\(_,_,_,_,x) -> x) §- xs)
unzip6 xs = ((\(x,_,_,_,_,_) -> x) §- xs,
              (\(_,x,_,_,_,_) -> x) §- xs,
              (\(_,_,x,_,_,_) -> x) §- xs,
              (\(_,_,_,x,_,_) -> x) §- xs,
              (\(_,_,_,_,x,_) -> x) §- xs,
              (\(_,_,_,_,_,x) -> x) §- xs)

