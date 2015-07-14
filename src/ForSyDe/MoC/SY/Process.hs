{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Process
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY.Process where

import ForSyDe.Core
import ForSyDe.MoC.SY.Signal
import Prelude hiding (filter, zip, zip3, unzipSY, unzip3)

-- | The `combSY` take a combinatorial function as argument and returns a process with one input signals and one output signal.
combSY :: (a -> b) -- ^ combinatorial function
       -> SignalSY a -- ^ input signal
       -> SignalSY b -- ^ output signal

-- | Behaves like 'combSY', but the process takes 2 input signals.
comb2SY :: (a -> b -> c) -> SignalSY a -> SignalSY b -> SignalSY c

-- | Behaves like 'combSY', but the process takes 3 input signals.
comb3SY :: (a -> b -> c -> d) -> SignalSY a -> SignalSY b -> SignalSY c -> SignalSY d

-- | Behaves like 'combSY', but the process takes 4 input signals.
comb4SY :: (a -> b -> c -> d -> e) -> SignalSY a -> SignalSY b -> SignalSY c -> SignalSY d -> SignalSY e

-- | The process 'zipSY' \"zips\" two incoming signals into one signal of tuples.
zipSY  :: SignalSY a -> SignalSY b -> SignalSY (a,b)

-- | Works as 'zipSY', but takes three input signals.
zip3SY :: SignalSY a -> SignalSY b -> SignalSY c -> SignalSY (a,b,c)

-- | Works as 'zipSY', but takes four input signals.
zip4SY :: SignalSY a -> SignalSY b -> SignalSY c -> SignalSY d -> SignalSY (a,b,c,d)

-- | Works as 'zipSY', but takes four input signals.
zip5SY :: SignalSY a -> SignalSY b -> SignalSY c -> SignalSY d -> SignalSY e -> SignalSY (a,b,c,d,e)

-- | Works as 'zipSY', but takes four input signals.
zip6SY :: SignalSY a -> SignalSY b -> SignalSY c -> SignalSY d -> SignalSY e -> SignalSY f -> SignalSY (a,b,c,d,e,f)

-- | The process 'unzipSY' \"unzips\" a signal of tuples into two signals.
unzipSY  :: SignalSY (a,b) -> (SignalSY a,SignalSY b)

-- | Works as 'unzipSY', but has three output signals.
unzip3SY :: SignalSY (a, b, c) -> (SignalSY a, SignalSY b, SignalSY c)

-- | Works as 'unzipSY', but has four output signals.
unzip4SY :: SignalSY (a,b,c,d) -> (SignalSY a,SignalSY b,SignalSY c,SignalSY d)

-- | Works as 'unzipSY', but has four output signals.
unzip5SY :: SignalSY (a,b,c,d,e)  -> (SignalSY a,SignalSY b,SignalSY c,SignalSY d,SignalSY e)

-- | Works as 'unzipSY', but has four output signals.
unzip6SY :: SignalSY (a,b,c,d,e,f) -> (SignalSY a,SignalSY b,SignalSY c,SignalSY d,SignalSY e,SignalSY f)

-- | The process constructor 'delaySY' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delaySY :: a -- ^Initial state 
        -> SignalSY a -- ^Input signal
        -> SignalSY a -- ^Output signal

-- | The process constructor 'delaynSY' delays the signal n events by introducing n identical default values.   
delaynSY :: a -- ^Initial state
         -> Int -- ^ Delay cycles 
         -> SignalSY a -- ^Input signal
         -> SignalSY a -- ^Output signal

-- | The process constructor 'mooreSY' is used to model state machines of \"Moore\" type, where the output only depends on the current state. The process constructors takes a function to calculate the next state, another function to calculate the output and a value for the initial state. 
--
-- In contrast the output of a process created by the process constructor 'mealySY' depends not only on the state, but also on the input values.
mooreSY :: (a -> b -> a) -- ^ Combinational function for next state decoder 
        -> (a -> c) -- ^ Combinational function for od decoder
        -> a -- ^ Initial state
        -> SignalSY b -- ^ Input signal
        -> SignalSY c -- ^ Output signal

-- | Behaves like 'mooreSY', but has 2 input signals.
moore2SY :: (a -> b -> c -> a) -> (a -> d) -> a -> SignalSY b -> SignalSY c -> SignalSY d

-- | Behaves like 'mooreSY', but has 3 input signals.
moore3SY :: (a -> b -> c -> d -> a) -> (a -> e) -> a -> SignalSY b -> SignalSY c -> SignalSY d -> SignalSY e

-- | The process constructor 'mealySY' is used to model state machines of \"Mealy\" type, where the od only depends on the current state and the input values. The process constructor is based on the process constructor The process constructors takes a function to calculate the next state, another function to calculate the od and a value for the initial state. 
--
-- In contrast the od of a process created by the process constructor 'mooreSY' depends only on the state, but not on the input values.
mealySY :: (a -> b -> a) -- ^Combinational function for next state decoder  
        -> (a -> b -> c) -- ^Combinational function for od decoder
        -> a -- ^Initial state
        -> SignalSY b -- ^Input signal 
        -> SignalSY c -- ^Output signal

-- | Behaves like 'mealySY', but has two input signals.
mealy2SY :: (a -> b -> c -> a) -> (a -> b -> c -> d) -> a -> SignalSY b -> SignalSY c -> SignalSY d

-- | Behaves like 'mealySY', but has three input signals.
mealy3SY :: (a -> b -> c -> d -> a) -> (a -> b -> c -> d -> e) -> a -> SignalSY b -> SignalSY c -> SignalSY d -> SignalSY e

-- | The process constructor 'filterSY' discards the values who do not fulfill a predicate given by a predicate function and replaces them with absent events.
filterSY :: (a -> Bool) -- Predicate function
         -> SignalSY a -- Input signal
         -> SignalSY (AbstExt a) -- Output signal

-- | The process constructor 'fillSY' creates a process that 'fills' a signal with present values by replacing absent values with a given value. The output signal is not any more of the type 'AbstExt'.
fillSY :: a -- ^Default value  
       -> SignalSY (AbstExt a) -- ^Absent extended input signal 
       -> SignalSY a -- ^Output signal

-- | The process constructor 'holdSY' creates a process that 'fills' a signal with values by replacing absent values by the preceding present value. Only in cases, where no preceding value exists, the absent value is replaced by a default value. The output signal is not any more of the type 'AbstExt'.
holdSY :: a -- ^Default value 
       -> SignalSY (AbstExt a) -- ^Absent extended input signal 
       -> SignalSY a -- ^Output signal


combSY  f x       = f §- x
comb2SY f x y     = f §- x -§- y
comb3SY f x y z   = f §- x -§- y -§- z
comb4SY f x y z q = f §- x -§- y -§- z -§- q

delaySY x xs = xs ->- x
delaynSY x n xs | n <= 0    = xs
                | otherwise = delaynSY x (n-1) xs ->- x

mooreSY ns od mem xs = od §- s
  where s = ns §- s -§- xs ->- mem
moore2SY ns od mem xs ys = od §- s
  where s = ns §- s -§- xs -§- ys ->- mem
moore3SY ns od mem xs ys zs = od §- s
  where s = ns §- s -§- xs -§- ys -§- zs ->- mem

mealySY ns od mem xs = od §- s -§- xs
  where s = ns §- s -§- xs ->- mem
mealy2SY ns od mem xs ys = od §- s -§- xs -§- ys
  where s = ns §- s -§- xs -§- ys ->- mem
mealy3SY ns od mem xs ys zs = od §- s -§- xs -§- ys -§- zs
  where s = ns §- s -§- xs -§- ys -§- zs ->- mem

filterSY p xs = xs -#- p
fillSY   a xs = fmap (replaceAbst a) xs
  where replaceAbst a' Abst     = a'
        replaceAbst _  (Prst x) = x
holdSY   a xs = s
  where s = hold §- (s ->- a) -§- xs
        hold a' Abst     = a'
        hold _  (Prst x) = x

zipSY xs ys              = (,) §- xs -§- ys
zip3SY xs ys zs          = (,,) §- xs -§- ys -§- zs
zip4SY xs ys zs as       = (,,,) §- xs -§- ys -§- zs -§- as
zip5SY xs ys zs as bs    = (,,,,) §- xs -§- ys -§- zs -§- as -§- bs
zip6SY xs ys zs as bs cs = (,,,,,) §- xs -§- ys -§- zs -§- as -§- bs -§- cs


unzipSY xs  = (fst §- xs,snd §- xs)
unzip3SY xs = ((\(x,_,_) -> x) §- xs,
               (\(_,x,_) -> x) §- xs,
               (\(_,_,x) -> x) §- xs)
unzip4SY xs = ((\(x,_,_,_) -> x) §- xs,
               (\(_,x,_,_) -> x) §- xs,
               (\(_,_,x,_) -> x) §- xs,
               (\(_,_,_,x) -> x) §- xs)
unzip5SY xs = ((\(x,_,_,_,_) -> x) §- xs,
               (\(_,x,_,_,_) -> x) §- xs,
               (\(_,_,x,_,_) -> x) §- xs,
               (\(_,_,_,x,_) -> x) §- xs,
               (\(_,_,_,_,x) -> x) §- xs)
unzip6SY xs = ((\(x,_,_,_,_,_) -> x) §- xs,
               (\(_,x,_,_,_,_) -> x) §- xs,
               (\(_,_,x,_,_,_) -> x) §- xs,
               (\(_,_,_,x,_,_) -> x) §- xs,
               (\(_,_,_,_,x,_) -> x) §- xs,
               (\(_,_,_,_,_,x) -> x) §- xs)

