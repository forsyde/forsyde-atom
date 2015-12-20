-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.DE
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

module ForSyDe.MoC.DE where

import ForSyDe.Core

data Subsig a = Subsig (Rational,a) deriving (Show)

infixl 5 §-
(§-) :: (a -> b) -> Signal (Subsig a) -> Signal (Subsig b)
_ §- NullS                 = NullS
f §- (Subsig (t, x) :- xs) = Subsig (t, f x) :- f §- xs


infixl 5 -§-
(-§-) :: Signal (Subsig (a -> b)) -> Signal (Subsig a) -> Signal (Subsig b)
NullS -§- _     = NullS
_     -§- NullS = NullS
s1@(Subsig (t1, f) :- fs) -§- s2@(Subsig (t2, x) :- xs) 
    | t1 == t2 = Subsig (t1, f x) :- (fs -§- xs)
    | t1 <  t2 = Subsig (t1, f x) :- (fs -§- s2)
    | t1 >  t2 = Subsig (t2, f x) :- (s1 -§- xs)

infixl 5 ->-
(->-) :: Signal (Subsig a) -> Subsig a -> Signal (Subsig a)
xs ->- i@(Subsig (t, _)) = i :- (\(Subsig (t1, g)) -> Subsig (t1+t, g)) <$> xs



----------------------
---- CONSTRUCTORS ----
----------------------

-- | The `comb` take a combinatorial function as argument and returns a process with one input signals and one output signal.
comb :: (a -> b) -- ^ combinatorial function
       -> Signal (Subsig a) -- ^ input signal
       -> Signal (Subsig b) -- ^ output signal

-- | Behaves like 'comb', but the process takes 2 input signals.
comb2 :: (a -> b -> c) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c)

-- | Behaves like 'comb', but the process takes 3 input signals.
comb3 :: (a -> b -> c -> d) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d)

-- | Behaves like 'comb', but the process takes 4 input signals.
comb4 :: (a -> b -> c -> d -> e) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d) -> Signal (Subsig e)

comb  f x       = f §- x
comb2 f x y     = f §- x -§- y
comb3 f x y z   = f §- x -§- y -§- z
comb4 f x y z q = f §- x -§- y -§- z -§- q


-- DELAY

-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delay :: (Subsig a) -- ^Initial state
        -> Signal (Subsig a) -- ^Input signal
        -> Signal (Subsig a) -- ^Output signal

delay x xs = xs ->- x

