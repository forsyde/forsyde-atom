-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.CT
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

module ForSyDe.MoC.CT where

import ForSyDe.Core
-- import ForSyDe.Shallow

import Data.Ratio
import Numeric()


data Subsig a = Subsig (Rational,(Rational -> a))


infixl 5 §-
(§-) :: (t -> a) -> Signal (Subsig t) -> Signal (Subsig a)
_ §- NullS                 = NullS
g §- (Subsig (t, f) :- fs) = Subsig (t, g . f) :- g §- fs


infixl 5 -§-
(-§-) :: Signal (Subsig (a -> b)) -> Signal (Subsig a) -> Signal (Subsig b)
NullS -§- _     = NullS
_     -§- NullS = NullS
s1@(Subsig (t1, g) :- gs) -§- s2@(Subsig (t2, f) :- fs) 
    | t1 == t2 = Subsig (t1, \x -> (g x) (f x)) :- (gs -§- fs)
    | t1 <  t2 = Subsig (t1, \x -> (g x) (f x)) :- (gs -§- s2)
    | t1 >  t2 = Subsig (t2, \x -> (g x) (f x)) :- (s1 -§- fs)


plot :: (Num a, Show a) => Rational -> Signal (Subsig a) -> Signal a
plot step = plot' 0.0
  where 
   plot' _    NullS                   = NullS
   plot' prev (Subsig (tag, f) :- ss) = (f <$> (signal [prev, prev + step .. tag - step])) +-+ (plot' tag ss)




