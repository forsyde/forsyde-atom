{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the CT MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT.Core where

import Data.Ratio
import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Utility

-- | Type alias for the tag type to represent metric time. Underneath
-- we use 'Rational' that is able to represent any /t/ between
-- /t&#8321;/ < /t&#8322;/ &#8712; /T/.
type Time = Rational


-- | Type synonym for a CT signal, i.e. "a signal of CT events"
type Signal a = Stream (CT a)

-- | The CT type, identifying a continuous time event and implementing an
-- instance of the 'MoC' class.
data CT a  = CT { tag   :: TimeStamp
                  -- ^ start time of event
                , phase :: Time
                  -- ^ phase. Models function delays
                , func  :: Time -> a
                  -- ^ function of time
                }

-- | Implenents the execution and synchronization semantics for the CT
-- MoC through its atoms.
instance MoC CT where
  ---------------------
  type Fun CT a b = a -> b
  type Ret CT b   = b 
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (f:-fs) -*- (x:-xs)           =      f  <*> x  :- comb f  x  fs xs
    where
      comb pf px s1@(f :- fs) s2@(x :- xs)
        | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
        | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
        | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
      comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f px fs NullS
      comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x NullS xs
      comb _ _ NullS NullS      = NullS
  ---------------------
  (-*) = id
  ---------------------
  (CT _ p v :- _) -<- xs = (CT 0 p v) :- xs
  ---------------------
  (_ :- CT d _ _ :- _) -&- xs
    = (\(CT t p v) -> CT (t + d) (p - toRational d) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.CT] signal delayed to infinity"
  ---------------------
    
-- | Allows for mapping of functions on a CT event.
instance Functor CT where
  fmap f (CT t p g) = CT t p (f . g)

-- | Allows for lifting functions on a pair of CT events.
instance Applicative CT where
  pure x = CT 0 0 (\_ -> x)
  (CT t p1 f) <*> (CT _ p2 g) = CT t 0 (\x -> (f (x+p1)) (g (x+p2)))

-- | A non-ideal instance meant for debug purpose only. For each event
-- it evaluates the function at the tag time /only/!
instance Show a => Show (CT a) where
  showsPrec _ e
    = (++) ( " "  ++ show (evalTs (tag e) e) ++
             " @" ++ show (tag e) )

-----------------------------------------------------------------------------
-- These functions are not exported and are used for testing purpose only.

infixl 7 %>
(CT t _ _) %> (CT _ p x) = CT t p x

-- end of testbench functions
-----------------------------------------------------------------------------

evalTm t (CT _ p f) = f (t + p)
evalTs t (CT _ p f) = f ((toRational t) + p)
time (CT t _ _) = toRational t
const v = (\_ -> v)

                   
unit  :: (TimeStamp, Time -> a) -> Signal a 
unit (t,f) = (CT 0 0 f :- CT t 0 f :- NullS)

-- | Wraps a (tuple of) pair(s) @(time, function)@ into the equivalent
-- unit signal(s), i.e. signal(s) with one event with the period
-- @time@ carrying @function@.
--
-- The following helpers are exported:
--
-- > unit, unit2, unit3, unit4,
unit2 = ($$) (unit,unit)
unit3 = ($$$) (unit,unit,unit)
unit4 = ($$$$) (unit,unit,unit,unit)

-- | Creates an infinite signal.
infinite :: (Time -> a) -> Signal a
infinite f = CT 0 0 f :- NullS

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a CT signal
signal :: [(TimeStamp, Time -> a)] -> Signal a
signal = checkSignal . stream . fmap (\(t, f) -> CT t 0 f)

-- | Checks if a signal is well-formed or not, according to the CT MoC
-- interpretation in @ForSyDe-Atom@.
checkSignal NullS = NullS
checkSignal s@(x:-_)
  | tag x == 0 = checkOrder s
  | otherwise  = error "[MoC.CT] signal does not tag from global 0"
  where
    checkOrder NullS      = NullS
    checkOrder (x:-NullS) = (x:-NullS)
    checkOrder (x:-y:-xs)
      | tag x < tag y = x :-checkOrder (y:-xs)
      | otherwise = error "[MoC.CT] malformed signal"

-- | Returns a stream with the results of evaluating a signal with a
-- given sampling period.
plot :: TimeStamp  -- ^ sample period
     -> TimeStamp  -- ^ end of plotting
     -> Signal a   -- ^ input CT signal
     -> Stream a   -- ^ stream with evaluation results
plot step until = evalPlot 0
  where
    evalPlot t s@(x:-y:-xs)
      | t >= until = NullS
      | tag y > t  = evalTs t x :- evalPlot (t + step) s
      | otherwise  = evalPlot t (y:-xs)
    evalPlot t s@(x:-NullS)
      | t >= until = NullS
      | otherwise  = evalTs t x :- evalPlot (t + step) s
plot2 s u = ($$)   (plot s u, plot s u)
plot3 s u = ($$$)  (plot s u, plot s u, plot s u)
plot4 s u = ($$$$) (plot s u, plot s u, plot s u, plot s u)

-- | Same as 'plot', but also converts rational outputs to floats
-- (which are easier to be read by drawing engines).
plotFloat :: RealFloat b
          => TimeStamp     -- ^ sample period
          -> TimeStamp     -- ^ end of plotting
          -> Signal Time   -- ^ CT signal with a 'Time' codomain
          -> Stream b      -- ^ plot output in floating point format
plotFloat  s u = fmap  fromRational . plot s u
plotFloat2 s u = ($$)   (plotFloat s u, plotFloat s u)
plotFloat3 s u = ($$$)  (plotFloat s u, plotFloat s u, plotFloat s u)
plotFloat4 s u = ($$$$) (plotFloat s u, plotFloat s u, plotFloat s u, plotFloat s u)


