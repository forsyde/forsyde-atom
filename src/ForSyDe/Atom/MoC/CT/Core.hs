{-# LANGUAGE TypeFamilies, FlexibleInstances, GADTs, StandaloneDeriving #-}
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

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Utility.Tuple


-- | Type synonym for a base DE signal as a stream of 'DE' events, where the type of
-- tags has not been determined yet. In designs, it is advised to define a type alias
-- for signals, using an appropriate numerical type for tags, e.g.
--
-- > import ForSyDe.Atom.MoC.CT
-- >
-- > type MyCtSignal a = SignalBase Int Float a
type SignalBase timestamp time a = Stream (CT timestamp time a)

-- | Convenience alias for a CT signal, where tags are represented using our
-- 'TimeStamp' alias and time is represented usign our 'Time' alias.
type Signal a = SignalBase TimeStamp Time a

-- | The CT event. It identifies a continuous time signal. It is composed of three
-- parts: a tag, a phase displacement and a continuous sub-signal as a function of
-- time. The types passed to this constructor need to satisfy all of the three
-- properties, as suggested by the type constraints:
--
-- * @time@ needs to be an arbitrary numerical type, member of the 'Fractional' class
--   (see 'MoC' instance). If possible, 'Time', which is an alias for 'Rational' is
--   preferred, as it is the closest numerical representation for continuums.
--
-- * @timestamp@ needs to be a numerical type, to express value 0 (global start) and
--   every representable number needs to have an additive inverse.
--
-- * @timestamp@ needs to be unambiguously comparable (defines a total order).
--
-- * @timestamp@ needs to unambiguously define an equality operation.
--
-- Due to these properties not all numerical types can represent CT timestamps, see
-- 'ForSyDe.Atom.MoC.DE.DE'.
data CT timestamp time a where
  CT :: (Num time, Num timestamp, Ord timestamp, Eq timestamp)
     => { tag   :: timestamp
          -- ^ start time of event
        , phase :: time
          -- ^ phase. Models function delays
        , func  :: time -> a
          -- ^ function of time
        } -> CT timestamp time a

-- | Implenents the execution semantics for the CT MoC atoms.
instance (Num ts, Num tm, Real ts, Fractional tm, Ord ts, Ord tm, Eq ts) =>
         MoC (CT ts tm) where
  ---------------------
  type Fun (CT ts tm) a b = a -> b
  type Ret (CT ts tm) b   = b 
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
    = (\(CT t p v) -> CT (t + d) (p - realToFrac d) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.CT] signal delayed to infinity"
  ---------------------
    
-- | Allows for mapping of functions on a CT event.
instance (Num tm, Num ts, Ord ts, Eq ts) => Functor (CT tm ts) where
  fmap f (CT t p g) = CT t p (f . g)

-- | Allows for lifting functions on a pair of CT events.
instance (Num tm, Num ts, Ord ts, Ord tm, Eq ts) => Applicative (CT tm ts) where
  pure x = CT 0 0 (\_ -> x)
  (CT t p1 f) <*> (CT _ p2 g) = CT t 0 (\x -> (f (x+p1)) (g (x+p2)))

-- | __FOR DEBUG ONLY!__ It evaluates /only/ the signal at
-- the start time.
instance (Show a, Show ts, Real ts, Fractional tm) =>
         Show (CT ts tm a) where
  showsPrec _ e
    = (++) ( show (evalTs (tag e) e) ++
             "@" ++ show (tag e) )

-----------------------------------------------------------------------------
-- These functions are not exported and are used for testing purpose only.

infixl 7 %>
(CT t _ _) %> (CT _ p x) = CT t p x

-- end of testbench functions
-----------------------------------------------------------------------------

evalTm t (CT _ p f) = f (t + p)
evalTs t (CT _ p f) = f ((realToFrac t) + p)
evalEv (CT t p f) = f ((realToFrac t) + p)
                   
unit   :: (Num ts, Num tm, Ord ts)
       => (ts, tm -> a) -> SignalBase ts tm a
-- | Wraps a (tuple of) pair(s) @(time, function)@ into the equivalent
-- unit signal(s), i.e. signal(s) with one event with the period
-- @time@ carrying @function@.
--
-- Helpers: @unit@ and @unit[2-4]@.
unit2  :: (Num ts, Num tm, Ord ts)
       => ((ts, tm -> a1),(ts, tm -> a2))
       -> (SignalBase ts tm a1, SignalBase ts tm a2)
unit3  :: (Num ts, Num tm, Ord ts)
       => ((ts, tm -> a1),(ts, tm -> a2),(ts, tm -> a3))
       -> (SignalBase ts tm a1, SignalBase ts tm a2, SignalBase ts tm a3)
unit4  :: (Num ts, Num tm, Ord ts)
       => ((ts, tm -> a1),(ts, tm -> a2),(ts, tm -> a3),(ts, tm -> a4))
       -> (SignalBase ts tm a1, SignalBase ts tm a2, SignalBase ts tm a3
          ,SignalBase ts tm a4)

unit (t,f) = (CT 0 0 f :- CT t 0 f :- NullS)
unit2 = ($$) (unit,unit)
unit3 = ($$$) (unit,unit,unit)
unit4 = ($$$$) (unit,unit,unit,unit)

-- | Creates an infinite signal.
infinite :: (Num ts, Num tm, Ord ts)
         => (tm -> a) -> SignalBase ts tm a
infinite f = CT 0 0 f :- NullS

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a CT signal
signal :: (Num ts, Num tm, Ord ts)
       => [(ts, tm -> a)] -> SignalBase ts tm a
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

