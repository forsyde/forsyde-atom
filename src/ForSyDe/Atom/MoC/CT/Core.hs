{-# LANGUAGE TypeFamilies #-}
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

import ForSyDe.Atom.MoC.Atom
import ForSyDe.Atom.MoC.Signal as S
import ForSyDe.Atom.Behavior

import Numeric
import Data.Ratio

type Time = Rational

-- | Type alias for a CT signal
type Sig a = S.Signal (CT (Value a))

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data CT a = CT Time (Time -> a) 

-- | Implenents the CT semantics for the MoC atoms
instance MoC CT where
  type Arg CT a = Value a
  ---------------------
  _ -$- NullS = NullS
  f -$- (x:-xs) = fmap f x :- f -$- xs
  ---------------------
  -- (-*-) :: Signal (CT Time (Time -> (Value a -> b))) -> Signal (CT Time (Time -> Value a)) -> Signal (CT Time (Time -> b))
  sf -*- sx = init (CT 0.0 (\_ -> Undef)) sf sx
    where init :: CT (Value a) -> Signal (CT (Value a -> b)) -> Signal (CT (Value a)) -> Signal (CT b)
          init (CT ptx px) s1@(CT tf f :- fs) s2@(CT tx x :- xs)
            | tf == tx = CT tf (\a -> (f a) (x a))  :- comb (CT tf f) (CT  tx  x) fs xs
            | tf <  tx = CT tf (\a -> (f a) (px a)) :- comb (CT tf f) (CT ptx px) fs s2
            | tf >  tx = CT tx (\a -> (f a) Undef)  :- init (CT tx x) s1 xs
          comb (CT ptf pf) (CT ptx px) s1@(CT tf f :- fs) s2@(CT tx x :- xs)
            | tf == tx = CT tf (\a -> (f a) (x a))  :- comb (CT  tf  f) (CT  tx  x) fs xs
            | tf <  tx = CT tf (\a -> (f a) (px a)) :- comb (CT  tf  f) (CT ptx px) fs s2
            | tf >  tx = CT tx (\a -> (pf a) (x a)) :- comb (CT ptf pf) (CT  tx  x) s1 xs
          comb _ (CT ptx px) (CT tf f :- fs) NullS
            = CT tf (\a -> (f a) (px a)) :- comb (CT tf f) (CT ptx px) fs NullS
          comb (CT ptf pf) _ NullS (CT tx x :- xs)
            = CT tx (\a -> (pf a) (x a)) :- comb (CT ptf pf) (CT tx x) NullS xs
          comb _ _ NullS NullS = NullS
  ---------------------
  (CT _ v) ->- xs = CT 0 v :- xs
  ---------------------
  (CT t _) -&- xs = (\(CT t1 v) -> CT (t1 + t) v) <$> xs


-- | Shows the event with tag @t@ and value @v@ as @(\@t:v)@
instance Show a => Show (CT a) where
  showsPrec _ (CT t x) = (++) ( show (x t) ++ "@" ++ (showFFloat Nothing $ fromRat t) "" )


-- | Needed to implement the @unzip@ utilities
instance Functor CT where
  fmap f (CT t a) = CT t (f <$> a)

-----------------------------------------------------------------------------

-- | Wraps a tuple @(tag, value)@ into a DE event of extended values
event       :: (Rational, Rational -> a) -> CT (Value a)
event (t,f) = CT t (Value . f)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a DE signal
signal   :: [(Rational, Rational -> a)] -> Sig a
signal l = S.signal $ event <$> l

partition :: Rational -> Sig a -> Sig a 
partition _    NullS     = NullS
partition sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t < nt    = CT t f :- chunk (CT (t+sample) f) s
          | otherwise = CT nt nf :- chunk (CT nt nf) fs
        chunk _ NullS = NullS

partitionUntil _ _ NullS = NullS
partitionUntil until sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t <= until && t < nt  = CT t f :- chunk (CT (t+sample) f) s
          | t <= until && t >= nt = CT nt nf :- chunk (CT nt nf) fs
          | otherwise = NullS
        chunk (CT t f) NullS
          | t <= until = CT t f :- chunk (CT (t+sample) f) NullS
          | otherwise  = NullS

----------------------------------------------------------------------------- 
