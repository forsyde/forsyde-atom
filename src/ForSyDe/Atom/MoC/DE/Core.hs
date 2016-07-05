{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE.Core where

import ForSyDe.Atom.MoC.Atom
import ForSyDe.Atom.MoC.Signal as S
import ForSyDe.Atom.Behavior

-- | Type alias for a DE signal
type Sig a = S.Signal (DE (Value a))

-- | The DE type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data DE a = DE Int a deriving Eq

-- | Implenents the DE semantics for the MoC atoms
instance MoC DE where
  _ -$- NullS = NullS
  f -$- (x:-xs) = fmap f x :- f -$- xs
  ---------------------
  sf -*- sx = init (DE 0 Undef) sf sx
    where init (DE ptx px) s1@(DE tf f :- fs) s2@(DE tx x :- xs)
            | tf == tx = DE tf (f  x) :- comb (DE tf f) (DE  tx  x) fs xs
            | tf <  tx = DE tf (f px) :- comb (DE tf f) (DE ptx px) fs s2
            | tf >  tx = DE tx (f  Undef) :- init (DE tx x) s1 xs
          comb (DE ptf pf) (DE ptx px) s1@(DE tf f :- fs) s2@(DE tx x :- xs)
            | tf == tx = DE tf ( f  x) :- comb (DE  tf  f) (DE  tx  x) fs xs
            | tf <  tx = DE tf ( f px) :- comb (DE  tf  f) (DE ptx px) fs s2
            | tf >  tx = DE tx (pf  x) :- comb (DE ptf pf) (DE  tx  x) s1 xs
          comb _ (DE ptx px) (DE tf f :- fs) NullS
            = DE tf (f px) :- comb (DE tf f) (DE ptx px) fs NullS
          comb (DE ptf pf) _ NullS (DE tx x :- xs)
            = DE tx (pf x) :- comb (DE ptf pf) (DE tx x) NullS xs
          comb _ _ NullS NullS = NullS
  ---------------------
  (DE _ v) ->- xs = DE 0 v :- xs
  ---------------------
  (DE t _) -&- xs = (\(DE t1 v) -> DE (t1 + t) v) <$> xs


-- | Shows the event with tag @t@ and value @v@ as @(\@t:v)@
instance Show a => Show (DE a) where
  showsPrec _ (DE t x) = (++) ( "(@" ++ show t ++ ":" ++ show x ++ ")")

-- | Reads the string for a normal tuple @(Int,Value a)@ as an event @DE a@
instance (Read a) => Read (DE a) where
  readsPrec _ x     = [(DE t v, x) | ((t,v), x) <- reads x]

-- | Needed to implement the @unzip@ utilities
instance Functor DE where
  fmap f (DE t a) = DE t (f a)

 -----------------------------------------------------------------------------

-- | Wraps a tuple @(tag, value)@ into a DE event of extended values
event       :: (Int, a) -> DE (Value a)
event (t,v) = DE t (Value v)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a DE signal
signal   :: [(Int, a)] -> Sig a
signal l = S.signal $ (\(t,v) -> DE t (Value v)) <$> l

----------------------------------------------------------------------------- 
