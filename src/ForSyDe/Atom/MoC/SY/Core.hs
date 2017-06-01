{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SY.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the SY atoms
-- 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.SY.Core where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.Utility

-- | Type synonym for a SY signal, i.e. "an ordered stream of SY
-- events"
type Signal a   = Stream (SY a)

-- | The SY event. It identifies a synchronous signal.
newtype SY a  = SY { val :: a }
  
-- | Implenents the execution and synchronization semantics for the SY
-- MoC through its atoms.
instance MoC SY where
  type Fun SY a b = a -> b
  type Res SY b   = b 
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  (-*-) a = (<*>) (fmap (<*>) a)
  (-*) = id
  ---------------------
  (-<-) (a:-_) = (:-) a
  ---------------------
  (-&-) _ a = a
  ---------------------

-- | Shows the value wrapped
instance Show a => Show (SY a) where
  showsPrec _ (SY x) = (++) (show x)

-- | Reads the value wrapped
instance Read a => Read (SY a) where
  readsPrec _ s = [(SY x, r) | (x, r) <- reads s]

-- | Allows for mapping of functions on a SY event.
instance Functor (SY) where
  fmap f (SY a) = SY (f a)

-- | Allows for lifting functions on a pair of SY events.
instance Applicative (SY) where
  pure = SY 
  (SY a) <*> (SY b) = SY (a b)

-----------------------------------------------------------------------------

unit  :: a -> Signal a
unit  = pure . pure
-- | Wraps a (tuple of) value(s) into the equivalent unit signal(s).
--
-- The following helpers are exported:
--
-- > unit, unit2, unit3, unit4,
unit2 = ($$) (unit, unit)
unit3 = ($$$) (unit, unit, unit)
unit4 = ($$$$) (unit, unit, unit, unit)

-- | Transforms a list of values into a SY signal.
signal   :: [a] -> Signal a
signal l = stream (SY <$> l)

-- | Reads a signal from a string. Like with the @read@ function from
-- @Prelude@, you must specify the tipe of the signal.
--
-- >>> readSignal "{1,2,3,4,5}" :: Signal Int
-- {1,2,3,4,5}
readSignal :: Read a => String -> Signal a
readSignal = read
