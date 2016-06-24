{-# OPTIONS_HADDOCK hide #-}
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

import ForSyDe.Atom.MoC.Atom
import ForSyDe.Atom.MoC.Signal as S
import ForSyDe.Atom.Behavior

-- | Type alias for a SY signal
type Sig a = S.Signal (SY (Value a))

-- | The SY event. It identifies a synchronous signal and implemets an
-- instance of the 'MoC' class. Since SY tags are implicit, this data
-- type only wraps values inside a constructor that identifies it as a
-- "synchronous event".
data SY a  = SY a deriving Eq
-----------------------------------------------------------------------------

-- | Implenents the SY semantics for the MoC atoms.
instance MoC SY where
  (-$-) = (<$>).(<$>)
  ---------------------
  _ -*- NullS = NullS
  NullS -*- _ = NullS
  (f:-fs) -*- (x:-xs) = f <*> x :- fs -*- xs
  ---------------------
  (->-) = (:-)
  ---------------------
  (-&-) _ a = a
  ---------------------

-- | Shows the (extended) value wrapped
instance Show a => Show (SY a) where
  showsPrec _ (SY x) = (++) (show x)

-- | Reads the (extended) value wrapped
instance Read a => Read (SY a) where
  readsPrec _ s       = [(SY x, r) | (x, r) <- reads s]

-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor SY where
  fmap f (SY a) = SY (f a)

-- | Needed for the implementation of the '-*-' atom
instance Applicative SY where
  pure = SY 
  (SY a) <*> (SY b) = SY (a b)

-----------------------------------------------------------------------------

-- | Wraps a base value into a SY event of extended values
event    :: a -> SY (Value a)
event    = SY . Value

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal ((SY . Value) <$> l)

-----------------------------------------------------------------------------
