{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts, UndecidableInstances #-}
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
import ForSyDe.Atom.Utility (($$), ($$$), ($$$$))

import GHC.Exts



-- | Type alias for a SY signal
type Event a = SY (Value a)
type Sig a   = S.Signal (Event a)


-- | The SY event. It identifies a synchronous signal and implemets an
-- instance of the 'MoC' class. Since SY tags are implicit, this data
-- type only wraps values inside a constructor that identifies it as a
-- "synchronous event".
data SY a  = SY a deriving Eq
-----------------------------------------------------------------------------

instance Partitioned SY where
  type Arg SY c a = SYArg c a
  type Context SY c = ()
  o = (fmap . fmap) unArg

  
-- | Implenents the SY semantics for the MoC atoms.
instance MoC SY where
  ---------------------
  (-$-) = fmap . (>$<)
  ---------------------
  _ -*- NullS = NullS
  NullS -*- _ = NullS
  (f:-fs) -*- (x:-xs) = f >*< x :- fs -*- xs
  ---------------------
  (->-) = (:-) . (fmap unArg)
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
instance Functor (SY) where
  fmap f (SY a) = SY (f a)

-- | Needed for the implementation of the '-*-' atom
instance Applicative (SY) where
  pure = SY 
  (SY a) <*> (SY b) = SY (a b)

-----------------------------------------------------------------------------

newtype SYArg c a = SYArg {unArg :: Value a }

instance Functor (SYArg c) where
  fmap f (SYArg a) = SYArg (fmap f a)

instance Applicative (SYArg c) where
  pure = SYArg . Value
  (SYArg f) <*> (SYArg a) = SYArg (f <*> a)

infixl 4 >$<, >*<
(>$<) = fmap . (\f a -> f $ SYArg a)
fs >*< gs = fs <*> (fmap SYArg gs)

-----------------------------------------------------------------------------

-- | Wraps a base value into a SY argument of extended values
argument  :: a -> SY (SYArg c a)
argument  = SY . pure
argument2 = ($$) (argument, argument)
argument3 = ($$$) (argument, argument, argument)
argument4 = ($$$$) (argument, argument, argument, argument)

event  :: a -> Event a
event  = SY . pure

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal ((SY . Value) <$> l)

-----------------------------------------------------------------------------
