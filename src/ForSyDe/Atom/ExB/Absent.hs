{-# OPTIONS_HADDOCK prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.ExB.Absent
-- Copyright   :  (c) George Ungureanu, 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the constructors and assocuated utilities of
-- a type which extends the behavior of a function to express "absent
-- events" (see <ForSyDe-Atom.html#halbwachs91 [Halbwachs91]>).
--
-- The 'AbstExt' type can be used directly with the atom patterns
-- defined in "ForSyDe.Atom.ExB", and no helpers or utilities are
-- needed. Example usage:
--
-- >>> res21 (+) (Prst 1) (Prst 2) 
-- 3
-- >>> res21 (+) Abst     Abst 
-- ⟂
-- >>> filter Abst         (Prst 1)
-- ⟂
-- >>> filter (Prst False) (Prst 1)
-- ⟂
-- >>> filter (Prst True)  (Prst 1)
-- 1
-- >>> filter' False 1 :: AbstExt Int
-- ⟂
-- >>> filter' True  1 :: AbstExt Int
-- 1
-- >>> degen 0 (Prst 1)
-- 1
-- >>> degen 0 Abst
-- 0
-- >>> ignore11 (+) 1 (Prst 1)
-- 2
-- >>> ignore11 (+) 1 Abst
-- 1
--
-- Incorrect usage (not covered by @doctest@):
--
-- > λ> res21 (+) (Prst 1) Abst 
-- > *** Exception: [ExB.Absent] Illegal occurrence of an absent and present event
----------------------------------------------------------------------

module ForSyDe.Atom.ExB.Absent (
  AbstExt(..),

  -- | Module "ForSyDe.Atom.ExB" is re-exported for convenience, to
  -- access the atom patterns more easily.

  module ForSyDe.Atom.ExB
  ) where

import ForSyDe.Atom.ExB
import Prelude hiding (filter)

-- | The 'AbstExt' type extends the base type with the \'\(\bot\)\'
-- symbol denoting the absence of a value/event (see
-- <ForSyDe-Atom.html#halbwachs91 [Halbwachs91]>).
data AbstExt a = Abst   -- ^ \(\bot\) denotes the absence of a value
               | Prst a -- ^ \(\top\) a present event with a value
               deriving (Eq)

-- | Implements the absent semantics of the extended behavior atoms.
instance ExB AbstExt where
  ------------------------
  extend = Prst
  ------------------------
  (/.\) = fmap
  ------------------------
  (/*\) = (<*>)
  ------------------------
  (Prst True) /&\ a = a
  _           /&\ _ = Abst
  ------------------------
  _ /!\ Prst a = a
  a /!\ _      = a 
  ------------------------

-- | Shows 'Abst' as \(\bot\), while a present event is represented
-- with its value.
instance Show a => Show (AbstExt a) where
 showsPrec _ x = showsPrst x
   where showsPrst Abst     = (++) "\10178"       
         showsPrst (Prst x) = (++) (show x)

-- | Reads the \'_\' character to an 'Abst' and a normal value to
-- 'Prst'-wrapped one.
instance Read a => Read (AbstExt a) where
  readsPrec _ x       = readsAbstExt x 
   where
     readsAbstExt s =
       [(Abst, r1)   | ("_", r1) <- lex s] ++
       [(Prst x, r3) | (x, r3) <- reads s]

-- | 'Functor' instance. Bypasses the special values and maps a
-- function to the wrapped value. 
instance Functor AbstExt where
  fmap _ Abst      = Abst
  fmap f (Prst x)  = Prst (f x)

-- | 'Applicative' instance, defines a resolution. Check source code
-- for the lifting rules.
instance Applicative AbstExt where
  pure = Prst 
  (Prst x) <*> (Prst y) = Prst (x y)
  Abst <*> Abst = Abst
  _ <*> _ = error "[ExB.Absent] Illegal occurrence of an absent and present event"
