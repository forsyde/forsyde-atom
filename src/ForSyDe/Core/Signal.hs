{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Core.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Signal' datatype and
-- functions operating on it.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Core.Signal (
       Signals(..)
) where

-- | Test signal
class (Applicative s) => Signals s where
    -- The data type associated with a filtered output.
  type Filtered s a
  -- | The function 'signal' converts a list into a signal.
  signal :: [a] -> s a 

  -- | The function 'fromSignal' converts a signal into a list.
  fromSignal :: s a -> [a]

  applyPattern :: s a -> b -> b -> b

  -- | operator for appending to the head of the list
  (-:-) :: a -> s a -> s a
 
  nullS :: s a

  -- | concatenation operator
  (-++-) :: s a -> s a -> s a 

  -- | operator for functional application on signals
  (§-) :: (a -> b) -> s a -> s b

  -- | operator for zipping signals
  (-§-) :: s (a -> b) -> s a -> s b

  -- | operator for the default delay function
  (-+>) :: s a -> a -> s a 

  -- | filtering operator
  (-#-) :: s a -> (a -> Bool) -> s (Filtered s a) 

  ----------------------------------------
  xs -+> x  = x -:- xs
  (§-)  = (<$>)
  (-§-) = (<*>)

infixr 4 -:-
infixr 5 -++-
infixl 4 -+>
infixl 4 -#-
infixl 4 §-, -§-
