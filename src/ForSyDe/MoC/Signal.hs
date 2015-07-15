{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Stream' datatype and
-- functions operating on it.
-----------------------------------------------------------------------------

module ForSyDe.MoC.Signal (
  Signal (..),
) where

import ForSyDe.Core

-- | The 'Signal' type class describes the characteristics and behavior of 
--   'ForSyDe.MoC'-bound signals. 
class (Applicative s) => Signal s where
  -- | The data type associated with a filtered output.
  type Filtered s a

  -- | The function 'toS' converts a MoC-bound signal to a stream of events (its base type)
  toS :: s a -> Stream a

  -- | The function 'fromS' converts a (base) stream of events to a MoC-bound signal
  fromS :: Stream a -> s a

  -- | operator for functional application on signals
  (§-) :: (a -> b) -> s a -> s b

  -- | operator for zipping signals
  (-§-) :: s (a -> b) -> s a -> s b

  -- | operator for the default delay function
  (->-) :: s a -> a -> s a 

  -- | operator for filtering signals. The output data type is defined by the 'Filtered' type synonym
  (-#-) :: s a -> (a -> Bool) -> s (Filtered s a) 

  ----------------------------------------
  xs ->- x  = fromS $ x :- toS xs
  (§-)  = (<$>)
  (-§-) = (<*>)

infixl 4 ->-
infixl 4 -#-
infixl 4 §-, -§-




