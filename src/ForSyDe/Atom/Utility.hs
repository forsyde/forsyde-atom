{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK prune, show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Utility
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This parent module contains sub-modules that concern utility
-- functions. Due to legacy reasons, only the
-- "ForSyDe.Atom.Utility.Tuple" module is re-exported. All other
-- sub-modules need to be imported explicitly.
-----------------------------------------------------------------------------

module ForSyDe.Atom.Utility (
  -- | This module contains utility functions dealing with tuples.
  module ForSyDe.Atom.Utility.Tuple
  ) where

import ForSyDe.Atom.Utility.Tuple
