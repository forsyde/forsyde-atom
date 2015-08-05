-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The ForSyDe module is an experimental shallow-embedded DSL implmenenting
-- the ForSyDe methodology. It loads the default libraries:
--
-- * 'ForSyDe.Core'
--
-- * 'ForSyDe.MoC.SY'
--
-- It does not load 'ForSyDe.Patterns', since patterns are not yet part of 
--  the ForSyDe language. In order to model using process network patterns
-- you need to import the module manually.
--
-----------------------------------------------------------------------------

module ForSyDe(
  module ForSyDe.Core,
) where

import ForSyDe.Core

