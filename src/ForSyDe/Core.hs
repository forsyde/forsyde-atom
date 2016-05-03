{-# OPTIONS_HADDOCK not-home, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015;
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The core library defines data types used in ForSyDe-Haskell and some 
-- interface functions for using them
-----------------------------------------------------------------------------
module ForSyDe.Core (
       -- * Signal class
       module ForSyDe.Core.Signal,
       -- * Vector type
       module ForSyDe.Core.Vector,
       module ForSyDe.Core.Utilities,
       module ForSyDe.Core.ValueExtensions,
) where

import ForSyDe.Core.Signal
import ForSyDe.Core.Vector
import ForSyDe.Core.Utilities
import ForSyDe.Core.ValueExtensions
