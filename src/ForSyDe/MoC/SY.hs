{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY (
         -- * Signals
         SignalSY(..),
         -- * Process constructors
         -- ** Combinatorial process constructors
         combSY, comb2SY, comb3SY, comb4SY,
         zipSY, zip3SY, zip4SY, zip5SY, zip6SY,
         unzipSY, unzip3SY, unzip4SY, unzip5SY, unzip6SY,
         -- ** Sequential process constructors
         delaySY, delaynSY,
         mooreSY, moore2SY, moore3SY,
         mealySY, mealy2SY, mealy3SY,
         -- ** SY specific processes
         filterSY, fillSY, holdSY,
       ) where

import ForSyDe.Core
import ForSyDe.MoC.SY.Signal
import ForSyDe.MoC.SY.Process

