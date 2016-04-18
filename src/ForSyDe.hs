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
-- This ForSyDe module is an experimental shallow-embedded DSL implmenenting
-- the ForSyDe methodology. It loads a small part of the core libraries
-- (documented below) in order to provide the necessary functions to the 
-- standard modeling framework.
--
-- Additional features must be included manually by importing the following
-- standard framework-compatible modules:
--
-- * @ForSyDe.MoC.@'ForSyDe.MoC.SY.SY' for using signals or process 
--   constructors in the syncronous MoC library
--
-- * @ForSyDe.MoC.@'ForSyDe.MoC.SDF.SDF' for using signals or process 
--   constructors in the SDF MoC library
--
-- * @ForSyDe.MoC.@'ForSyDe.MoC.Interface.Interface' for using interfaces
--   between MoCs
--
-- * @ForSyDe.@'ForSyDe.Patterns.Patterns' for using process network pattern
--   constructors
--
-- Due to possible name clashes between namespaces it is advised to import the 
-- mentioned modules as /qualified/. An example:
--
-- >    import ForSyDe
-- >    import qualified ForSyDe.MoC.SY as SY
-- >    import qualified ForSyDe.Patterns as PN
-- >
-- >    a = SY.ssignal [1,2,3,4] :: SY.Signal Int
-- >    p1 = SY.comb (+1)
-- >
-- >    v1 = vector [a,a,a,a]
-- >    pn = PN.farm p1 v1
--
-----------------------------------------------------------------------------

module ForSyDe(
  -- * The Signal
  Signal, signal, fromSignal,
  -- * The Vector
  Vector, vector, fromVector,
  -- * The absent-extended type
  AbstExt (..), fromAbstExt, abstExt, isAbsent, isPresent, 
) where

import ForSyDe.Core

