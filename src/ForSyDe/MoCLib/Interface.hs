{-# OPTIONS_HADDOCK show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.Interface
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015;
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable 
-- 
-- This module implements interface processes between MoCs.
-----------------------------------------------------------------------------
module ForSyDe.MoC.Interface where

import ForSyDe.Core
import qualified ForSyDe.MoC.SY.Signal as SY
import qualified ForSyDe.MoC.SDF.Signal as SDF

-- | MoC interface from SY to SDF
sy2sdf :: SY.Signal a -> SDF.Signal a
sy2sdf = SDF.fromS . convertTokens . SY.toS
  where 
    convertTokens NullS        = NullS
    convertTokens (Abst  :-xs) = convertTokens xs
    convertTokens (Prst x:-xs) = x :- convertTokens xs

-- | MoC interface from SDF to SY
sdf2sy :: SDF.Signal a -> SY.Signal a
sdf2sy = SY.fromS . convertTokens . SDF.toS
  where 
    convertTokens = (<$>) pure
