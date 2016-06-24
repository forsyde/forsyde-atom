{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoCLib.Interface
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015;
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable 
-- 
-- This module implements interface processes between MoCs.
-----------------------------------------------------------------------------
module ForSyDe.Atom.MoC.Interface where

-- import ForSyDe.Core
-- import qualified ForSyDe.MoCLib.SY as SY
-- import qualified ForSyDe.MoCLib.DE as DE

-- de2sy :: DE.Sig a -> (SY.Sig Int, SY.Sig a)
-- de2sy s = ((\(DE.DE t a) -> (SY.event t, SY.SY a)) <$> s |<)

-- sy2de :: SY.Sig Int -> SY.Sig a ->  DE.Sig a
-- sy2de t s = (\(Value tag) a -> DE.DE tag a ) <$> s1 |<)



-- -- | MoC interface from SY to SDF
-- sy2sdf :: SY.Signal a -> SDF.Signal a
-- sy2sdf = SDF.fromS . convertTokens . SY.toS
--   where 
--     convertTokens NullS        = NullS
--     convertTokens (Abst  :-xs) = convertTokens xs
--     convertTokens (Prst x:-xs) = x :- convertTokens xs

-- -- | MoC interface from SDF to SY
-- sdf2sy :: SDF.Signal a -> SY.Signal a
-- sdf2sy = SY.fromS . convertTokens . SDF.toS
--   where 
--     convertTokens = (<$>) pure
