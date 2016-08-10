{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SY.Interface
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.SY.Interface where

import           ForSyDe.Atom.Behavior
import           ForSyDe.Atom.Utility
import           ForSyDe.Atom.MoC
import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SY.Core as SY

eventToDE :: SY.Event DE.Tag -> SY.Event a -> DE.Event a
eventToDE (SY.SY [t]) (SY.SY a) = DE.DE (unsafeFromValue t) a

toDE  ::  SY.Sig DE.Tag -> SY.Sig a                                     -> DE.Sig a
toDE2 ::  SY.Sig DE.Tag -> SY.Sig a -> SY.Sig b                         -> (DE.Sig a, DE.Sig b)
toDE3 ::  SY.Sig DE.Tag -> SY.Sig a -> SY.Sig b -> SY.Sig c             -> (DE.Sig a, DE.Sig b, DE.Sig c)
toDE4 ::  SY.Sig DE.Tag -> SY.Sig a -> SY.Sig b -> SY.Sig c -> SY.Sig d -> (DE.Sig a, DE.Sig b, DE.Sig c, DE.Sig d)

toDE  ts s1          = eventToDE <$> ts <*> s1
toDE2 ts s1 s2       = (toDE ts s1, toDE ts s2)
toDE3 ts s1 s2 s3    = (toDE ts s1, toDE ts s2, toDE ts s3)
toDE4 ts s1 s2 s3 s4 = (toDE ts s1, toDE ts s2, toDE ts s3, toDE ts s4)
