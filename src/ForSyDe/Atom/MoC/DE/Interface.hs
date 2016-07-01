{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.Interface
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE.Interface where

import           ForSyDe.Atom.Behavior
import           ForSyDe.Atom.MoC.Atom
import           ForSyDe.Atom.MoC.DE.Core (Tag)
import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SY.Core as SY
import           ForSyDe.Atom.Utility

eventToSY :: DE.DE (Value a) -> (SY.SY (Value Tag), SY.SY (Value a))
eventToSY (DE.DE t a) = (SY.event t, SY.SY a)

toSY  :: DE.Sig a ->                                     (SY.Sig Tag, SY.Sig a)
toSY2 :: DE.Sig a -> DE.Sig b ->                         (SY.Sig Tag, SY.Sig a, SY.Sig b)
toSY3 :: DE.Sig a -> DE.Sig b -> DE.Sig c ->             (SY.Sig Tag, SY.Sig a, SY.Sig b, SY.Sig c)
toSY4 :: DE.Sig a -> DE.Sig b -> DE.Sig c -> DE.Sig d -> (SY.Sig Tag, SY.Sig a, SY.Sig b, SY.Sig c, SY.Sig d)

toSY  s1           = (eventToSY <$> s1 |<)
toSY2 s1 s2        = let (tags, sigs)         = (eventToSY <$> (psi21 (,) -$- s1 -*- s2) |<)
                         (sy1, sy2)           = (sigs |||<)
                     in  (tags, sy1, sy2) 
toSY3 s1 s2 s3     = let (tags, sigs)         = (eventToSY <$> (psi31 (,,) -$- s1 -*- s2 -*- s3) |<)
                         (sy1, sy2, sy3)      = (sigs |||<<)
                     in  (tags, sy1, sy2, sy3) 
toSY4 s1 s2 s3 s4  = let (tags, sigs)         = (eventToSY <$> (psi41 (,,,) -$- s1 -*- s2 -*- s3 -*- s4) |<)
                         (sy1, sy2, sy3, sy4) = (sigs |||<<<)
                     in  (tags, sy1, sy2, sy3, sy4) 

