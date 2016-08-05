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
import           ForSyDe.Atom.MoC.AtomLib hiding (comb22, comb33, comb44)
import           ForSyDe.Atom.MoC.DE.Core (Tag)
import           ForSyDe.Atom.MoC.DE.Lib (sync2, sync3, sync4)
import           ForSyDe.Atom.Utility

import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SY.Core as SY
import qualified ForSyDe.Atom.MoC.CT.Core as CT

eventToSY :: DE.Event a -> (SY.Event Tag, SY.Event a)
eventToSY (DE.DE t a) = (SY.event t, SY.SY a)

toSY  :: DE.Sig a ->                                     (SY.Sig Tag, SY.Sig a)
toSY2 :: DE.Sig a -> DE.Sig b ->                         (SY.Sig Tag, SY.Sig a, SY.Sig b)
toSY3 :: DE.Sig a -> DE.Sig b -> DE.Sig c ->             (SY.Sig Tag, SY.Sig a, SY.Sig b, SY.Sig c)
toSY4 :: DE.Sig a -> DE.Sig b -> DE.Sig c -> DE.Sig d -> (SY.Sig Tag, SY.Sig a, SY.Sig b, SY.Sig c, SY.Sig d)

toSY  s1           = (eventToSY <$> s1 |<)
toSY2 s1 s2        = let (de1, de2)           = sync2 s1 s2
                     in  (fst $ toSY de1, snd $ toSY de1, snd $ toSY de2) 
toSY3 s1 s2 s3     = let (de1, de2, de3)      = sync3 s1 s2 s3
                     in  (fst $ toSY de1, snd $ toSY de1, snd $ toSY de2, snd $ toSY de3) 
toSY4 s1 s2 s3 s4  = let (de1, de2, de3, de4) = sync4 s1 s2 s3 s4
                     in  (fst $ toSY de1, snd $ toSY de1, snd $ toSY de2, snd $ toSY de3, snd $ toSY de4) 


eventToCT :: CT.Time -> DE.Event a -> CT.Event a
eventToCT scale (DE.DE t a) = CT.CT (scale * (fromInteger (toInteger t))) (\_->a)

toCT  :: CT.Time -> DE.Sig a ->                                     (CT.Sig a)
toCT2 :: CT.Time -> DE.Sig a -> DE.Sig b ->                         (CT.Sig a, CT.Sig b)
toCT3 :: CT.Time -> DE.Sig a -> DE.Sig b -> DE.Sig c ->             (CT.Sig a, CT.Sig b, CT.Sig c)
toCT4 :: CT.Time -> DE.Sig a -> DE.Sig b -> DE.Sig c -> DE.Sig d -> (CT.Sig a, CT.Sig b, CT.Sig c, CT.Sig d)

toCT  sc s1          = eventToCT sc <$> s1
toCT2 sc s1 s2       = (toCT sc, toCT sc)                   $$   (s1,s2)
toCT3 sc s1 s2 s3    = (toCT sc, toCT sc, toCT sc)          $$$  (s1,s2,s3)
toCT4 sc s1 s2 s3 s4 = (toCT sc, toCT sc, toCT sc, toCT sc) $$$$ (s1,s2,s3,s4)
