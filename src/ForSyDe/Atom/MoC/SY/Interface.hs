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
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.SY.Interface where

import           ForSyDe.Atom.Behavior
import           ForSyDe.Atom.Utility
import           ForSyDe.Atom.MoC
import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SY.Core as SY

eventToDE :: SY.Event DE.Tag -> SY.Event a -> DE.Event a
eventToDE (SY.SY [t]) (SY.SY a) = DE.DE (unsafeFromValue t) a

toDE  ::  SY.Sig DE.Tag -> SY.Sig a -> DE.Sig a
toDE  ts s1          = eventToDE <$> ts <*> s1
toDE2 ts s1 s2       = (toDE ts s1, toDE ts s2)
toDE3 ts s1 s2 s3    = (toDE ts s1, toDE ts s2, toDE ts s3)
toDE4 ts s1 s2 s3 s4 = (toDE ts s1, toDE ts s2, toDE ts s3, toDE ts s4)


     
----------------- DOCUMENTATION -----------------

-- | Translates a (set of) SY signal(s) into synchronized DE ones. A
-- SY signal with the timestamps of the respective events is also
-- necessary in creating DE signals.
--
-- <<includes/figs/sy-tode-graph.png>>
--
-- "ForSyDe.Atom.MoC.DE" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > toDE1, toDE2, toDE3, toDE4,
toDE2 ::  SY.Sig DE.Tag       -- ^ SY signal carrying 'ForSyDe.Atom.MoC.DE.DE' timestamps
      -> SY.Sig a             -- ^ first input SY signal
      -> SY.Sig b             -- ^ second input SY signal
      -> (DE.Sig a, DE.Sig b) -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals
     
--------------- END DOCUMENTATION ---------------
