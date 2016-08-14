{-# OPTIONS_HADDOCK hide, show-extensions #-}

module ForSyDe.Atom.MoC.DE.Hybrid where

import           ForSyDe.Atom.MoC
import qualified ForSyDe.Atom.MoC.DE.Core      as DE (Sig)
import qualified ForSyDe.Atom.MoC.DE.Interface as DE
import qualified ForSyDe.Atom.MoC.SY.Core      as SY (Sig)
import qualified ForSyDe.Atom.MoC.SY.Interface as SY
import           ForSyDe.Atom.Utility


embedSY11 syproc de1 = let (ts, sy1) = DE.toSY de1
                       in  SY.toDE ts $     syproc sy1 
embedSY12 syproc de1 = let (ts, sy1) = DE.toSY de1
                       in  SY.toDE2 ts <>   syproc sy1
embedSY13 syproc de1 = let (ts, sy1) = DE.toSY de1
                       in  SY.toDE3 ts <>>  syproc sy1
embedSY14 syproc de1 = let (ts, sy1) = DE.toSY de1
                       in  SY.toDE4 ts <>>> syproc sy1

embedSY21 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE ts $     syproc sy1 sy2
embedSY22 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE2 ts <>   syproc sy1 sy2
embedSY23 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE3 ts <>>  syproc sy1 sy2
embedSY24 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE4 ts <>>> syproc sy1 sy2

embedSY31 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE ts $     syproc sy1 sy2 sy3
embedSY32 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE2 ts <>   syproc sy1 sy2 sy3
embedSY33 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE3 ts <>>  syproc sy1 sy2 sy3
embedSY34 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE4 ts <>>> syproc sy1 sy2 sy3

embedSY41 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE ts $     syproc sy1 sy2 sy3 sy4
embedSY42 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE2 ts <>   syproc sy1 sy2 sy3 sy4
embedSY43 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE3 ts <>>  syproc sy1 sy2 sy3 sy4
embedSY44 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE4 ts <>>> syproc sy1 sy2 sy3 sy4
----------------- DOCUMENTATION -----------------

-- | Embeds a 'ForSyDe.Atom.MoC.SY.SY' process inside a DE
-- environment. Internally, it synchronizes the input signals,
-- translates them to SY, feeds them to a SY process and translates
-- the result back to DE using the same input tags.
--
-- <<includes/figs/de-embedsy-graph.png>>
--
-- "ForSyDe.Atom.MoC.DE" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > embedSY11, embedSY12, embedSY13, embedSY14,
-- > embedSY21, embedSY22, embedSY23, embedSY24,
-- > embedSY31, embedSY32, embedSY33, embedSY34,
-- > embedSY41, embedSY42, embedSY43, embedSY44,
embedSY22 :: (SY.Sig a1 -> SY.Sig a2 -> (SY.Sig b1, SY.Sig b2))
             -- ^ 'ForSyDe.Atom.MoC.SY.SY' process
             -> DE.Sig a1 -- ^ first input DE signal
             -> DE.Sig a2 -- ^ second input DE signal 
             -> (DE.Sig b1, DE.Sig b2)
             -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals
      
--------------- END DOCUMENTATION ---------------
