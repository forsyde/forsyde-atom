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

import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SDF.Core as SDF
import qualified ForSyDe.Atom.MoC.SY.Core as SY
import           ForSyDe.Atom.MoC.TimeStamp
import           ForSyDe.Atom.Utility
-- import qualified ForSyDe.Atom.Skeleton.Vector as V (Vector, zipx, unzipx)

------- MoC INTERFACES -------

-- | Wraps explicit timestamps to a (set of) 'ForSyDe.Atom.MoC.SY.SY'
-- signal(s), rendering the equivalent synchronized
-- 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- The following constructors are provided:
--
-- > toDE, toDE2, toDE3, toDE4
--
-- >>> let s1 = SY.signal [0,3,4,6,9]
-- >>> let s2 = SY.signal [1,2,3,4,5]
-- >>> toDE s1 s2
-- { 1 @0s, 2 @3s, 3 @4s, 4 @6s, 5 @9s}
--
-- <<docfiles/figs/moc-sy-tode.png>>
toDE2 ::  SY.Signal TimeStamp
      -- ^ SY signal carrying 'ForSyDe.Atom.MoC.DE.DE' timestamps
      -> SY.Signal a                -- ^ first input SY signal
      -> SY.Signal b                -- ^ second input SY signal
      -> (DE.Signal a, DE.Signal b)
      -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals

eventToDE (SY.SY t) (SY.SY a) = DE.DE t a
toDE  ts s1          = eventToDE <$> ts <*> s1
toDE2 ts s1 s2       = (toDE ts s1, toDE ts s2)
toDE3 ts s1 s2 s3    = (toDE ts s1, toDE ts s2, toDE ts s3)
toDE4 ts s1 s2 s3 s4 = (toDE ts s1, toDE ts s2, toDE ts s3, toDE ts s4)


-- | Transforms a (set of) 'ForSyDe.Atom.MoC.SY.SY' signal(s) into the
-- equivalent 'ForSyDe.Atom.MoC.SDF.SDF' signal(s). The only change is
-- the event consructor. The total order of SY is interpreted as
-- partial order by the next SDF process downstream.
--
-- The following constructors are provided:
--
-- > toSDF, toSDF2, toSDF3, toSDF4
--
-- >>> let s = SY.signal [1,2,3,4,5]
-- >>> toSDF s
-- {1,2,3,4,5}
--
-- <<docfiles/figs/moc-sy-tosdf.png>>
toSDF2 :: SY.Signal a -> SY.Signal b
       -> (SDF.Signal a, SDF.Signal b)
toSDF  :: SY.Signal a
       -> SDF.Signal a
toSDF3 :: SY.Signal a -> SY.Signal b -> SY.Signal c
       -> (SDF.Signal a, SDF.Signal b, SDF.Signal c)
toSDF4 :: SY.Signal a -> SY.Signal b -> SY.Signal c -> SY.Signal d
       -> (SDF.Signal a, SDF.Signal b, SDF.Signal c, SDF.Signal d)
eventToSDF (SY.SY a) = SDF.SDF a
toSDF  = fmap eventToSDF
toSDF2 s1 s2       = (toSDF s1, toSDF s2)
toSDF3 s1 s2 s3    = (toSDF s1, toSDF s2, toSDF s3)
toSDF4 s1 s2 s3 s4 = (toSDF s1, toSDF s2, toSDF s3, toSDF s4)



-- -- Towards skeleton layer

-- -- | Synchronizes all signals inside a vector into one signal carrying
-- -- vector events. This is simply an instantiation of the skeleton
-- -- 'V.zipx', which passes the SY context wrappers ('SY.wrap22') to
-- -- instantiate a 'ForSyDe.Atom.MoC.comb22' properly.
-- zipx :: V.Vector (SY.Sig a) -> SY.Sig (V.Vector a)
-- zipx = V.zipx SY.wrap21 SY.wrap11


-- -- | Unfolds a signal carrying vector events into a vector of synchronized
-- -- signals. This is simply an instantiation of the skeleton 'V.unzipx',
-- -- which passes the SY context wrappers ('SY.wrap22') to instantiate a
-- -- 'ForSyDe.Atom.MoC.comb22' properly.
-- --
-- -- __/ATTENTION!/__ this  is a temporary unsafe  implementation, since
-- -- it assumes all events are carrying vectors of the same length.
-- unzipx :: SY.Sig (V.Vector a) -> V.Vector (SY.Sig a)
-- unzipx = V.unzipx SY.val SY.wrap11 SY.wrap11
