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

import           ForSyDe.Atom.Utility
import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SY.Core as SY
-- import qualified ForSyDe.Atom.Skeleton.Vector as V (Vector, zipx, unzipx)

------- MoC INTERFACES -------

-- | Wraps explicit timestamps to a (set of) 'ForSyDe.Atom.MoC.SY.SY'
-- signal(s), rendering the equivalent synchronized
-- 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- The following constructors are provisyd:
--
-- > toDE, toDE2, toDE3, toDE4
--
-- >>> let s1 = SY.signal [0,3,4,6,9]
-- >>> let s2 = SY.signal [1,2,3,4,5]
-- >>> toDE s1 s2
-- { 1 @0s, 2 @3s, 3 @4s, 4 @6s, 5 @9s}
--
-- <<docfiles/figs/moc-sy-tode.png>>
toDE2 ::  SY.Signal DE.Tag
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
