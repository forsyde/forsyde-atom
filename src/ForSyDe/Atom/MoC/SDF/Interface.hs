{-# LANGUAGE TypeFamilies, PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.SDF.Interface where

import qualified ForSyDe.Atom.MoC as MoC
import qualified ForSyDe.Atom.MoC.SDF.Core as SDF
import qualified ForSyDe.Atom.MoC.SY.Core as SY
-- import qualified ForSyDe.Atom.Skeleton.Vector as V

eventToSY :: SDF.SDF a -> SY.SY a
eventToSY (SDF.SDF a) = SY.SY a

------- MoC INTERFACES -------

-- | Transforms a (set of) 'ForSyDe.Atom.MoC.SDF.SDF' signal(s) into
-- the equivalent 'ForSyDe.Atom.MoC.SY.SY' signal(s). The only change
-- is the event consructor. The partial order of DE is interpreted as
-- SY's total order, based on the positioning of events in the signals
-- (e.g. FIFO buffers) at that moment.
--
-- The following constructors are provided:
--
-- > toSY, toSY2, toSY3, toSY4
--
-- >>> let s = SDF.signal [1,2,3,4,5]
-- >>> toSY s
-- {1,2,3,4,5}
--
-- <<docfiles/figs/moc-sdf-tosy.png>>
toSY2 :: SDF.Signal a -> SDF.Signal b
      -> (SY.Signal a, SY.Signal b)
toSY  :: SDF.Signal a
      -> SY.Signal a
toSY3 :: SDF.Signal a -> SDF.Signal b -> SDF.Signal c
      -> (SY.Signal a, SY.Signal b, SY.Signal c)
toSY4 :: SDF.Signal a -> SDF.Signal b -> SDF.Signal c -> SDF.Signal d
      -> (SY.Signal a, SY.Signal b, SY.Signal c, SY.Signal d)

toSY  = fmap eventToSY
toSY2 s1 s2       = (toSY s1, toSY s2)
toSY3 s1 s2 s3    = (toSY s1, toSY s2, toSY s3)
toSY4 s1 s2 s3 s4 = (toSY s1, toSY s2, toSY s3, toSY s4)


-- -- | Synchronizes all signals inside a vector into one signal carrying
-- -- vector events. 
-- zipx cs = V.reduce2' catEv (SDF.constant1 [V.Null]) cs . V.map11 unitEv 
--   where 
--     catEv c = SDF.comb21 ((c,1),1,\x y->  [(V.concat (V.vector x)) V.<++> (V.concat (V.vector y))])
--     unitEv  = SDF.comb11 (1,1,map V.unit)

-- | Unfolds a signal carrying vector events into a vector of synchronized
-- signals. This is simply an instantiation of the skeleton 'V.unzipx',
-- which passes the SDF context wrappers ('SDF.wrap22') to instantiate a
-- 'ForSyDe.Atom.MoC.comb22' properly.
--
-- __/ATTENTION!/__ this  is a temporary unsafe  implementation, since
-- it assumes all events are carrying vectors of the same length.
-- unzipx :: SDF.Sig (V.Vector a) -> V.Vector (SDF.Sig a)
-- unzipx = V.unzipx SDF.partition SDF.wrap11 SDF.wrap11

