{-# LANGUAGE TypeFamilies, PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.SDF.Interface where

import           ForSyDe.Atom.MoC as MoC
import qualified ForSyDe.Atom.MoC.SDF.Core as SDF
import qualified ForSyDe.Atom.MoC.SY.Core as SY
import qualified ForSyDe.Atom.Skel.Vector as V

------- MoC INTERFACES -------

eventToSY :: SDF.SDF a -> SY.SY a
eventToSY (SDF.SDF a) = SY.SY a

toSY1 :: SDF.Prod -> SDF.Signal a -> SY.Signal (V.Vector a)
-- | Transforms a (set of) 'ForSyDe.Atom.MoC.SDF.SDF' signal(s) into the equivalent
-- 'ForSyDe.Atom.MoC.SY.SY' signal(s). The partial ordering is transformed to total
-- ordering with respect to the firings of the interface process(es), and events
-- consumed during one firing are grouped into vectors.
--
-- Constructors: @toSY[1-4]@.
--
-- >>> let s = SDF.signal [1,2,3,4,5,6,7,8,9]
-- >>> toSY1 2 s
-- {<1,2>,<3,4>,<5,6>,<7,8>}
--
-- <<fig/moc-sdf-tosy.png>>
toSY2 :: (SDF.Cons, SDF.Cons) -- ^ consumption rate of interface process
      -> SDF.Signal a         -- ^ 'SDF.SDF' signal
      -> SDF.Signal b         -- ^ 'SDF.SDF' signal
      -> (SY.Signal (V.Vector a), SY.Signal (V.Vector b))
      -- ^ 'SY.SY' signals
toSY3 :: (SDF.Cons, SDF.Cons, SDF.Cons)
      -> SDF.Signal a -> SDF.Signal b -> SDF.Signal c
      -> (SY.Signal (V.Vector a), SY.Signal (V.Vector b), SY.Signal (V.Vector c))
toSY4 :: (SDF.Cons, SDF.Cons, SDF.Cons, SDF.Cons)
      -> SDF.Signal a -> SDF.Signal b -> SDF.Signal c -> SDF.Signal d
      -> (SY.Signal (V.Vector a), SY.Signal (V.Vector b),
          SY.Signal (V.Vector c), SY.Signal (V.Vector d))

toSY1 c s1 = eventToSY <$> (c, V.vector) -.- s1
toSY2 (c1,c2) s1 s2             = (toSY1 c1 s1, toSY1 c2 s2)
toSY3 (c1,c2,c3) s1 s2 s3       = (toSY1 c1 s1, toSY1 c2 s2, toSY1 c3 s3)
toSY4 (c1,c2,c3,c4) s1 s2 s3 s4 = (toSY1 c1 s1, toSY1 c2 s2, toSY1 c3 s3, toSY1 c4 s4)

toSY1' :: SDF.Signal a -> SY.Signal a
-- | Alternative implementation to 'toSY2', where the consumption rate of the
-- interface process is @1@, meaning that each SDF event has a corresponding SY event.
--
-- Constructors: @toSY[1-4]'@.
--
-- >>> let s = SDF.signal [1,2,3,4,5]
-- >>> toSY1' s
-- {1,2,3,4,5}
--
-- <<fig/moc-sdf-tosyp.png>>
toSY2' :: SDF.Signal a -- ^ 'SDF.SDF' signal
       -> SDF.Signal b -- ^ 'SDF.SDF' signal
       -> (SY.Signal a, SY.Signal b)
       -- ^ 'SY.SY' signals
toSY3' :: SDF.Signal a -> SDF.Signal b -> SDF.Signal c
       -> (SY.Signal a, SY.Signal b, SY.Signal c)
toSY4' :: SDF.Signal a -> SDF.Signal b -> SDF.Signal c -> SDF.Signal d
       -> (SY.Signal a, SY.Signal b, SY.Signal c, SY.Signal d)

toSY1' = fmap eventToSY
toSY2' s1 s2       = (toSY1' s1, toSY1' s2)
toSY3' s1 s2 s3    = (toSY1' s1, toSY1' s2, toSY1' s3)
toSY4' s1 s2 s3 s4 = (toSY1' s1, toSY1' s2, toSY1' s3, toSY1' s4)


------- SKELETON INTERFACES -------

-- | Consumes tokens from a vector of signals and merges them into a
-- signal of vectors, with a production rate of 1. It instantiates the
-- 'ForSyDe.Atom.Skel.Vector.zipx' skeleton.
--
-- >>> let s1 = SDF.signal [1,2,3,4,5]
-- >>> let s2 = SDF.signal [11,12,13,14,15]
-- >>> let v1 = V.vector [s1,s1,s2,s2]
-- >>> let r  = V.vector [2,1,2,1]
-- >>> v1
-- <{1,2,3,4,5},{1,2,3,4,5},{11,12,13,14,15},{11,12,13,14,15}>
-- >>> zipx r v1
-- {<1,2,1,11,12,11>,<3,4,2,13,14,12>}
--
-- <<fig/moc-sdf-zipx.png>>
zipx :: V.Vector SDF.Cons       -- ^ consumption rates
     -> V.Vector (SDF.Signal a) -- ^ vector of signals
     -> SDF.Signal (V.Vector a) -- ^ signal of vectors
zipx rates = V.zipx (V.farm11 transpose rates)
  where transpose r cat = MoC.ctxt21 (r,1) 1 (syncf cat)
        syncf catf a b  = (foldr1 (V.<++>) a `catf`) <$> b


-- (V.fanout (\cat a b -> V.unit a `cat` b))
-- | Consumes the vectors carried by a signal with a rate of 1, and
-- unzips them into a vector of signals based on the user provided
-- rates. It instantiates the 'ForSyDe.Atom.Skel.Vector.unzipx'
-- skeleton.
--
-- __OBS:__ due to the 'ForSyDe.Atom.Skel.Vector.recur' pattern
-- contained by 'ForSyDe.Atom.Skel.Vector.unzipx', the vector of
-- production rates needs to be provided in reverse order (see
-- "ForSyDe.Atom.Skel.Vector").
--
-- >>> let s1 = SDF.signal [1,2,3,4,5]
-- >>> let s2 = SDF.signal [11,12,13,14,15]
-- >>> let v1 = V.vector [s1,s1,s2,s2]
-- >>> let r  = V.vector [2,1,2,1]
-- >>> let sz = zipx r v1
-- >>> v1
-- <{1,2,3,4,5},{1,2,3,4,5},{11,12,13,14,15},{11,12,13,14,15}>
-- >>> sz
-- {<1,2,1,11,12,11>,<3,4,2,13,14,12>}
-- >>> unzipx (V.reverse r) sz
-- <{1,2,3,4},{1,2},{11,12,13,14},{11,12}>
--
-- <<fig/moc-sdf-unzipx.png>>
unzipx :: V.Vector SDF.Prod  -- ^ production rates (in reverse order)
       -> SDF.Signal (V.Vector a) -- ^ signal of vectors
       -> V.Vector (SDF.Signal a) -- ^ vector of signals
unzipx rates = V.unzipx eventify (V.length rates)
  where
    eventify  = V.farm21 vec2res rates . V.gather2 parts
    vec2res r = MoC.wres r . V.fromVector . fmap getJust
    parts     = V.farm21 V.take rates $ V.tail
                $ V.recuri1 V.drop rates V.indexes
    getJust (Just a) = a
    getJust Nothing  = error "[MoC.SDF.unzipx] vectors carried by signal are not large enough"
