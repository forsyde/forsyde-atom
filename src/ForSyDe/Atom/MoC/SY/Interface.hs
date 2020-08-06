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
import           ForSyDe.Atom.MoC ((-.-),(-*))
import           ForSyDe.Atom.MoC.Stream (Stream(..))
import           ForSyDe.Atom.MoC.TimeStamp
import qualified ForSyDe.Atom.Skel.Vector as V (
  Vector, vector, zipx, unzipx, fanout, unit, length, reverse, fromVector)
import           ForSyDe.Atom.Utility.Tuple

------- MoC INTERFACES -------

-- | Wraps explicit timestamps to a (set of) 'ForSyDe.Atom.MoC.SY.SY' signal(s),
-- rendering the equivalent synchronized 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- Constructors: @toDE@, @toDE2@, @toDE3@, @toDE4@.
--
-- >>> let s1 = SY.signal [0,3,4,6,9] :: SY.Signal TimeStamp
-- >>> let s2 = SY.signal [1,2,3,4,5]
-- >>> toDE s1 s2
-- {1@0s,2@3s,3@4s,4@6s,5@9s}
--
-- <<fig/moc-sy-tode.png>>
toDE2 :: (Num t, Ord t, Eq t) => SY.Signal t
      -- ^ SY signal carrying 'ForSyDe.Atom.MoC.DE.DE' timestamps
      -> SY.Signal a                -- ^ first input SY signal
      -> SY.Signal b                -- ^ second input SY signal
      -> (DE.SignalBase t a, DE.SignalBase t b)
      -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals

eventToDE (SY.SY t) (SY.SY a) = DE.DE t a
toDE  ts s1          = eventToDE <$> ts <*> s1
toDE2 ts s1 s2       = (toDE ts s1, toDE ts s2)
toDE3 ts s1 s2 s3    = (toDE ts s1, toDE ts s2, toDE ts s3)
toDE4 ts s1 s2 s3 s4 = (toDE ts s1, toDE ts s2, toDE ts s3, toDE ts s4)


toSDF1 :: SY.Signal a
       -> SDF.Signal a
-- | Transforms a (set of) 'ForSyDe.Atom.MoC.SY.SY' signal(s) into the equivalent
-- 'ForSyDe.Atom.MoC.SDF.SDF' signal(s). The only change is the event consructor,
-- meaning that the order is preserved.
--
-- Constructors: @toSDF[1-4]@.
--
-- >>> let s = SY.signal [1,2,3,4,5]
-- >>> toSDF1 s
-- {1,2,3,4,5}
--
-- <<fig/moc-sy-tosdf.png>>
toSDF2 :: SY.Signal a -- ^ 'SY.SY' signal
       -> SY.Signal b -- ^ 'SY.SY' signal
       -> (SDF.Signal a, SDF.Signal b)
        -- ^ 'SDF.SDF' signals
toSDF3 :: SY.Signal a -> SY.Signal b -> SY.Signal c
       -> (SDF.Signal a, SDF.Signal b, SDF.Signal c)
toSDF4 :: SY.Signal a -> SY.Signal b -> SY.Signal c -> SY.Signal d
       -> (SDF.Signal a, SDF.Signal b, SDF.Signal c, SDF.Signal d)
eventToSDF (SY.SY a) = SDF.SDF a
toSDF1 = fmap eventToSDF
toSDF2 s1 s2       = (toSDF1 s1, toSDF1 s2)
toSDF3 s1 s2 s3    = (toSDF1 s1, toSDF1 s2, toSDF1 s3)
toSDF4 s1 s2 s3 s4 = (toSDF1 s1, toSDF1 s2, toSDF1 s3, toSDF1 s4)

toSDF1' :: SDF.Prod               
        -> SY.Signal (V.Vector a)
        -> SDF.Signal a
-- | Transforms a (set of) 'SY.SY' signal(s) of vectors of the same length into
-- equivanlent 'SDF.SDF' signal(s) by serializing the vectors according to a
-- production rate. If the production rate and the vector lengths do not match then a
-- runtime error is thrown.
--
-- Constructors: @toSDF[1-4]'@.
--
-- >>> let s = read "{<1,2>,<3,4>,<5,6>}" :: SY.Signal (V.Vector Int)
-- >>> toSDF1' 2 s
-- {1,2,3,4,5,6}
--
-- <<fig/moc-sy-tosdfp.png>>
toSDF2' :: (SDF.Prod, SDF.Prod)   -- ^ production rates \(p_1,p_2\)
        -> SY.Signal (V.Vector a) -- ^ 'SY.SY' signal of vectors of length \(p_1\)
        -> SY.Signal (V.Vector b) -- ^ 'SY.SY' signal of vectors of length \(p_2\)
        -> (SDF.Signal a, SDF.Signal b)
         -- ^ 'SDF.SDF' signals where the vectors are serialized.
toSDF3' :: (SDF.Prod, SDF.Prod, SDF.Prod)
        -> SY.Signal (V.Vector a) -> SY.Signal (V.Vector b) -> SY.Signal (V.Vector c)
       -> (SDF.Signal a, SDF.Signal b, SDF.Signal c)
toSDF4' :: (SDF.Prod, SDF.Prod, SDF.Prod, SDF.Prod)
        -> SY.Signal (V.Vector a) -> SY.Signal (V.Vector b)
        -> SY.Signal (V.Vector c) -> SY.Signal (V.Vector d)
       -> (SDF.Signal a, SDF.Signal b, SDF.Signal c, SDF.Signal d)
toSDF1' p1 s1 = (eventToSDF <$> (((,) p1 . V.fromVector) -.- s1) -*)
toSDF2' (p1,p2) s1 s2             = (toSDF1' p1 s1, toSDF1' p2 s2)
toSDF3' (p1,p2,p3) s1 s2 s3       = (toSDF1' p1 s1, toSDF1' p2 s2, toSDF1' p3 s3)
toSDF4' (p1,p2,p3,p4) s1 s2 s3 s4 = (toSDF1' p1 s1, toSDF1' p2 s2, toSDF1' p3 s3,
                                     toSDF1' p4 s4)

-- Towards skeleton layer

-- | Synchronizes all the signals contained by a vector and zips them
-- into one signal of vectors. It instantiates the
-- 'ForSyDe.Atom.Skel.Vector.zipx' skeleton.
--
-- >>> let s1 = SY.signal [1,2,3,4,5]
-- >>> let s2 = SY.signal [11,12,13,14,15]
-- >>> let v1 = V.vector [s1,s1,s2,s2]
-- >>> v1
-- <{1,2,3,4,5},{1,2,3,4,5},{11,12,13,14,15},{11,12,13,14,15}>
-- >>> zipx v1
-- {<1,1,11,11>,<2,2,12,12>,<3,3,13,13>,<4,4,14,14>,<5,5,15,15>}
--
-- <<fig/moc-sy-zipx.png>>
zipx ::V.Vector (SY.Signal a) -> SY.Signal (V.Vector a)
zipx = V.zipx (V.fanout (\cat a b -> a `cat` b))

-- | Unzips the vectors carried by a signal into a vector of
-- signals. It instantiates the 'ForSyDe.Atom.Skel.Vector.unzipx'
-- skeleton. To avoid infinite recurrence, the user needs to provide
-- the length of the output vector.
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = SY.signal [v1,v1,v1,v1,v1]
-- >>> s1
-- {<1,2,3,4>,<1,2,3,4>,<1,2,3,4>,<1,2,3,4>,<1,2,3,4>}
-- >>> unzipx 4 s1
-- <{1,1,1,1,1},{2,2,2,2,2},{3,3,3,3,3},{4,4,4,4,4}>
--
-- <<fig/moc-sy-zipx.png>>
unzipx :: Integer -> SY.Signal (V.Vector a) -> V.Vector (SY.Signal a)
unzipx n = V.reverse . V.unzipx id n

-- | Same as 'unzipx', but \"sniffs\" the first event to determine the
-- length of the output vector. /Has an unsafe behavior!/
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = SY.signal [v1,v1,v1,v1,v1]
-- >>> s1
-- {<1,2,3,4>,<1,2,3,4>,<1,2,3,4>,<1,2,3,4>,<1,2,3,4>}
-- >>> unzipx' s1
-- <{1,1,1,1,1},{2,2,2,2,2},{3,3,3,3,3},{4,4,4,4,4}>
unzipx' :: SY.Signal (V.Vector a) -> V.Vector (SY.Signal a)
unzipx' s@(a:-_) = unzipx (V.length $ SY.val a) s
