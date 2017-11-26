{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.DE.Interface where

import           ForSyDe.Atom.MoC.DE.Lib (sync2, sync3, sync4)
import           ForSyDe.Atom.MoC.Stream (Stream(..))
import           ForSyDe.Atom.MoC.Time   (Time(..))
import           ForSyDe.Atom.MoC.TimeStamp
import qualified ForSyDe.Atom.Skeleton.Vector as V (
  Vector, zipx, unzipx, fanout, unit, length, vector, reverse)
import           ForSyDe.Atom.Utility

import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SY.Core as SY
import qualified ForSyDe.Atom.MoC.CT.Core as CT

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.DE.Lib

------- MoC INTERFACES -------

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.DE.DE' signal(s) an
-- strips off their explicit tags, outputting the equivalent
-- 'ForSyDe.Atom.MoC.SY.SY' signal(s), tupled with an SY signal
-- carrying the timestamps for the synchronization points.
--
-- The following constructors are provided:
--
-- > toSY, toSY2, toSY3, toSY4
--
-- >>> let s1 = DE.infinite 1
-- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: DE.Signal Int
-- >>> toSY2 s1 s2
-- ({0s,2s,6s,8s,9s},{1,1,1,1,1},{1,2,3,4,5})
--
-- <<docfiles/figs/moc-de-tosy.png>>
toSY2 :: DE.Signal a             -- ^ first input DE signal
      -> DE.Signal b             -- ^ second input DE signal
      -> (SY.Signal TimeStamp, SY.Signal a, SY.Signal b)
      -- ^ signal carrying timestamps tupled with the two output
      -- 'ForSyDe.Atom.MoC.SY.SY' signals
toSY  :: DE.Signal a
      -> (SY.Signal TimeStamp, SY.Signal a)
toSY3 :: DE.Signal a -> DE.Signal b -> DE.Signal c
      -> (SY.Signal TimeStamp, SY.Signal a, SY.Signal b, SY.Signal c)
toSY4 :: DE.Signal a -> DE.Signal b -> DE.Signal c -> DE.Signal d
      -> (SY.Signal TimeStamp, SY.Signal a, SY.Signal b, SY.Signal c, SY.Signal d)

eventToSY (DE.DE t a) = (SY.SY t, SY.SY a)
toSY  s1              = (eventToSY <$> s1 |<)
toSY2 s1 s2
  = let (sy1,sy2) = (toSY,toSY) $$ sync2 s1 s2
    in  (fst,snd,snd) $$$ (sy1,sy1,sy2) 
toSY3 s1 s2 s3
  = let (sy1,sy2,sy3) = (toSY,toSY,toSY) $$$ sync3 s1 s2 s3
    in  (fst,snd,snd,snd) $$$$ (sy1,sy1,sy2,sy3)  
toSY4 s1 s2 s3 s4  
  = let (sy1,sy2,sy3,sy4) = (toSY,toSY,toSY,toSY) $$$$ sync4 s1 s2 s3 s4
    in  (fst,snd,snd,snd,snd) $$$$$ (sy1,sy1,sy2,sy3,sy4) 


-- | Semantic preserving transformation between a (set of) DE
-- signal(s) and the equivalent CT signals. The
-- 'ForSyDe.Atom.MoC.DE.DE' events must carry a function of 'Time'
-- which will be lifted by providing it with 'ForSyDe.Atom.MoC.CT.CT'
-- implicit time semantics.
--
-- The following constructors are provided:
--
-- > toCT, toCT2, toCT3, toCT4
--
-- TODO: below is incorrect
--
-- <<docfiles/figs/moc-de-toct.png>>
toCT2 :: DE.Signal (Time -> a)  -- ^ first input DE signal
      -> DE.Signal (Time -> b)  -- ^ second input DE signal
      -> (CT.Signal a, CT.Signal b)
      -- ^ two output 'ForSyDe.Atom.MoC.CT.CT' signals
eventToCT (DE.DE t a) = CT.CT t 0 a
toCT  s1          = eventToCT <$> s1
toCT2 s1 s2       = (toCT, toCT) $$ (s1,s2)
toCT3 s1 s2 s3    = (toCT, toCT, toCT) $$$ (s1,s2,s3)
toCT4 s1 s2 s3 s4 = (toCT, toCT, toCT, toCT) $$$$ (s1,s2,s3,s4)


-- Towards skeleton layer

-- | Synchronizes all the signals contained by a vector and zips them
-- into one signal of vectors. It instantiates the
-- 'ForSyDe.Atom.Skeleton.Vector.zipx' skeleton.
--
-- >>> let s1 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: DE.Signal Int
-- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@4, 4@8, 5@9}" :: DE.Signal Int
-- >>> let v1 = V.vector [s1,s1,s2,s2]
-- >>> v1
-- <{ 1 @0s, 2 @2s, 3 @6s, 4 @8s, 5 @9s},{ 1 @0s, 2 @2s, 3 @6s, 4 @8s, 5 @9s},{ 1 @0s, 2 @2s, 3 @4s, 4 @8s, 5 @9s},{ 1 @0s, 2 @2s, 3 @4s, 4 @8s, 5 @9s}>
-- >>> zipx v1
-- { <1,1,1,1> @0s, <2,2,2,2> @2s, <2,2,3,3> @4s, <3,3,3,3> @6s, <4,4,4,4> @8s, <5,5,5,5> @9s}
--
-- <<docfiles/figs/moc-de-zipx.png>>
zipx ::V.Vector (DE.Signal a) -> DE.Signal (V.Vector a)
zipx = V.zipx (V.fanout (\cat a b -> a `cat` b))

-- | Unzips the vectors carried by a signal into a vector of
-- signals. It instantiates the 'ForSyDe.Atom.Skeleton.Vector.unzipx'
-- skeleton. To avoid infinite recurrence, the user needs to provide
-- the length of the output vector.
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = DE.signal [(0,v1),(2,v1),(6,v1),(8,v1),(9,v1)]
-- >>> s1
-- { <1,2,3,4> @0s, <1,2,3,4> @2s, <1,2,3,4> @6s, <1,2,3,4> @8s, <1,2,3,4> @9s}
-- >>> unzipx 4 s1
-- <{ 1 @0s, 1 @2s, 1 @6s, 1 @8s, 1 @9s},{ 2 @0s, 2 @2s, 2 @6s, 2 @8s, 2 @9s},{ 3 @0s, 3 @2s, 3 @6s, 3 @8s, 3 @9s},{ 4 @0s, 4 @2s, 4 @6s, 4 @8s, 4 @9s}>
--
-- <<docfiles/figs/moc-de-unzipx.png>>
unzipx :: Integer -> DE.Signal (V.Vector a) -> V.Vector (DE.Signal a)
unzipx n = V.reverse . V.unzipx id n

-- | Same as 'unzipx', but \"sniffs\" the first event to determine the length of the output vector. Might have unsafe behavior!
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = DE.signal [(0,v1),(2,v1),(6,v1),(8,v1),(9,v1)]
-- >>> s1
-- { <1,2,3,4> @0s, <1,2,3,4> @2s, <1,2,3,4> @6s, <1,2,3,4> @8s, <1,2,3,4> @9s}
-- >>> unzipx' s1
-- <{ 1 @0s, 1 @2s, 1 @6s, 1 @8s, 1 @9s},{ 2 @0s, 2 @2s, 2 @6s, 2 @8s, 2 @9s},{ 3 @0s, 3 @2s, 3 @6s, 3 @8s, 3 @9s},{ 4 @0s, 4 @2s, 4 @6s, 4 @8s, 4 @9s}>
unzipx' :: DE.Signal (V.Vector a) -> V.Vector (DE.Signal a)
unzipx' s@(a:-_) = unzipx (V.length $ DE.val a) s

