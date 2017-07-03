{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.DE.Interface where

import           ForSyDe.Atom.MoC.TimeStamp
import           ForSyDe.Atom.MoC.DE.Lib (sync2, sync3, sync4)
-- import qualified ForSyDe.Atom.Skeleton.Vector as V (Vector, zipx, unzipx)
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
-- signal(s) and the equivalent CT signals, provided there is a
-- relation between the timestamps and real time. There is no
-- interpolation or other convertion method involved, the CT events
-- being represented as constant functions during their time span.
--
-- The following constructors are provided:
--
-- > toCT, toCT2, toCT3, toCT4
--
-- TODO: example
--
-- <<docfiles/figs/moc-de-toct.png>>
toCT2 :: DE.Signal a             -- ^ first input DE signal
      -> DE.Signal b             -- ^ second input DE signal
      -> (CT.Signal a, CT.Signal b)
      -- ^ two output 'ForSyDe.Atom.MoC.CT.CT' signals
toCT  :: DE.Signal a
      -> (CT.Signal a)
toCT3 :: DE.Signal a -> DE.Signal b -> DE.Signal c
      -> (CT.Signal a, CT.Signal b, CT.Signal c)
toCT4 :: DE.Signal a -> DE.Signal b -> DE.Signal c -> DE.Signal d
      -> (CT.Signal a, CT.Signal b, CT.Signal c, CT.Signal d)
eventToCT (DE.DE t a) = CT.CT t 0 (\_->a)
toCT  s1          = eventToCT <$> s1
toCT2 s1 s2       = (toCT, toCT) $$ (s1,s2)
toCT3 s1 s2 s3    = (toCT, toCT, toCT) $$$ (s1,s2,s3)
toCT4 s1 s2 s3 s4 = (toCT, toCT, toCT, toCT) $$$$ (s1,s2,s3,s4)


-- -- Towards skeleton layer

-- -- | Synchronizes all signals inside a vector into one signal carrying
-- -- vector events. This is simply an instantiation of the skeleton
-- -- 'V.zipx', which passes the DE context wrappers ('DE.wrap22') to
-- -- instantiate a 'ForSyDe.Atom.MoC.comb22' properly.
-- zipx :: V.Vector (DE.Sig a) -> DE.Sig (V.Vector a)
-- zipx = V.zipx DE.wrap21 DE.wrap11


-- -- | Unfolds a signal carrying vector events into a vector of synchronized
-- -- signals. This is simply an instantiation of the skeleton 'V.unzipx',
-- -- which passes the DE context wrappers ('DE.wrap22') to instantiate a
-- -- 'ForSyDe.Atom.MoC.comb22' properly.
-- --
-- -- __/ATTENTION!/__ this  is a temporary unsafe  implementation, since
-- -- it assumes all events are carrying vectors of the same length.
-- unzipx :: DE.Sig (V.Vector a) -> V.Vector (DE.Sig a)
-- unzipx = V.unzipx DE.val DE.wrap11 DE.wrap11

