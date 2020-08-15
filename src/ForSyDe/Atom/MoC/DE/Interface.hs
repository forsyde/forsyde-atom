{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.DE.Interface where

import           ForSyDe.Atom.MoC.DE.Lib (sync2, sync3, sync4)
import           ForSyDe.Atom.MoC.Stream (Stream(..))
import qualified ForSyDe.Atom.Skel.Vector as V (
  Vector, vector, zipx, unzipx, fanout, length, reverse)
import           ForSyDe.Atom.Utility.Tuple

import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.DE.Lib as DE (comb11)
import qualified ForSyDe.Atom.MoC.SY.Core as SY
import qualified ForSyDe.Atom.MoC.CT.Core as CT

import Prelude hiding ((<>))

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.DE.Lib

------- MoC INTERFACES -------

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.DE.DE' signal(s) an
-- strips off their explicit tags, outputting the equivalent
-- 'ForSyDe.Atom.MoC.SY.SY' signal(s), tupled with an SY signal
-- carrying the timestamps for the synchronization points.
--
-- Constructors: @toSY[1-4]@
--
-- >>> let s1 = DE.infinite 1
-- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: DE.Signal Int
-- >>> toSY2 s1 s2
-- ({0s,2s,6s,8s,9s},{1,1,1,1,1},{1,2,3,4,5})
--
-- <<fig/moc-de-tosy.png>>
toSY2 :: (Num t, Ord t, Eq t)
      => DE.SignalBase t a             -- ^ first input DE signal
      -> DE.SignalBase t b             -- ^ second input DE signal
      -> (SY.Signal t, SY.Signal a, SY.Signal b)
      -- ^ signal carrying timestamps tupled with the two output
      -- 'ForSyDe.Atom.MoC.SY.SY' signals
toSY1 :: (Num t, Ord t, Eq t)
      => DE.SignalBase t a
      -> (SY.Signal t, SY.Signal a)
toSY3 :: (Num t, Ord t, Eq t)
      => DE.SignalBase t a -> DE.SignalBase t b -> DE.SignalBase t c
      -> (SY.Signal t, SY.Signal a, SY.Signal b, SY.Signal c)
toSY4 :: (Num t, Ord t, Eq t)
      => DE.SignalBase t a -> DE.SignalBase t b -> DE.SignalBase t c
      -> DE.SignalBase t d
      -> (SY.Signal t, SY.Signal a, SY.Signal b, SY.Signal c, SY.Signal d)

eventToSY (DE.DE t a) = (SY.SY t, SY.SY a)
toSY1 s1              = (eventToSY <$> s1 |<)
toSY2 s1 s2
  = let (sy1,sy2) = (toSY1,toSY1) $$ sync2 s1 s2
    in  (fst,snd,snd) $$$ (sy1,sy1,sy2) 
toSY3 s1 s2 s3
  = let (sy1,sy2,sy3) = (toSY1,toSY1,toSY1) $$$ sync3 s1 s2 s3
    in  (fst,snd,snd,snd) $$$$ (sy1,sy1,sy2,sy3)  
toSY4 s1 s2 s3 s4  
  = let (sy1,sy2,sy3,sy4) = (toSY1,toSY1,toSY1,toSY1) $$$$ sync4 s1 s2 s3 s4
    in  (fst,snd,snd,snd,snd) $$$$$ (sy1,sy1,sy2,sy3,sy4) 


-- | Semantic preserving transformation between a (set of) DE
-- signal(s) and the equivalent CT signals. The
-- 'ForSyDe.Atom.MoC.DE.DE' events must carry a function of 'Time'
-- which will be lifted by providing it with 'ForSyDe.Atom.MoC.CT.CT'
-- implicit time semantics.
--
-- Constructors: @toCT[1-4]@.
--
-- <<fig/moc-de-toct.png>>
toCT2 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => DE.SignalBase ts (tm -> a)  -- ^ first input DE signal
      -> DE.SignalBase ts (tm -> b)  -- ^ second input DE signal
      -> (CT.SignalBase ts tm a, CT.SignalBase ts tm b)
      -- ^ two output 'ForSyDe.Atom.MoC.CT.CT' signals
eventToCT (DE.DE t a) = CT.CT t 0 a
toCT1 s1          = eventToCT <$> s1
toCT2 s1 s2       = (toCT1, toCT1) $$ (s1,s2)
toCT3 s1 s2 s3    = (toCT1, toCT1, toCT1) $$$ (s1,s2,s3)
toCT4 s1 s2 s3 s4 = (toCT1, toCT1, toCT1, toCT1) $$$$ (s1,s2,s3,s4)

hold1 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => DE.SignalBase ts a -> CT.SignalBase ts tm a
-- | Translates a DE signal into a CT signal of constant sub-signals.
hold2 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => DE.SignalBase ts a -> DE.SignalBase ts b
      -> (CT.SignalBase ts tm a, CT.SignalBase ts tm b)
hold3 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => DE.SignalBase ts a -> DE.SignalBase ts b -> DE.SignalBase ts c
      -> (CT.SignalBase ts tm a, CT.SignalBase ts tm b, CT.SignalBase ts tm c)
hold4 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
      => DE.SignalBase ts a -> DE.SignalBase ts b -> DE.SignalBase ts c
      -> DE.SignalBase ts d
      -> (CT.SignalBase ts tm a, CT.SignalBase ts tm b, CT.SignalBase ts tm c, CT.SignalBase ts tm d)
hold1 = toCT1 . DE.comb11 (\a _ -> a)
hold2 s1 s2       = (hold1, hold1) $$ (s1,s2)
hold3 s1 s2 s3    = (hold1, hold1, hold1) $$$ (s1,s2,s3)
hold4 s1 s2 s3 s4 = (hold1, hold1, hold1, hold1) $$$$ (s1,s2,s3,s4)


-- Towards skeleton layer

-- | Synchronizes all the signals contained by a vector and zips them
-- into one signal of vectors. It instantiates the
-- 'ForSyDe.Atom.Skel.Vector.zipx' skeleton.
--
-- >>> let s1 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: DE.Signal Int
-- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@4, 4@8, 5@9}" :: DE.Signal Int
-- >>> let v1 = V.vector [s1,s1,s2,s2]
-- >>> v1
-- <{1@0s,2@2s,3@6s,4@8s,5@9s},{1@0s,2@2s,3@6s,4@8s,5@9s},{1@0s,2@2s,3@4s,4@8s,5@9s},{1@0s,2@2s,3@4s,4@8s,5@9s}>
-- >>> zipx v1
-- {<1,1,1,1>@0s,<2,2,2,2>@2s,<2,2,3,3>@4s,<3,3,3,3>@6s,<4,4,4,4>@8s,<5,5,5,5>@9s}
--
-- <<fig/moc-de-zipx.png>>
zipx ::(Num t, Ord t, Eq t)
     => V.Vector (DE.SignalBase t a) -> DE.SignalBase t (V.Vector a)
zipx = V.zipx (V.fanout (\cat a b -> a `cat` b))

-- | Unzips the vectors carried by a signal into a vector of
-- signals. It instantiates the 'ForSyDe.Atom.Skel.Vector.unzipx'
-- skeleton. To avoid infinite recurrence, the user needs to provide
-- the length of the output vector.
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = DE.signal [(0,v1),(2,v1),(6,v1),(8,v1),(9,v1)] :: DE.Signal (V.Vector Int)
-- >>> s1
-- {<1,2,3,4>@0s,<1,2,3,4>@2s,<1,2,3,4>@6s,<1,2,3,4>@8s,<1,2,3,4>@9s}
-- >>> unzipx 4 s1
-- <{1@0s,1@2s,1@6s,1@8s,1@9s},{2@0s,2@2s,2@6s,2@8s,2@9s},{3@0s,3@2s,3@6s,3@8s,3@9s},{4@0s,4@2s,4@6s,4@8s,4@9s}>
--
-- <<fig/moc-de-unzipx.png>>
unzipx :: (Num t, Ord t, Eq t)
       => Integer -> DE.SignalBase t (V.Vector a) -> V.Vector (DE.SignalBase t a)
unzipx n = V.reverse . V.unzipx id n

-- | Same as 'unzipx', but \"sniffs\" the first event to determine the length of the output vector. Might have unsafe behavior!
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = DE.signal [(0,v1),(2,v1),(6,v1),(8,v1),(9,v1)] :: DE.Signal (V.Vector Int)
-- >>> s1
-- {<1,2,3,4>@0s,<1,2,3,4>@2s,<1,2,3,4>@6s,<1,2,3,4>@8s,<1,2,3,4>@9s}
-- >>> unzipx' s1
-- <{1@0s,1@2s,1@6s,1@8s,1@9s},{2@0s,2@2s,2@6s,2@8s,2@9s},{3@0s,3@2s,3@6s,3@8s,3@9s},{4@0s,4@2s,4@6s,4@8s,4@9s}>
unzipx' :: (Num t, Ord t, Eq t)
        => DE.SignalBase t (V.Vector a) -> V.Vector (DE.SignalBase t a)
unzipx' s@(a:-_) = unzipx (V.length $ DE.val a) s

