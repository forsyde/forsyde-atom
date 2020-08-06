{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.CT.Interface where

import ForSyDe.Atom.MoC ((-.-), (-*-))
import ForSyDe.Atom.MoC.CT.Core as CT
import ForSyDe.Atom.MoC.DE.Core as DE
import ForSyDe.Atom.MoC.DE.Interface (toCT1)
import ForSyDe.Atom.MoC.Stream (Stream(..))
import ForSyDe.Atom.MoC.Time (Time(..))
import qualified ForSyDe.Atom.Skel.Vector as V (
  Vector, zipx, unzipx, fanout, unit, length, vector)
import ForSyDe.Atom.Utility.Tuple (($$),($$$),($$$$))

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)
-- >>> import ForSyDe.Atom.MoC.DE.Lib as DE
-- >>> import ForSyDe.Atom.MoC.CT.Lib as CT
-- >>> import qualified Data.Number.FixedFunctions as RatF
-- >>> let pi'  = realToFrac pi
-- >>> let sin' = RatF.sin 0.001
-- >>> let cos' = RatF.cos 0.001


------- MoC INTERFACES -------

-- | Translates a (set of) 'ForSyDe.Atom.MoC.CT.CT' signal(s) into
-- 'ForSyDe.Atom.MoC.DE.DE' semantics without loss of information. In
-- 'ForSyDe.Atom.MoC.DE.DE', the abstract function of time inferred by
-- the 'ForSyDe.Atom.MoC.CT.CT' event loses its abstraction and it is
-- "dropped" to explicit form, under a lower layer. In other words the
-- implicit time semantics are lost, the carried value simply becoming
-- an ordinary function.
--
-- Constructors: @toDE[1-4]@.
--
-- <<fig/moc-ct-tode.png>>
toDE1 :: (Num ts, Ord ts, Eq ts, Num tm, Ord tm) 
      => CT.SignalBase ts tm a
      -> DE.SignalBase ts (tm -> a)
toDE2 :: (Num ts, Ord ts, Eq ts, Num tm, Ord tm) 
      => (CT.SignalBase ts tm a, CT.SignalBase ts tm b)
      -> (DE.SignalBase ts (tm -> a), DE.SignalBase ts (tm -> b))
toDE3 :: (Num ts, Ord ts, Eq ts, Num tm, Ord tm) 
      => (CT.SignalBase ts tm a, CT.SignalBase ts tm b, CT.SignalBase ts tm c)
      -> (DE.SignalBase ts (tm -> a), DE.SignalBase ts (tm -> b),
          DE.SignalBase ts (tm -> c))
toDE4 :: (Num ts, Ord ts, Eq ts, Num tm, Ord tm) 
      => (CT.SignalBase ts tm a, CT.SignalBase ts tm b,
          CT.SignalBase ts tm c, CT.SignalBase ts tm d)
      -> (DE.SignalBase ts (tm -> a), DE.SignalBase ts (tm -> b),
          DE.SignalBase ts (tm -> c), DE.SignalBase ts (tm -> d))

toDE1 = fmap (\(CT ts p f) -> DE ts (\t -> f (t + p)))
toDE2 = ($$) (toDE1, toDE1)
toDE3 = ($$$) (toDE1, toDE1, toDE1)
toDE4 = ($$$$) (toDE1, toDE1, toDE1, toDE1)

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.CT.CT' signal(s) with a
-- 'ForSyDe.Atom.MoC.DE.DE' carrier which holds the timestamps at
-- which the CT signal must be sampled, and outputs the respective
-- (set of) 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- Constructors: @sampDE[1-4]@.
--
-- >>> let s = CT.infinite (fromRational . sin')
-- >>> let c = DE.generate1 id (pi'/2, 1)
-- >>> takeS 6 $ sampDE1 c s
-- {0.0@0.0,1.0@1.5707963267948966,1.2246489323228883e-16@3.141592653589793,-1.0@4.71238898038469,0.0@6.283185307179586,1.0@7.853981633974483}

--
-- <<fig/moc-ct-sampde.png>>
sampDE2 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => DE.SignalBase ts t -- ^ 'ForSyDe.Atom.MoC.DE.DE' timestamp carrier 
        -> CT.SignalBase ts tm a -- ^ 'ForSyDe.Atom.MoC.CT.CT' input
        -> CT.SignalBase ts tm b -- ^ 'ForSyDe.Atom.MoC.CT.CT' input
        -> (DE.SignalBase ts a, DE.SignalBase ts b) -- ^ 'ForSyDe.Atom.MoC.DE.DE' outputs
sampDE1 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => DE.SignalBase ts t
        -> CT.SignalBase ts tm a
        -> DE.SignalBase ts a
sampDE3 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => DE.SignalBase ts t
        -> CT.SignalBase ts tm a -> CT.SignalBase ts tm b -> CT.SignalBase ts tm c
        -> (DE.SignalBase ts a, DE.SignalBase ts b, DE.SignalBase ts c)
sampDE4 :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => DE.SignalBase ts t
        -> CT.SignalBase ts tm a -> CT.SignalBase ts tm b -> CT.SignalBase ts tm c
        -> CT.SignalBase ts tm d
        -> (DE.SignalBase ts a, DE.SignalBase ts b, DE.SignalBase ts c,
            DE.SignalBase ts d)

sampDE1 carrier = fmap evalEvent . sync carrier
  where evalEvent e@(CT ts _ _) = DE.DE ts (CT.evalTs ts e)
        sync c s = (\_ x -> x) -.- toCT1 ((const . const) () -.- c) -*- s
sampDE2 c s1 s2       = (sampDE1 c s1, sampDE1 c s2)
sampDE3 c s1 s2 s3    = (sampDE1 c s1, sampDE1 c s2, sampDE1 c s3) 
sampDE4 c s1 s2 s3 s4 = (sampDE1 c s1, sampDE1 c s2, sampDE1 c s3, sampDE1 c s4)

-- Towards skeleton layer

-- | Synchronizes all the signals contained by a vector and zips them
-- into one signal of vectors. It instantiates the
-- 'ForSyDe.Atom.Skel.Vector.zipx' skeleton.
--
-- >>> let s1 = CT.signal [(0,const 1), (2,const 2), (6,const 3)]
-- >>> let s2 = CT.signal [(0,const 1), (2,const 2), (4,const 3)]
-- >>> let v1 = V.vector [s1,s1,s2,s2]
-- >>> zipx v1
-- {<1,1,1,1>@0,<2,2,2,2>@2,<2,2,3,3>@4,<3,3,3,3>@6}
--
-- See 'ForSyDe.Atom.MoC.DE.zipx' from the "ForSyDe.Atom.MoC.DE"
-- library for a comprehensive visual example.
zipx :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
     => V.Vector (CT.SignalBase ts tm a) -> CT.SignalBase ts tm (V.Vector a)
zipx = V.zipx (V.fanout (\cat a b -> a `cat` b))

-- | Unzips the vectors carried by a signal into a vector of
-- signals. It instantiates the 'ForSyCt.Atom.Skel.Vector.unzipx'
-- skeleton. To avoid infinite recurrence, the user needs to provict
-- the length of the output vector.
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = CT.signal [(0,const v1),(2,const v1),(6,const v1)]
-- >>> unzipx 4 s1
-- <{4@0,4@2,4@6},{3@0,3@2,3@6},{2@0,2@2,2@6},{1@0,1@2,1@6}>
--
-- See 'ForSyDe.Atom.MoC.DE.unzipx' from the "ForSyDe.Atom.MoC.DE"
-- library for a comprehensive visual example.
unzipx :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
       => Integer -> CT.SignalBase ts tm (V.Vector a)
       -> V.Vector (CT.SignalBase ts tm a)
unzipx = V.unzipx id

-- | Same as 'unzipx', but \"sniffs\" the first event to determine the
-- length of the output vector. Has unsafe behavior!
--
-- >>> let v1 = V.vector [1,2,3,4]
-- >>> let s1 = CT.signal [(0,const v1),(2,const v1),(6,const v1)]
-- >>> unzipx' s1
-- <{4@0,4@2,4@6},{3@0,3@2,3@6},{2@0,2@2,2@6},{1@0,1@2,1@6}>
unzipx' :: (Num ts, Real ts, Ord ts, Eq ts, Num tm, Fractional tm, Ord tm) 
        => CT.SignalBase ts tm (V.Vector a) -> V.Vector (CT.SignalBase ts tm a)
unzipx' s@(a:-_) = unzipx (V.length $ CT.evalEv a) s
