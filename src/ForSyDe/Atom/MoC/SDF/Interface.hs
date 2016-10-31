{-# LANGUAGE TypeFamilies, PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.SDF.Interface where

import qualified ForSyDe.Atom.Behavior as B
import qualified ForSyDe.Atom.MoC as MoC
import qualified ForSyDe.Atom.MoC.SDF.Lib as SDF
import qualified ForSyDe.Atom.Skeleton.Vector as V

-- Towards skeleton layer

-- | Synchronizes all signals inside a vector into one signal carrying
-- vector events. 
zipx cs = V.reduce2' catEv (SDF.constant1 [V.Null]) cs . V.map11 unitEv 
  where 
    catEv c = SDF.comb21 ((c,1),1,\x y->  [(V.concat (V.vector x)) V.<++> (V.concat (V.vector y))])
    unitEv  = SDF.comb11 (1,1,map V.unit)

-- | Unfolds a signal carrying vector events into a vector of synchronized
-- signals. This is simply an instantiation of the skeleton 'V.unzipx',
-- which passes the SDF context wrappers ('SDF.wrap22') to instantiate a
-- 'ForSyDe.Atom.MoC.comb22' properly.
--
-- __/ATTENTION!/__ this  is a temporary unsafe  implementation, since
-- it assumes all events are carrying vectors of the same length.
-- unzipx :: SDF.Sig (V.Vector a) -> V.Vector (SDF.Sig a)
-- unzipx = V.unzipx SDF.partition SDF.wrap11 SDF.wrap11

