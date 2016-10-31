{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.CT.Interface where

import qualified ForSyDe.Atom.MoC.CT.Core as CT 
import qualified ForSyDe.Atom.Skeleton.Vector as V (Vector, zipx, unzipx)

-- Towards skeleton layer

-- | Synchronizes all signals inside a vector into one signal carrying
-- vector events. This is simply an instantiation of the skeleton
-- 'V.zipx', which passes the CT context wrappers ('CT.wrap22') to
-- instantiate a 'ForSyDe.Atom.MoC.comb22' properly.
zipx :: V.Vector (CT.Sig a) -> CT.Sig (V.Vector a)
zipx = V.zipx CT.wrap21 CT.wrap11


-- | Unfolds a signal carrying vector events into a vector of synchronized
-- signals. This is simply an instantiation of the skeleton 'V.unzipx',
-- which passes the CT context wrappers ('CT.wrap22') to instantiate a
-- 'ForSyDe.Atom.MoC.comb22' properly.
--
-- __/ATTENTION!/__ this  is a temporary unsafe  implementation, since
-- it assumes all events are carrying vectors of the same length.
unzipx :: CT.Sig (V.Vector a) -> V.Vector (CT.Sig a)
unzipx = V.unzipx (CT.sniff) CT.wrap11 CT.wrap11

