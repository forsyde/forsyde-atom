{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.CT.Interface where

import ForSyDe.Atom.MoC.CT.Core as CT
import ForSyDe.Atom.MoC.DE.Core as DE
import ForSyDe.Atom.MoC.Stream

import Data.Ratio

tag2time resolution s = 

  toDE :: CT.Time -> DE.Signal t -> CT.Signal a -> DE.Signal a
toDE resolution carrier s =
  where
    
-- toDE res carrier s = evaluate s carrier
--   where evaluate NullS _ = NullS
--         evaluate _ NullS = NullS
--         evaluate l@(CT tm _ f:-NullS) (DE tg _:-ds)
--           = (DE tg (f $ res*(toInteger tg%1))) :- evaluate l ds
--         evaluate l@(CT tm1 f1:-CT tm2 f2:-cs) m@(DE tg _:-ds)
--           | (toInteger tg%1)*res < tm2 = (DE tg (f1 $ res*(toInteger tg%1))) :- evaluate l ds
--           | otherwise    = evaluate (CT tm2 f2:-cs) m 

-- Towards skeleton layer

-- -- | Synchronizes all signals inside a vector into one signal carrying
-- -- vector events. This is simply an instantiation of the skeleton
-- -- 'V.zipx', which passes the CT context wrappers ('CT.wrap22') to
-- -- instantiate a 'ForSyDe.Atom.MoC.comb22' properly.
-- zipx :: V.Vector (CT.Signal a) -> CT.Signal (V.Vector a)
-- zipx = V.zipx CT.wrap21 CT.wrap11


-- -- | Unfolds a signal carrying vector events into a vector of synchronized
-- -- signals. This is simply an instantiation of the skeleton 'V.unzipx',
-- -- which passes the CT context wrappers ('CT.wrap22') to instantiate a
-- -- 'ForSyDe.Atom.MoC.comb22' properly.
-- --
-- -- __/ATTENTION!/__ this  is a temporary unsafe  implementation, since
-- -- it assumes all events are carrying vectors of the same length.
-- unzipx :: CT.Signal (V.Vector a) -> V.Vector (CT.Signal a)
-- unzipx = V.unzipx (CT.sniff) CT.wrap11 CT.wrap11

