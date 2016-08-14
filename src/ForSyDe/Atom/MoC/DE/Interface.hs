{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.DE.Interface where

import           ForSyDe.Atom.Behavior
import           ForSyDe.Atom.MoC hiding (comb22, comb33, comb44)
import           ForSyDe.Atom.MoC.DE.Core (Tag)
import           ForSyDe.Atom.MoC.DE.Lib (sync2, sync3, sync4)
import           ForSyDe.Atom.Utility

import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.SY.Core as SY
import qualified ForSyDe.Atom.MoC.CT.Core as CT

eventToSY :: DE.Event a -> (SY.Event Tag, SY.Event a)
eventToSY (DE.DE t a) = (SY.event t, SY.SY a)

toSY  :: DE.Sig a ->                                     (SY.Sig Tag, SY.Sig a)
toSY3 :: DE.Sig a -> DE.Sig b -> DE.Sig c ->             (SY.Sig Tag, SY.Sig a, SY.Sig b, SY.Sig c)
toSY4 :: DE.Sig a -> DE.Sig b -> DE.Sig c -> DE.Sig d -> (SY.Sig Tag, SY.Sig a, SY.Sig b, SY.Sig c, SY.Sig d)

toSY  s1           = (eventToSY <$> s1 |<)
toSY2 s1 s2        = let (de1, de2)           = sync2 s1 s2
                     in  (fst $ toSY de1, snd $ toSY de1, snd $ toSY de2) 
toSY3 s1 s2 s3     = let (de1, de2, de3)      = sync3 s1 s2 s3
                     in  (fst $ toSY de1, snd $ toSY de1, snd $ toSY de2, snd $ toSY de3) 
toSY4 s1 s2 s3 s4  = let (de1, de2, de3, de4) = sync4 s1 s2 s3 s4
                     in  (fst $ toSY de1, snd $ toSY de1, snd $ toSY de2, snd $ toSY de3, snd $ toSY de4) 


eventToCT :: CT.Time -> DE.Event a -> CT.Event a
eventToCT scale (DE.DE t a) = CT.CT (scale * (fromInteger (toInteger t))) (\_->a)

toCT  :: CT.Time -> DE.Sig a ->                                     (CT.Sig a)
toCT3 :: CT.Time -> DE.Sig a -> DE.Sig b -> DE.Sig c ->             (CT.Sig a, CT.Sig b, CT.Sig c)
toCT4 :: CT.Time -> DE.Sig a -> DE.Sig b -> DE.Sig c -> DE.Sig d -> (CT.Sig a, CT.Sig b, CT.Sig c, CT.Sig d)

toCT  sc s1          = eventToCT sc <$> s1
toCT2 sc s1 s2       = (toCT sc, toCT sc)                   $$   (s1,s2)
toCT3 sc s1 s2 s3    = (toCT sc, toCT sc, toCT sc)          $$$  (s1,s2,s3)
toCT4 sc s1 s2 s3 s4 = (toCT sc, toCT sc, toCT sc, toCT sc) $$$$ (s1,s2,s3,s4)


----------------- DOCUMENTATION -----------------

-- | Translates a (set of) DE signal(s) into the equivalent (set of)
-- SY ones after synchronizing them. The outputs are tupled with a SY
-- signal carrying events containing the timestaps for the
-- synchronization points.
--
-- <<includes/figs/de-tosy-graph.png>>
--
-- "ForSyDe.Atom.MoC.DE" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > toSY1, toSY2, toSY3, toSY4,
toSY2 :: DE.Sig a             -- ^ first input DE signal
      -> DE.Sig b             -- ^ second input DE signal
      -> (SY.Sig Tag, SY.Sig a, SY.Sig b)
      -- ^ signal carrying timestamps tupled with the two output
      -- 'ForSyDe.Atom.MoC.SY.SY' signals


-- | Translates a (set of) DE signal(s) into the equivalent (set of)
-- CT ones by transforming its values into constant functions. The
-- process constructor inputs also a factor to scale timestamps into
-- physical time and.
--
-- <<includes/figs/de-toct-graph.png>>
--
-- "ForSyDe.Atom.MoC.DE" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > toCT1, toCT2, toCT3, toCT4,
toCT2 :: CT.Time              -- ^ time scale
      -> DE.Sig a             -- ^ first input DE signal
      -> DE.Sig b             -- ^ second input DE signal
      -> (CT.Sig a, CT.Sig b)
      -- ^ two output 'ForSyDe.Atom.MoC.CT.CT' signals

      
--------------- END DOCUMENTATION ---------------
