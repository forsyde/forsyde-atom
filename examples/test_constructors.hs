module Test where

import ForSyDe
import ForSyDe.Core.Signal
import qualified ForSyDe.MoC.SY as SY
import qualified ForSyDe.MoC.SDF as SDF
import qualified ForSyDe.MoC.DE as DE
import qualified ForSyDe.MoC.CT as CT
import qualified Data.Param.FSVec as V
import qualified Data.TypeLevel as T

s = signal [1,2,3,4,5,6,7,8,9,10,11] :: Signal Int


-- SY
syComb  = SY.comb2 (+) (SY.delay 0 s) s
syOsc   = SY.comb (+1) (SY.delay 0 syOsc)

-- SDF
f1sdf :: V.FSVec T.D1 Int -> V.FSVec T.D1 Int
f1sdf = V.map (+1)

f2sdf :: V.FSVec T.D1 Int -> V.FSVec T.D3 Int -> V.FSVec T.D1 Int
f2sdf x y = V.singleton $ V.foldl (+) (V.head x) y

sdfComb = SDF.comb2 f2sdf (SDF.delay 0 s) s
sdfOsc  = SDF.comb  f1sdf (SDF.delay 0 sdfOsc)

-- DE
s2 = signal [DE.Subsig (1.0, 1), DE.Subsig (2.0, 0.5)]
i2 = DE.Subsig (0.5, 0.5)
deComb  = DE.comb2 (+) (DE.delay i2 s2) s2
deOsc   = DE.comb (+1) (DE.delay i2 deOsc)

q1 = signal [Event 0 (D 2), Event 2 (D 1), Event 6 (D 3)]
q2 = signal [Event 1 (D 2), Event 2 (D 1), Event 6 (D 3)]
i1 = Event 1 (D 1)
i2 = Event 3 (D 1)
i3 = Event 0 (D 1)

osc1 = (+1) -$- (i1 ->- osc1)
osc2 = (+1) -$- (i2 ->- osc2)
osc3 = (+1) -$- (i3 ->- osc3)

cosc1 = (+) -$- (i1 ->- cosc1) -*- q1
cosc2 = (+) -$- (i2 ->- cosc2) -*- q1
cosc3 = (+) -$- (i1 ->- cosc3) -*- q2
cosc4 = (+) -$- (i2 ->- cosc4) -*- q2
cosc5 = (+) -$- (i3 ->- cosc5) -*- q2

-- CT
s1 = signal [CT.Subsig (1.0, (\_ -> 1)), CT.Subsig (2.0, (\_ -> 0.5))]
i1 = CT.Subsig (0.5, (\_ -> 0.5))
ctComb  = CT.comb2 (+) (CT.delay i1 s1) s1
ctOsc   = CT.comb (+1) (CT.delay i1 ctOsc)


plot :: (Num a, Show a) => Rational -> Signal (CT.Subsig a) -> Signal a
plot step = plot' 0.0
  where 
    plot' _    NullS                   = NullS
    plot' prev (CT.Subsig (tag, f) :- ss) = (f <$> (signal [prev, prev + step .. tag - step])) +-+ (plot' tag ss)

