module Test where

import ForSyDe
import qualified ForSyDe.MoC.SY as SY
import qualified ForSyDe.MoC.SDF as SDF
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

-- CT

