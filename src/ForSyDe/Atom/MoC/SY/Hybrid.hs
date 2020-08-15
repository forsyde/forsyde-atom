{-# LANGUAGE PostfixOperators #-}
module ForSyDe.Atom.MoC.SY.Hybrid where

import           ForSyDe.Atom.MoC
import qualified ForSyDe.Atom.MoC.SDF.Interface as SDF
import           ForSyDe.Atom.MoC.SY.Core as SY
import qualified ForSyDe.Atom.MoC.SY.Interface as SY

-- Hybrid skeletons

-- | Interleaves two SY signals by passing them through a SDF process
-- first. In other words it wraps a SDF process with \(n\) inputs with
-- consumption rates \(1\) and one output with production rate \(n\)
-- inside a SY environment.
--
-- The output is up-sampled, thus breaking the SY hypothesis on the
-- whole system. The inputs and outputs can rather be regarded as two
-- distinct SY domains.
--
-- Constructors: @interleave[2-4]@
-- 
-- >>> let s1 = SY.signal [1..5]
-- >>> let s2 = SY.signal [11..15]
-- >>> interleave2 s1 s2
-- {1,11,2,12,3,13,4,14,5,15}
interleave2 :: Signal a -> Signal a -> Signal a
interleave2 a b = SDF.toSY1' (ctxt21 (1,1) 2 (++)
                  -.- SY.toSDF1 a -*- SY.toSDF1 b -*)

interleave3 :: Signal a -> Signal a -> Signal a -> Signal a
interleave3 a b c = SDF.toSY1' (ctxt31 (1,1,1) 3 (\x y z -> x++y++z)
                  -.- SY.toSDF1 a -*- SY.toSDF1 b -*- SY.toSDF1 c -*)

interleave4 :: Signal a -> Signal a -> Signal a -> Signal a -> Signal a
interleave4 a b c d = SDF.toSY1' (ctxt41 (1,1,1,1) 4 (\x y z q -> x++y++z++q)
                  -.- SY.toSDF1 a -*- SY.toSDF1 b -*- SY.toSDF1 c -*- SY.toSDF1 d -*)
