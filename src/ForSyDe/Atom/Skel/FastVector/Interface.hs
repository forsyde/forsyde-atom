{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Atom.Skel.FastVector.Interface where

import ForSyDe.Atom.Skel.FastVector.Lib
import ForSyDe.Atom.MoC.SY as SY
import Data.List as L

-- | Fast equivalent of a SY 'ForSyDe.Atom.MoC.SY.unzipx'.
unzipx :: SY.Signal (Vector a) -> Vector (SY.Signal a)
unzipx = vector . map SY.signal . L.transpose . map fromVector . SY.fromSignal

-- | Fast equivalent of a SY 'ForSyDe.Atom.MoC.SY.zipx'.
zipx :: Vector (SY.Signal a) -> SY.Signal (Vector a)
zipx = SY.signal . map vector . L.transpose . map SY.fromSignal . fromVector
