{-# LANGUAGE TypeFamilies #-}
module FFT where

import ForSyDe.Atom
import ForSyDe.Atom.Behavior (psi22,psi11)
import qualified ForSyDe.Atom.MoC as MoC (comb22, comb11)
import qualified ForSyDe.Atom.MoC.SY as SY
import qualified ForSyDe.Atom.MoC.DE as DE
import qualified ForSyDe.Atom.MoC.CT as CT
import qualified ForSyDe.Atom.MoC.SDF as SDF
import qualified ForSyDe.Atom.Skeleton.Vector as V

import Data.Complex


-- assumes n == 2 ^ k
fft bffunc k vs = (V.bitrev . V.pipe1 stage (V.iterate k (*2) 2)) vs
  where
    n = V.length vs 
    stage     k         = V.concat . V.map21 segment (V.take (n `div` k) twiddles) . V.group k
    segment   twid      = V.unduals . V.map11 (butterfly twid) . V.duals
    butterfly w (s1,s2) = MoC.comb22 (bffunc w) s1 s2
    twiddles            = (V.bitrev . V.map11 halfcycle . V.take (n `div` 2)) V.indexes
      where halfcycle l = cis $ negate $ -2 * pi * (fromInteger (l - 1)) / (fromInteger n)


syFFT  k = getMagnitude SY.wrap11 . fft (bffunc SY.wrap22) k . sampleSignal (SY.delay 0) k 
deFFT  k = getMagnitude DE.wrap11 . fft (bffunc DE.wrap22) k . sampleSignal (DE.delay 5 0) k 
ctFFT  k = getMagnitude CT.wrap11 . fft (bffunc CT.wrap22) k . sampleSignal (CT.delay 0.5 (\_->0)) k 
sdfFFT k = getMagnitude' (SDF.wrap11 1 1) . fft (bffunc' (SDF.wrap22 (1,1) (1,1))) k . sampleSignal (SDF.delay [0]) k

bffunc  mocwrapper w = (mocwrapper . psi22) (\x0 x1 -> let t = w * x1 in (x0 + t, x0 - t))
bffunc' mocwrapper w = (mocwrapper . psi22) (\x0 x1 -> let t = w * (head x1) in ([(head x0) + t], [(head x0) - t]))

sampleSignal delta k s = V.systolic0 (V.fanoutn (2 ^ k) delta) s
getMagnitude  mocwrapper = V.map11 (MoC.comb11 (mocwrapper (psi11 magnitude)))
getMagnitude' mocwrapper = V.map11 (MoC.comb11 (mocwrapper (psi11 (map magnitude))))





-- fft :: Int -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
-- fft k xs | n == 2 ^ k = (zipxPN . bitrevPN . unzipxPN . pipe1PN stage (iterateV k (*2) 2)) xs
--   where
-- 	stage :: Int -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
-- 	stage k = zipxPN . concatPN . (mapV unzipxPN) . farm1PN segment (takeV m twiddles) 
--               . (mapV zipxPN) . groupPN k . unzipxPN
-- 		where m = n `div` k

-- 	segment :: Complex Float -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
-- 	segment twid = zipxPN . undualsSYPN . farmPN (butterfly twid) . dualsSYPN . unzipxPN

-- 	butterfly :: RealFloat a => Complex a -> Signal (Complex  a, Complex a) -> Signal (Complex a, Complex a)
-- 	butterfly w = mapSY (\(x0, x1) -> let t = w * x1 in (x0 + t, x0 - t))

-- 	twiddles :: Vector (Complex Float)
-- 	twiddles = vector $ (bitrev . map (cis . negate) . halfcycle) (toInteger n)

-- 	halfcycle :: Integer -> [Float]
-- 	halfcycle n = halfcycle1 0 (fromInteger n / 2) n
-- 	  	where halfcycle1 l m n 
-- 			           | l == m = []
-- 					   | l /= m = -2 * pi * l / (fromInteger n) : halfcycle1 (l+1) m n

-- 	n = lengthV $ unzipxPN xs

-- -- helpers

-- evens []  = []
-- evens [x] = [x]
-- evens (x:_:xs) = x : evens xs
-- odds []  = []
-- odds [x] = []
-- odds (_:x:xs) = x : odds xs
-- bitrev :: [a] -> [a]
-- bitrev [x] = [x]
-- bitrev xs = bitrev (evens xs) ++ bitrev (odds xs)
