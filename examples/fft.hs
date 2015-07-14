import ForSyDe.Shallow hiding (fft)
import ForSyDe.Shallow.Patterns

import Data.Complex

fft :: Int -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
fft k xs | n == 2 ^ k = (zipxPN . bitrevPN . unzipxPN . pipe1PN stage (iterateV k (*2) 2)) xs
  where
	stage :: Int -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
	stage k = zipxPN . concatPN . (mapV unzipxPN) . farm1PN segment (takeV m twiddles) 
              . (mapV zipxPN) . groupPN k . unzipxPN
		where m = n `div` k

	segment :: Complex Float -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
	segment twid = zipxPN . undualsSYPN . farmPN (butterfly twid) . dualsSYPN . unzipxPN

	butterfly :: RealFloat a => Complex a -> Signal (Complex  a, Complex a) -> Signal (Complex a, Complex a)
	butterfly w = mapSY (\(x0, x1) -> let t = w * x1 in (x0 + t, x0 - t))

	twiddles :: Vector (Complex Float)
	twiddles = vector $ (bitrev . map (cis . negate) . halfcycle) (toInteger n)

	halfcycle :: Integer -> [Float]
	halfcycle n = halfcycle1 0 (fromInteger n / 2) n
	  	where halfcycle1 l m n 
			           | l == m = []
					   | l /= m = -2 * pi * l / (fromInteger n) : halfcycle1 (l+1) m n

	n = lengthV $ unzipxPN xs

-- helpers

evens []  = []
evens [x] = [x]
evens (x:_:xs) = x : evens xs
odds []  = []
odds [x] = []
odds (_:x:xs) = x : odds xs
bitrev :: [a] -> [a]
bitrev [x] = [x]
bitrev xs = bitrev (evens xs) ++ bitrev (odds xs)
