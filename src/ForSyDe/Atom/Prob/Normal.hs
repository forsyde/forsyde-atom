module ForSyDe.Atom.Prob.Normal where

import System.Random
import qualified Data.Random.Normal as N
import ForSyDe.Atom.Prob

normal' :: (Random a, Floating a) => Dist a
normal' = Dist (fst . N.normal)

normal :: (Random a, Floating a) => a -> a -> Dist a
normal dev m = Dist $ fst . N.normal' (m,dev)


ranges l r n = takeWhile (\(x,y) -> x < r)
               [(l + k*step, l + (k+1)*step-0.0000001) | k <- [0..]]
  where step = (r-l) / n
count (l,r) = length.filter (\x -> l <= x && x <= r)
histogram l r n xs = count <$> range <*> pure xs
  where range = ranges l r n

plothist r l n = zipWith (\(r,l) h->((r+l)/2,h)) (ranges r l n) . histogram r l n

montecarlo n d qs = take n $ map (\r -> sample r d) qs 
