module DETest where

import           ForSyDe.Atom
import qualified ForSyDe.Atom.MoC.DE as DE
import qualified ForSyDe.Atom.MoC.SY as SY
import           ForSyDe.Atom.MoC.SY.Interface (toDE)
import           ForSyDe.Atom.MoC.DE.Interface (toSY)
import           ForSyDe.Atom.MoC.DE.Core (part, stream)
import           ForSyDe.Atom.Signal as S
import           Test.HUnit
import           Test.QuickCheck

-- Utils

type TagList = OrderedList DE.Tag

sigGen  ts xs = toDE (SY.signal $ rmDup $ getOrdered ts) xs
  where types   = (ts :: TagList , xs :: SY.Sig Int)
        rmDup   = foldr rm []
        rm x [] = [x]
        rm x xs = if x == head xs then xs else x:xs

-- TODO: proof for total order!!!
checkTags NullS                              = True
checkTags (x:-NullS)                         = True
checkTags ((DE.DE t1 _):-x@(DE.DE t2 _):-xs) = t1 < t2 && checkTags (x:-xs)

countVal _ NullS = 0
countVal a (DE.DE _ [x] :- xs) | a == x    = 1 + countVal a xs
                               | otherwise = countVal a xs

-- Properties

prop_delay_1 ts xs = 1 + length sig == length (DE.delay 2 1 sig)
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sig   = sigGen ts xs

prop_delay_2 ts xs = checkTags $ DE.delay 2 1 sig
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sig   = sigGen ts xs   
        
prop_delay_comb_1 ts xs | length sigi == 0 = length sigo == 0
                        | otherwise           = 50 + length sigi == length (S.takeS (50 + length sigi) sigo)
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sigi  = sigGen ts xs
        sigo  = DE.mealy11 (+) (-) (1,1) sigi

prop_comb_1 ts xs = length sig == length (DE.comb11 (+1) sig)
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sig   = sigGen ts xs   

prop_comb_2 tx xs ty ys | length sig1 == 0 = length sigo == 0
                        | length sig2 == 0 = length sigo == 0
                        | otherwise           = max (length sig1) (length sig2) <= length sigo
  where types = (tx :: TagList , xs :: SY.Sig Int, ty :: TagList , ys :: SY.Sig Int)
        sig1  = sigGen tx xs
        sig2  = sigGen ty ys
        sigo  = DE.comb21 (+) sig1 sig2

prop_comb_3 tx xs ty ys = checkTags sigo
  where types = (tx :: TagList , xs :: SY.Sig Int, ty :: TagList , ys :: SY.Sig Int)
        sig1  = sigGen tx xs
        sig2  = sigGen ty ys
        sigo  = DE.comb21 (+) sig1 sig2

prop_comb_psi_1 ts xs = inputUndef == outputUndef
  where types       = (ts :: TagList , xs :: SY.Sig Int)
        inputUndef  = countVal Undef (sigGen ts xs)
        outputUndef = countVal Undef (DE.comb11 (+1) (sigGen ts xs))

prop_comb_psi_2 tx xs ty ys = minInUndef <= outUndef
  where types      = (tx :: TagList , xs :: SY.Sig Int, ty :: TagList , ys :: SY.Sig Int)
        sig1       = sigGen tx xs
        sig2       = sigGen ty ys
        minInUndef = max (countVal Undef sig1) (countVal Undef sig2)
        outUndef   = countVal Undef (DE.comb21 (+) sig1 sig2)

prop_to_sy_1 ts xs = length sig == length out1 && length sig == length out2
  where types       = (ts :: TagList , xs :: SY.Sig Int)
        sig         = sigGen ts xs
        (out1,out2) = toSY sig

-- Unit tests

(s1,s2) = (S.takeS 5 k1, S.takeS 7 k2) :: (DE.Sig Int, DE.Sig Int)
  where (k1, k2) = DE.generate2 (\a b -> (a+1,b+2)) ((9,2),(24,5))

test_comb_1 = DE.comb11 (+1) s1 @?= DE.signal [(0,3),(9,4),(18,5),(27,6),(33,6)]
test_comb_2 = DE.comb22 (\a b-> (a+b,a-b)) s1 s2 @?=
              (DE.signal [(0,7),(9,8),(18,9),(24,11),(27,12),(33,12),(42,12),(48,14),(51,14),(57,14)],
               DE.signal [(0,-3),(9,-2),(18,-1),(24,-3),(27,-2),(33,-2),(42,-2),(48,-4),(51,-4),(57,-4)])
              
test_delay_1 = DE.delay 5 5 s1 @?= DE.signal [(0,5),(5,2),(14,3),(23,4),(32,5),(38,5)]

test_constant = (S.takeS 3 c1, S.takeS 3 c2) @?= (DE.signal [(0,1)], DE.signal [(0,2)])
  where (c1, c2) = DE.constant2 (1,2)

test_generate = (s1, s2) @?= (DE.signal [(0,2),(9,3),(18,4),(27,5),(33,5)],                -- delta 9
                              DE.signal [(0,5),(24,7),(33,7),(42,7),(48,9),(51,9),(57,9)]) -- delta 24

test_state = S.takeS 5 (DE.state11 (+) (5,2) s1)      @?= DE.signal [(0,4),(5,6),(9,7),(10,9),(14,10)]
test_stated = S.takeS 5 (DE.stated11 (+) (5,2) s1)    @?= DE.signal [(0,2),(5,4),(10,6),(14,7),(15,9)]
test_moore = S.takeS 5 (DE.moore11 (+) (+1) (5,2) s1) @?= DE.signal [(0,3),(5,5),(10,7),(14,8),(15,10)]
test_mealy = S.takeS 5 (DE.mealy11 (+) (-) (5,2) s1)  @?= DE.signal [(0,0),(5,2),(9,1),(10,3),(14,4)]
