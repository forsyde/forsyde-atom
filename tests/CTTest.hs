module CTTest where

import           ForSyDe.Atom
import qualified ForSyDe.Atom.MoC.CT as CT
import qualified ForSyDe.Atom.MoC.DE as DE (Tag)
import qualified ForSyDe.Atom.MoC.SY as SY
import           ForSyDe.Atom.MoC.SY.Interface (toDE)
import           ForSyDe.Atom.MoC.DE.Interface (toCT)
import           ForSyDe.Atom.MoC.CT.Core (part, stream, eval)
import           ForSyDe.Atom.Signal as S
import           Test.HUnit
import           Test.QuickCheck

-- Utils

type TagList = OrderedList (DE.Tag)

sigGen  ts xs = toCT 0.5 $ toDE (SY.signal $ rmDup $ getOrdered ts) xs
  where types   = (ts :: TagList , xs :: SY.Sig Int)
        rmDup   = foldr rm []
        rm x [] = [x]
        rm x xs = if x == head xs then xs else x:xs

-- TODO: proof for total order!!!
checkTags NullS                              = True
checkTags (x:-NullS)                         = True
checkTags ((CT.CT t1 _):-x@(CT.CT t2 _):-xs) = t1 < t2 && checkTags (x:-xs)

constSig l = CT.signal $ (\(t,v) -> (t,\_->v)) <$> l

-- countVal _ NullS = 0
-- countVal a (CT.CT _ [x] :- xs) | a == x    = 1 + countVal a xs
--                                | otherwise = countVal a xs

-- -- Properties

prop_delay_1 ts xs = 1 + S.lengthS sig == S.lengthS (CT.delay 2 (\_->1) sig)
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sig   = sigGen ts xs

prop_delay_2 ts xs = checkTags $ CT.delay 2 (\_->1) sig
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sig   = sigGen ts xs   
        
prop_delay_comb_1 ts xs | S.lengthS sigi == 0 = S.lengthS sigo == 0
                        | otherwise           = 50 + S.lengthS sigi == S.lengthS (S.takeS (50 + S.lengthS sigi) sigo)
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sigi  = sigGen ts xs
        sigo  = CT.mealy11 (+) (-) (1,(\_->1)) sigi

prop_comb_1 ts xs = S.lengthS sig == S.lengthS (CT.comb11 (+1) sig)
  where types = (ts :: TagList , xs :: SY.Sig Int)
        sig   = sigGen ts xs   

prop_comb_2 tx xs ty ys | S.lengthS sig1 == 0 = S.lengthS sigo == 0
                        | S.lengthS sig2 == 0 = S.lengthS sigo == 0
                        | otherwise           = max (S.lengthS sig1) (S.lengthS sig2) <= S.lengthS sigo
  where types = (tx :: TagList , xs :: SY.Sig Int, ty :: TagList , ys :: SY.Sig Int)
        sig1  = sigGen tx xs
        sig2  = sigGen ty ys
        sigo  = CT.comb21 (+) sig1 sig2

prop_comb_3 tx xs ty ys = checkTags sigo
  where types = (tx :: TagList , xs :: SY.Sig Int, ty :: TagList , ys :: SY.Sig Int)
        sig1  = sigGen tx xs
        sig2  = sigGen ty ys
        sigo  = CT.comb21 (+) sig1 sig2

-- -- Unit tests

(s1,s2) = (S.takeS 5 k1, S.takeS 7 k2) :: (CT.Sig Int, CT.Sig Int)
  where (k1, k2) = CT.generate2 (\a b -> (a+1,b+2)) ((9,\_->2),(24,\_->5))

test_comb_1 = eval (CT.comb11 (+1) s1) @?= eval (constSig [(0,3),(9,4),(18,5),(27,6),(33,6)])
test_comb_2 = (eval,eval) $$ (CT.comb22 (\a b-> (a+b,a-b)) s1 s2) @?=
              (eval $ constSig [(0,7), (9,8), (18,9), (24,11),(27,12),(33,12),(42,12),(48,14),(51,14),(57,14)],
               eval $ constSig [(0,-3),(9,-2),(18,-1),(24,-3),(27,-2),(33,-2),(42,-2),(48,-4),(51,-4),(57,-4)])
              
test_delay_1 = eval (CT.delay 5 (\_->5) s1) @?= eval (constSig [(0,5),(5,2),(14,3),(23,4),(32,5),(38,5)])

test_constant = (S.takeS 3 c1, S.takeS 3 c2) @?= (eval $ constSig [(0,1)], eval $ constSig [(0,2)])
  where (c1, c2) = (eval,eval) $$ CT.constant2 (\_->1,\_->2)

test_generate = (eval s1, eval s2) @?= (eval $ constSig [(0,2),(9,3), (18,4),(27,5),(33,5)],               -- delta 9
                                        eval $ constSig [(0,5),(24,7),(33,7),(42,7),(48,9),(51,9),(57,9)]) -- delta 24

test_state = (S.takeS 5 . eval . CT.state11 (+)      (5,\_->2)) s1 @?= (eval . constSig) [(0,4),(5,6),(9,7),(10,9),(14,10)]
test_stated = (S.takeS 5 . eval . CT.stated11 (+)    (5,\_->2)) s1 @?= (eval . constSig) [(0,2),(5,4),(10,6),(14,7),(15,9)]
test_moore = (S.takeS 5 . eval . CT.moore11 (+) (+1) (5,\_->2)) s1 @?= (eval . constSig) [(0,3),(5,5),(10,7),(14,8),(15,10)]
test_mealy = (S.takeS 5 . eval . CT.mealy11 (+) (-)  (5,\_->2)) s1 @?= (eval . constSig) [(0,0),(5,2),(9,1),(10,3),(14,4)]
