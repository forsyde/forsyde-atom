{-# LANGUAGE FlexibleInstances #-}
module SDFTest where

import           ForSyDe.Atom
import qualified ForSyDe.Atom.MoC.SDF as SDF
import           ForSyDe.Atom.MoC.SDF.Core --(part, stream)
import           ForSyDe.Atom.Signal as S
import           Test.HUnit
import           Test.QuickCheck


type Rate = Positive Int

-- Utils

checkProd l NullS         = True
checkProd l (SDF x :- xs) = length x == l && checkProd l xs

countToks :: SDF.Sig a -> Int
countToks = foldr (\y x -> x + length y) 0 . fmap fromSDF

-- Properties

prop_prod_1 xs = (countToks xs) `div` (3 * 2) >= (countToks out) `div` (2 * 3)
  where types = xs :: SDF.Sig Int
        out   = SDF.mealy11 ((2,4), 2, zipWith (+)) ((3,1), 3, (\x y -> (+(head y)) <$> x)) [1,1,1] xs

prop_prod_2 gc gp xs = rateI >= rateO && rateI <= rateO + 1
  where types  = (gc :: Rate, gp :: Rate, xs :: SDF.Sig Int)
        (c, p) = (getPositive gc, getPositive gc)
        rateI  = (countToks xs) `div` c
        rateO  = (countToks out) `div` p
        out    = SDF.comb11 (c, p, take p . repeat . head) xs

prop_prod_3 gc gp xs ys = rateI1 >= rateO && rateI2 >= rateO && (min rateI1 rateI2) <= rateO + 1
  where types  = (gc :: (Rate,Rate), gp :: Rate, xs :: SDF.Sig Int, ys :: SDF.Sig Int)
        (c1, c2, p) = (getPositive $ fst gc, getPositive $ snd gc, getPositive gp)
        rateI1 = (countToks xs) `div` c1
        rateI2 = (countToks ys) `div` c2
        rateO  = (countToks out) `div` p
        out    = SDF.comb21 ((c1,c2), p, \x -> take p . repeat . head) xs ys
        

-- Unit tests

(s1,s2) = (S.takeS 10 k1, S.takeS 10 k2) :: (SDF.Sig Int, SDF.Sig Int)
  where (k1, k2) = SDF.generate2 ((1,1),(2,1), \a b -> ([head a, last b + 1],[last a + 2])) ([1,1],[2,2,2]) 

test_comb_1 = SDF.comb11 (7,7,map (+1)) s1 @?= SDF.signal [2,2,2,4,2,4,2,4,4,5,2,5,4,5]
test_comb_2 = SDF.comb22 ((3,4),(2,1),\a b-> ([head a, last b + 1],[last a + 2])) s1 s2 @?=
              (SDF.signal [1,4,3,4,1,6], SDF.signal [3,5,5])
              
test_delay_1 = SDF.delay [5,5,5,5,5] s1 @?= SDF.signal [5,5,5,5,5,1,1,1,3,1,3,1,3,3,4,1,4,3,4,1,6,3,4,3,6]

test_constant = (S.takeS 10 c1, S.takeS 10 c2) @?= (SDF.signal [1,1,2,1,1,2,1,1,2,1,1,2],
                                                    SDF.signal [2,4,2,2,4,2,2,4,2,2,4,2])
  where (c1, c2) = SDF.constant2 ([1,1,2],[2,4,2])

test_generate = (s1, s2) @?= (SDF.signal [1,1,1,3,1,3,1,3,3,4,1,4,3,4,1,6,3,4,3,6],
                              SDF.signal [2,2,2,3,3,3,5,3,5,3,5,5])

test_state = SDF.state11 ((2,3), 2, zipWith (+)) [1,1] s1   @?= SDF.signal [    2,2,5,3,6,6,10,7,13,11,19,14]
test_stated = SDF.stated11 ((2,3), 2, zipWith (+)) [1,1] s1 @?= SDF.signal [1,1,2,2,5,3,6,6,10,7,13,11,19,14]
test_moore = SDF.moore11 ((2,3), 2, zipWith (+)) (2,2,map (+1)) [1,1] s1
                                                            @?= SDF.signal [2,2,3,3,6,4,7,7,11,8,14,12,20,15]
test_mealy = SDF.mealy11 ((2,3), 2, zipWith (+)) ((2,2),2, zipWith (+)) [1,1] s1
                                                            @?= SDF.signal [2,2,3,5,6,6,7,9,13,11,14,15,22,18]

---------------------------

instance (Arbitrary a) => Arbitrary (SDF.SDF [a]) where
  arbitrary = do
    l <- arbitrary
    x <- vector l
    return (SDF.SDF x)
