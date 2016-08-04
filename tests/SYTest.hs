{-# LANGUAGE FlexibleInstances #-}
module SYTest where

import           ForSyDe.Atom
import qualified ForSyDe.Atom.MoC.SY as SY
import           ForSyDe.Atom.MoC.SY.Core (part)
import           ForSyDe.Atom.Signal as S
import           Test.HUnit
import           Test.QuickCheck
import           TestUtils

instance (Arbitrary a) => Arbitrary (SY.SY [a]) where
  arbitrary = do
    x <- arbitrary
    return (SY.SY [x])

-- Properties

prop_delay_1 xs = 1 + S.lengthS xs == S.lengthS (SY.delay 1 xs)
  where types = (xs :: SY.Sig Int)

prop_delay_comb_1 xs = S.lengthS xs == S.lengthS (SY.mealy11 (+) (-) 1 xs)
  where types = (xs :: SY.Sig Int)

prop_comb_1 xs = S.lengthS xs == S.lengthS (SY.comb11 (+1) xs)
  where types = (xs :: SY.Sig Int)

prop_comb_2 xs ys = min (S.lengthS xs) (S.lengthS ys) == S.lengthS (SY.comb21 (+) xs ys)
  where types = (xs :: SY.Sig Int, ys :: SY.Sig Int)

prop_comb_psi_1 xs = inputAbst xs == outputAbst xs
  where types = (xs :: SY.Sig Int)
        inputAbst    = countEvent (part Abst)
        outputAbst x = countEvent (part Abst) (SY.comb11 (+1) x)

prop_comb_psi_2 xs ys = minInAbst xs ys <= outAbst xs ys && maxInAbst xs ys >= outAbst xs ys
  where types = (xs :: SY.Sig Int, ys :: SY.Sig Int)
        minInAbst x y = max (countEvent (part Abst) x) (countEvent (part Abst) y)
        maxInAbst x y = (countEvent (part Abst) x) + (countEvent (part Abst) y)
        outAbst   x y = countEvent (part Abst) (SY.comb21 (+) x y)

prop_comb_psi_3 xs = inputAbst xs == outputAbst xs
  where types = (xs :: SY.Sig Int)
        inputAbst    = countEvent (part Undef)
        outputAbst x = countEvent (part Undef) (SY.comb11 (+1) x)

prop_comb_psi_4 xs ys = minInAbst xs ys <= outAbst xs ys && maxInAbst xs ys >= outAbst xs ys
  where types = (xs :: SY.Sig Int, ys :: SY.Sig Int)
        minInAbst x y = max (countEvent (part Undef) x) (countEvent (part Undef) y)
        maxInAbst x y = (countEvent (part Undef) x) + (countEvent (part Undef) y)
        outAbst   x y = countEvent (part Undef) (SY.comb21 (+) x y)

-- Unit tests

(s1,s2) = (S.takeS 5 k1, S.takeS 7 k2)
  where (k1, k2) = SY.generate2 (\a b -> (a+1,b+2)) (1,2) 

test_comb_1 = SY.comb11 (+1) s1 @?= SY.signal [2,3,4,5,6]
test_comb_2 = SY.comb22 (\a b-> (a+b,a-b)) s1 s2 @?=
              (SY.signal [3,6,9,12,15], SY.signal [-1,-2,-3,-4,-5])
              
test_delay_1 = SY.delay 5 s1 @?= SY.signal [5,1,2,3,4,5]

test_constant = (S.takeS 3 c1, S.takeS 3 c2) @?= (SY.signal [1,1,1], SY.signal [2,2,2])
  where (c1, c2) = SY.constant2 (1,2)

test_generate = (s1, s2) @?= (SY.signal [1,2,3,4,5], SY.signal [2,4,6,8,10,12,14])

test_state = SY.state11 (+) 1 s1 @?= SY.signal [2,4,7,11,16]

test_stated = SY.stated11 (+) 1 s1 @?= SY.signal [1,2,4,7,11,16]

test_moore = SY.moore11 (+) (+1) 1 s1 @?= SY.signal [2,3,5,8,12,17]

test_mealy = SY.mealy11 (+) (-) 1 s1 @?= SY.signal [0,0,1,3,6]

-- test_when = SY.when p s1 @?= SY.SY <$> S.signal [Abst, Abst, Abst, Value 4, Value 5]
--   where p = SY.comb11 (>3) s1

-- test_filter = SY.filter (>3) s1 @?= SY.SY <$> S.signal [Value 1, Value 2, Value 3, Undef, Undef]

-- test_fill = SY.fill 5 s @?= SY.SY <$> S.signal [Value 1, Value 2, Value 3, Value 5, Value 5]
--   where s = SY.filter (>3) s1

-- test_hold = SY.hold 5 s @?= SY.SY <$> S.signal [Value 1, Value 2, Value 3, Value 3, Value 3]
--   where s = SY.filter (>3) s1
