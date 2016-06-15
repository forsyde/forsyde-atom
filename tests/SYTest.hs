module Main where

import           Test.HUnit
import           Test.QuickCheck
import           ForSyDe.Core
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           ForSyDe.Core.Signal as S
import qualified ForSyDe.MoCLib.SY as SY

instance Arbitrary a => Arbitrary (Signal a) where
  arbitrary = do
    x <- arbitrary
    return (signal x)

instance Arbitrary a => Arbitrary (SY.SY a) where
  arbitrary = do
    x <- arbitrary
    return (SY.SY x)

instance Arbitrary a => Arbitrary (Value a) where
  arbitrary = do
    x <- arbitrary
    return (Value x)

-- Utilities

countEvent _ NullS = 0
countEvent a (x :- xs) | a == x    = 1 + countEvent a xs
                       | otherwise = countEvent a xs


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
        inputAbst    = countEvent (SY.SY Abst)
        outputAbst x = countEvent (SY.SY Abst) (SY.comb11 (+1) x)

prop_comb_psi_2 xs ys = minInAbst xs ys <= outAbst xs ys && maxInAbst xs ys >= outAbst xs ys
  where types = (xs :: SY.Sig Int, ys :: SY.Sig Int)
        minInAbst x y = max (countEvent (SY.SY Abst) x) (countEvent (SY.SY Abst) y)
        maxInAbst x y = (countEvent (SY.SY Abst) x) + (countEvent (SY.SY Abst) y)
        outAbst   x y = countEvent (SY.SY Abst) (SY.comb21 (+) x y)

prop_comb_psi_3 xs = inputAbst xs == outputAbst xs
  where types = (xs :: SY.Sig Int)
        inputAbst    = countEvent (SY.SY Undef)
        outputAbst x = countEvent (SY.SY Undef) (SY.comb11 (+1) x)

prop_comb_psi_4 xs ys = minInAbst xs ys <= outAbst xs ys && maxInAbst xs ys >= outAbst xs ys
  where types = (xs :: SY.Sig Int, ys :: SY.Sig Int)
        minInAbst x y = max (countEvent (SY.SY Undef) x) (countEvent (SY.SY Undef) y)
        maxInAbst x y = (countEvent (SY.SY Undef) x) + (countEvent (SY.SY Undef) y)
        outAbst   x y = countEvent (SY.SY Undef) (SY.comb21 (+) x y)

-- Unit tests

(s1,s2) = (S.takeS 5 k1, S.takeS 7 k2)
  where (k1, k2) = SY.generate2 (\(a,b) -> (a+1,b+2)) (1,2) 

test_comb_1 = SY.comb11 (+1) s1 @?= SY.signal [2,3,4,5,6]
test_comb_2 = SY.comb22 (\a b-> (a+b,a-b)) s1 s2 @?=
              (SY.signal [3,6,9,12,15], SY.signal [-1,-2,-3,-4,-5])
              
test_delay_1 = SY.delay 5 s1 @?= SY.signal [5,1,2,3,4,5]

test_constant = (S.takeS 3 c1, S.takeS 3 c2) @?= (SY.signal [1,1,1], SY.signal [2,2,2])
  where (c1, c2) = SY.constant2 (1,2)

test_generate = (s1, s2) @?= (SY.signal [1,2,3,4,5], SY.signal [2,4,6,8,10,12,14])

test_scanl = SY.scanl11 (+) 1 s1 @?= SY.signal [2,4,7,11,16]

test_scanld = SY.scanld11 (+) 1 s1 @?= SY.signal [1,2,4,7,11,16]

test_moore = SY.moore11 (+) (+1) 1 s1 @?= SY.signal [2,3,5,8,12,17]

test_mealy = SY.mealy11 (+) (-) 1 s1 @?= SY.signal [0,0,1,3,6]

test_when = SY.when p s1 @?= SY.SY <$> S.signal [Abst, Abst, Abst, Value 4, Value 5]
  where p = SY.comb11 (>3) s1

test_filter = SY.filter (>3) s1 @?= SY.SY <$> S.signal [Value 1, Value 2, Value 3, Undef, Undef]

test_fill = SY.fill 5 s @?= SY.SY <$> S.signal [Value 1, Value 2, Value 3, Value 5, Value 5]
  where s = SY.filter (>3) s

test_hold = SY.hold 5 s @?= SY.SY <$> S.signal [Value 1, Value 2, Value 3, Value 3, Value 3]
  where s = SY.filter (>3) s1


-- Test suite

main = defaultMain tests
 
tests = [
  testGroup "SY Tests" [
      testProperty "delay adds an initial token" prop_delay_1,
      testProperty "initial tokens are nullified by merge-combs" prop_delay_comb_1,
      testProperty "comb reacts to inputs only" prop_comb_1,      
      testProperty "comb synchronizes events only when available" prop_comb_2,      
      testProperty "comb(psi) preserves Abst events" prop_comb_psi_1,
      testProperty "comb(psi) merges Abst events" prop_comb_psi_2,       
      testProperty "comb(psi) preserves Undef events" prop_comb_psi_3,
      testProperty "comb(psi) merges Undef events" prop_comb_psi_4,      

      testCase "test comb11" test_comb_1,
      testCase "test comb22" test_comb_2,
      testCase "test delay" test_delay_1,
      testCase "test constant" test_constant,
      testCase "test generate" test_generate,
      testCase "test scanl" test_scanl,
      testCase "test scanld" test_scanld,
      testCase "test moore" test_moore,
      testCase "test mealy" test_mealy,
      testCase "test when" test_when,
      testCase "test filter" test_filter,
      testCase "test fill" test_fill,
      testCase "test hold" test_hold,
      testCase "dummybar" testbar
      ]
  ] 


testfoo = "Foo" @?= "Foo"
testbar = "Foo" @?= "Bar"
