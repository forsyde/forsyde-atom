module TestUtils where


import ForSyDe.Core
import ForSyDe.Core.Signal
import Test.QuickCheck


instance Arbitrary a => Arbitrary (Signal a) where
  arbitrary = do
    x <- arbitrary
    return (signal x)

instance Arbitrary a => Arbitrary (Value a) where
  arbitrary = do
    x <- arbitrary
    return (Value x)


countEvent _ NullS = 0
countEvent a (x :- xs) | a == x    = 1 + countEvent a xs
                       | otherwise = countEvent a xs

