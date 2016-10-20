module VectorTest where

import           Data.Maybe
import           ForSyDe.Atom
import qualified ForSyDe.Atom.MoC.SY as SY
import           ForSyDe.Atom.Signal as S
import           ForSyDe.Atom.Skeleton.Vector as V
import           ForSyDe.Atom.Skeleton.Vector.Lib as VL (takeWhile)
import           Test.HUnit
import           Test.QuickCheck

-- Properties

prop_concat_1 vv = V.reduce1' (+) 0 (V.map11 V.length vv) == V.length (V.concat vv)
  where types = vv :: Vector (Vector Int)

prop_concat_2 n v = (V.concat . V.group (getPositive n)) v == v
  where types = (n :: Positive Integer, v :: Vector Int)

prop_reverse_1 v = V.length v == V.length (V.reverse v)
  where types = v :: Vector Int

prop_get_1 n v = if isNothing e then True else fromJust e == V.last (V.take n v)
  where types = (n :: Integer, v :: Vector Int)
        e     = V.get n v

prop_take_1 pn v = V.length (V.take n v) <= n
  where types = (pn :: Positive Integer, v :: Vector Int)
        n     = getPositive pn

prop_drop_1 pn v = V.length (V.drop n v) >= V.length v - n
  where types = (pn :: Positive Integer, v :: Vector Int)
        n     = getPositive pn

prop_replace_1 n v = V.length (V.replace n 1 v) == V.length v
  where types = (n :: Integer, v :: Vector Int)

prop_stride_1 pn1 pn2 v = V.stride f s v  == V.gather1 (V.iterate l (+s) f) v
  where types = (pn1 :: Positive Integer, pn2 :: Positive Integer, v :: Vector Int)
        f     = getPositive pn1
        s     = getPositive pn2
        l     = ceiling $ (fromIntegral (V.length v - f + 1)) / (fromIntegral s)

prop_shift_1 e v | l > 0 = V.length (V.shiftl v e) == l && V.length (V.shiftr v e) == l
                 | otherwise = True
  where types = (e :: Int, v :: Vector Int)
        l     = V.length v

prop_rot_1 v | l > 0 = (rotl . rotr) v == v 
             | otherwise = True
  where types = (v :: Vector Int)
        l     = V.length v

prop_gather_1 v1 v2 = V.length (gather1 v1 v2) == V.length v1
  where types = (v1 :: Vector Integer, v2 :: Vector Int)

prop_scatter_1 v1 v2 v3 = V.length (scatter v1 v2 v3) == V.length v2
  where types = (v1 :: Vector Integer, v2 :: Vector Int, v2 :: Vector Int)


instance (Arbitrary a) => Arbitrary (Vector a) where
  arbitrary = do
    x <- arbitrary
    return (V.vector x)

