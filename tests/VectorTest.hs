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


(v1,v2) = V.map12 (\x -> (x+1,x*2)) $ V.iterate 10 (+1) 3 :: (Vector Integer, Vector Integer)
v3      = V.generate 5 id 1
v4      = V.group 3 v1
v5      = V.vector [1,3,5]

test_map_1 = V.map11 (+1) v1 @?= V.vector [5,6,7,8,9,10,11,12,13,14]
test_map_2 = V.map21 (+) v1 v2 @?= V.vector [10,13,16,19,22,25,28,31,34,37]

test_reduce_1 = V.reduce1 (+) v1 @?= 85
test_reduce_2 = V.reduce1' (+) 5 v1 @?= 90

test_prefix_1 = V.prefix1 (+) v1 @?= V.vector [4,9,15,22,30,39,49,60,72,85]
test_prefix_2 = V.prefix1' (+) 5 v1 @?= V.vector [9,14,20,27,35,44,54,65,77,90]

test_suffix_1 = V.suffix1 (+) v1 @?= V.vector [85,81,76,70,63,55,46,36,25,13]
test_suffix_2 = V.suffix1' (+) 5 v1 @?= V.vector [90,86,81,75,68,60,51,41,30,18]

test_pipe_1 = V.pipe0 (fanoutn 5 V.tail) v1 @?= V.vector [9,10,11,12,13]

test_systolic_1 = V.systolic0 (fanoutn 5 V.tail) v1 @?=
  V.vector [V.vector [5,6,7,8,9,10,11,12,13],
            V.vector [6,7,8,9,10,11,12,13],
            V.vector [7,8,9,10,11,12,13],
            V.vector [8,9,10,11,12,13],
            V.vector [9,10,11,12,13]]

test_cascade_1 = V.cascade0 (+) v3 v3 @?= V.vector [6,21,56,126,252]

test_mesh_1 = V.mesh0 (+) v3 v3 @?=
  V.vector [V.vector [2,3,4,5,6],
            V.vector [3,6,10,15,21],
            V.vector [4,10,20,35,56],
            V.vector [5,15,35,70,126],
            V.vector [6,21,56,126,252]]

test_lenght_1    = V.length v1        @?= 10
test_concat_1    = v1 V.<++> v2       @?= V.vector [4,5,6,7,8,9,10,11,12,13,6,8,10,12,14,16,18,20,22,24]
test_concat_2    = V.concat v4        @?= V.vector [4,5,6,7,8,9,10,11,12,13]
test_first_1     = V.first v1         @?= 4 
test_last_1      = V.last v1          @?= 13
test_init_1      = V.init v1          @?= V.vector [4,5,6,7,8,9,10,11,12] 
test_tail_1      = V.tail v1          @?= V.vector [5,6,7,8,9,10,11,12,13]
test_inits_1     = V.inits v3         @?= V.vector [V.vector [1],
                                                    V.vector [1,1],
                                                    V.vector [1,1,1],
                                                    V.vector [1,1,1,1],
                                                    V.vector [1,1,1,1,1]]
test_tails_1     = V.tails v3         @?= V.vector [V.vector [1,1,1,1,1],
                                                    V.vector [1,1,1,1],
                                                    V.vector [1,1,1],
                                                    V.vector [1,1],
                                                    V.vector [1]]
test_reverse_1   = V.reverse v1       @?= V.vector [13,12,11,10,9,8,7,6,5,4]
test_group_1     = V.group 3 v1       @?= V.vector [V.vector [4,5,6],
                                                    V.vector [7,8,9],
                                                    V.vector [10,11,12],
                                                    V.vector [13]]
test_shiftr_1    = V.shiftr v1 1      @?= V.vector [1,4,5,6,7,8,9,10,11,12]
test_shiftl_1    = V.shiftl v1 1      @?= V.vector [5,6,7,8,9,10,11,12,13,1]
test_rotr_1      = V.rotr v1          @?= V.vector [13,4,5,6,7,8,9,10,11,12]
test_rotl_1      = V.rotl v1          @?= V.vector [5,6,7,8,9,10,11,12,13,4]
test_take_1      = V.take 4 v1        @?= V.vector [4,5,6,7]
test_drop_1      = V.drop 4 v1        @?= V.vector [8,9,10,11,12,13]
test_filterIdx_1 = V.filterIdx odd v1 @?= V.vector [4,6,8,10,12] 
test_odds_1      = V.odds v1          @?= V.vector [4,6,8,10,12]
test_evens_1     = V.evens v1         @?= V.vector [5,7,9,11,13]
test_stride_1    = V.stride 2 4 v1    @?= V.vector [5,9,13]
test_get_1       = v1 <@! 4           @?= 7
test_gather_1    = V.gather1 v3 v1    @?= V.vector [4,4,4,4,4]
test_replace_1   = V.replace 4 200 v1 @?= V.vector [4,5,6,200,8,9,10,11,12,13]
test_scatter_1   = V.scatter v5 v3 v1 @?= V.vector [4,1,5,1,6]
test_bitrev_1    = V.bitrev v1        @?= V.vector [11,7,9,13,5,10,6,8,12,4]
test_duals_1     = V.duals v1         @?= (V.vector [4,5,6,7,8], V.vector [9,10,11,12,13])


instance (Arbitrary a) => Arbitrary (Vector a) where
  arbitrary = do
    x <- arbitrary
    return (V.vector x)

