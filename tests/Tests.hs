module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import ForSyDe.Atom.Behavior (Value(..))
import ForSyDe.Atom.Signal (Signal,signal)

import SYTest  as SY
import DETest  as DE
import CTTest  as CT
import SDFTest as SDF
import VectorTest as V


main = defaultMain tests
 
tests = [
  testGroup "SY Tests" [
      testProperty  "delay adds an initial token                  " SY.prop_delay_1,
      testProperty  "initial tokens are nullified by merge-combs  " SY.prop_delay_comb_1,
      testProperty  "comb reacts to inputs only                   " SY.prop_comb_1,      
      testProperty  "comb synchronizes events only when available " SY.prop_comb_2,      
      testProperty  "comb(psi) preserves Abst events              " SY.prop_comb_psi_1,
      testProperty  "comb(psi) merges Abst events                 " SY.prop_comb_psi_2,       
      testProperty  "comb(psi) preserves Undef events             " SY.prop_comb_psi_3,
      testProperty  "comb(psi) merges Undef events                " SY.prop_comb_psi_4,      
      testCase  "test comb11   "                                    SY.test_comb_1,
      testCase  "test comb22   "                                    SY.test_comb_2,
      testCase  "test delay    "                                    SY.test_delay_1,
      testCase  "test constant "                                    SY.test_constant,
      testCase  "test generate "                                    SY.test_generate,
      testCase  "test state    "                                    SY.test_state,
      testCase  "test stated   "                                    SY.test_stated,
      testCase  "test moore    "                                    SY.test_moore,
      testCase  "test mealy    "                                    SY.test_mealy ,
      testCase  "test when     "                                    SY.test_when,
      testCase  "test filter   "                                    SY.test_filter,
      testCase  "test fill     "                                    SY.test_fill,
      testCase  "test hold     "                                    SY.test_hold
      ],
  
  testGroup "DE Tests" [
      testProperty  "delay adds an initial token                               " DE.prop_delay_1,
      testProperty  "delay produces increasing tags                            " DE.prop_delay_2,
      testProperty  "a state-based process produces an infinite sequence       "
                                                                                 DE.prop_delay_comb_1,
      testProperty  "comb1 does not create additional events                   " DE.prop_comb_1,      
      testProperty  "comb(>1) might create a (finite) # of events during synch "
                                                                                 DE.prop_comb_2,      
      testProperty  "comb produces increasing tags                             " DE.prop_comb_3,      
      testProperty  "comb(psi) preserves Undef values                          " DE.prop_comb_psi_1,
      testProperty  "comb(psi) merges Undef values                             " DE.prop_comb_psi_2,
      testProperty  "comb output signals are totally sync'd                    " DE.prop_to_sy_1,   
      testCase  "test comb11   "                                                 DE.test_comb_1,
      testCase  "test comb22   "                                                 DE.test_comb_2,
      testCase  "test delay    "                                                 DE.test_delay_1,
      testCase  "test constant "                                                 DE.test_constant,
      testCase  "test generate "                                                 DE.test_generate,
      testCase  "test state    "                                                 DE.test_state,
      testCase  "test stated   "                                                 DE.test_stated,
      testCase  "test moore    "                                                 DE.test_moore,
      testCase  "test mealy    "                                                 DE.test_mealy
      ],

  testGroup "CT Tests" [
      testProperty  "delay adds an initial token                               " CT.prop_delay_1,
      testProperty  "delay produces increasing tags                            " CT.prop_delay_2,
      testProperty  "a state-based process produces an infinite sequence       " CT.prop_delay_comb_1,
      testProperty  "comb1 does not create additional events                   " CT.prop_comb_1,      
      testProperty  "comb(>1) might create a (finite) # of events during synch " CT.prop_comb_2,      
      testProperty  "comb produces increasing tags                             " CT.prop_comb_3,
      testCase  "test comb11   "                                                 CT.test_comb_1,
      testCase  "test comb22   "                                                 CT.test_comb_2,
      testCase  "test delay    "                                                 CT.test_delay_1,
      testCase  "test constant "                                                 CT.test_constant,
      testCase  "test generate "                                                 CT.test_generate,
      testCase  "test state    "                                                 CT.test_state,
      testCase  "test stated   "                                                 CT.test_stated,
      testCase  "test moore    "                                                 CT.test_moore,
      testCase  "test mealy    "                                                 CT.test_mealy
      ],
  
  testGroup "SDF Tests" [
      testProperty "the production/consumption rates hold for mealy" SDF.prop_prod_1,
      testProperty "the production/consumption rates hold for comb1" SDF.prop_prod_2,
      testProperty "the production/consumption rates hold for comb2" SDF.prop_prod_3,     
      testCase  "test comb11   " SDF.test_comb_1,
      testCase  "test comb22   " SDF.test_comb_2,
      testCase  "test delay    " SDF.test_delay_1,
      testCase  "test constant " SDF.test_constant,
      testCase  "test generate " SDF.test_generate,
      testCase  "test state    " SDF.test_state,
      testCase  "test stated   " SDF.test_stated,
      testCase  "test moore    " SDF.test_moore,
      testCase  "test mealy    " SDF.test_mealy 
      ],
  
  
  testGroup "SDF Tests" [
      testProperty "L of concatenation is equal to the sum of L components" V.prop_concat_1,
      testProperty "concat . group = id                                   " V.prop_concat_2,
      testProperty "reverse does not modify the length of a vector        " V.prop_reverse_1,
      testProperty "get n === last . take n                               " V.prop_get_1,
      testProperty "L (take n v) <= n                                     " V.prop_take_1,
      testProperty "L (drop n v) >= L (v) - n                             " V.prop_drop_1,
      testProperty "replace does not change L                             " V.prop_replace_1,
      testProperty "stride can be rewritten as gather (===)               " V.prop_stride_1,
      testProperty "shift does not change L                               " V.prop_shift_1,
      testProperty "rotr . rotl = id                                      " V.prop_rot_1,
      testProperty "gather returns as many elements as the index vector   " V.prop_gather_1,
      testProperty "scatter does not modify the host L                    " V.prop_scatter_1,
      testCase  "test map 1       " V.test_map_1,
      testCase  "test map 2       " V.test_map_2,
      testCase  "test reduce 1    " V.test_reduce_1,
      testCase  "test reduce 2    " V.test_reduce_2,
      testCase  "test prefix 1    " V.test_prefix_1,
      testCase  "test prefix 2    " V.test_prefix_2,
      testCase  "test suffix 1    " V.test_suffix_1,
      testCase  "test suffix 2    " V.test_suffix_2,
      testCase  "test pipe 1      " V.test_pipe_1,
      testCase  "test systolic 2  " V.test_systolic_1,
      testCase  "test cascade_1   " V.test_cascade_1,
      testCase  "test mesh_1      " V.test_mesh_1,
      testCase  "test lenght_1    " V.test_lenght_1,
      testCase  "test concat_1    " V.test_concat_1,
      testCase  "test concat_2    " V.test_concat_2,
      testCase  "test first_1     " V.test_first_1,
      testCase  "test last_1      " V.test_last_1,
      testCase  "test init_1      " V.test_init_1,
      testCase  "test tail_1      " V.test_tail_1,
      testCase  "test inits_1     " V.test_inits_1,
      testCase  "test tails_1     " V.test_tails_1,
      testCase  "test reverse_1   " V.test_reverse_1,
      testCase  "test group_1     " V.test_group_1,
      testCase  "test shiftr_1    " V.test_shiftr_1,
      testCase  "test shiftl_1    " V.test_shiftl_1,
      testCase  "test rotr_1      " V.test_rotr_1,
      testCase  "test rotl_1      " V.test_rotl_1,
      testCase  "test take_1      " V.test_take_1,
      testCase  "test drop_1      " V.test_drop_1,
      testCase  "test filterIdx_1 " V.test_filterIdx_1,
      testCase  "test odds_1      " V.test_odds_1,
      testCase  "test evens_1     " V.test_evens_1,
      testCase  "test stride_1    " V.test_stride_1,
      testCase  "test get_1       " V.test_get_1,
      testCase  "test gather_1    " V.test_gather_1,
      testCase  "test replace_1   " V.test_replace_1,
      testCase  "test scatter_1   " V.test_scatter_1,
      testCase  "test bitrev_1    " V.test_bitrev_1,
      testCase  "test duals_1     " V.test_duals_1
      ]

  ] 


instance Arbitrary a => Arbitrary (Signal a) where
  arbitrary = do
    x <- arbitrary
    return (signal x)

instance Arbitrary a => Arbitrary (Value a) where
  arbitrary = do
    x <- arbitrary
    return (Value x)
  
