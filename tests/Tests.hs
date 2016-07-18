module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import SYTest as SY


main = defaultMain tests
 
tests = [
  testGroup "SY Tests" [
      testProperty "delay adds an initial token"                  SY.prop_delay_1,
      testProperty "initial tokens are nullified by merge-combs"  SY.prop_delay_comb_1,
      testProperty "comb reacts to inputs only"                   SY.prop_comb_1,      
      testProperty "comb synchronizes events only when available" SY.prop_comb_2,      
      testProperty "comb(psi) preserves Abst events"              SY.prop_comb_psi_1,
      testProperty "comb(psi) merges Abst events"                 SY.prop_comb_psi_2,       
      testProperty "comb(psi) preserves Undef events"             SY.prop_comb_psi_3,
      testProperty "comb(psi) merges Undef events"                SY.prop_comb_psi_4,      
      
      -- testCase "dummybar" testbar
      testCase "test comb11"   SY.test_comb_1,
      testCase "test comb22"   SY.test_comb_2,
      testCase "test delay"    SY.test_delay_1,
      testCase "test constant" SY.test_constant,
      testCase "test generate" SY.test_generate,
      testCase "test scanl"    SY.test_scanl,
      testCase "test scanld"   SY.test_scanld,
      testCase "test moore"    SY.test_moore,
      testCase "test mealy"    SY.test_mealy-- ,
      -- testCase "test when"     SY.test_when,
      -- testCase "test filter"   SY.test_filter,
      -- testCase "test fill"     SY.test_fill,
      -- testCase "test hold"     SY.test_hold
      ]
  ] 


-- testfoo = "Foo" @?= "Foo"
-- testbar = "Foo" @?= "Bar"
