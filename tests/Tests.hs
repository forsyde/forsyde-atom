module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import ForSyDe.Atom.Behavior (Value(..))
import ForSyDe.Atom.Signal (Signal,signal)

import SYTest as SY
import DETest as DE
import CTTest as CT

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
      testCase "test state"    SY.test_state,
      testCase "test stated"   SY.test_stated,
      testCase "test moore"    SY.test_moore,
      testCase "test mealy"    SY.test_mealy ,
      testCase "test when"     SY.test_when,
      testCase "test filter"   SY.test_filter,
      testCase "test fill"     SY.test_fill,
      testCase "test hold"     SY.test_hold
      ],
  
  testGroup "DE Tests" [
      testProperty "delay adds an initial token"                  DE.prop_delay_1,
      testProperty "delay produces increasing tags"               DE.prop_delay_2,
      testProperty "a state-based process produces an infinite sequence"
                                                                  DE.prop_delay_comb_1,
      testProperty "comb1 does not create additional events"      DE.prop_comb_1,      
      testProperty "comb(>1) might create a (finite) # of events during synch"
                                                                  DE.prop_comb_2,      
      testProperty "comb produces increasing tags"                DE.prop_comb_3,      
      testProperty "comb(psi) preserves Undef values"             DE.prop_comb_psi_1,
      testProperty "comb(psi) merges Undef values"                DE.prop_comb_psi_2,
      testProperty "comb output signals are totally sync'd"       DE.prop_to_sy_1,  
      
      testCase "test comb11"   DE.test_comb_1,
      testCase "test comb22"   DE.test_comb_2,
      testCase "test delay"    DE.test_delay_1,
      testCase "test constant" DE.test_constant,
      testCase "test generate" DE.test_generate,
      testCase "test state"    DE.test_state,
      testCase "test stated"   DE.test_stated,
      testCase "test moore"    DE.test_moore,
      testCase "test mealy"    DE.test_mealy
      ],

    testGroup "CT Tests" [
      testProperty "delay adds an initial token"                  CT.prop_delay_1,
      testProperty "delay produces increasing tags"               CT.prop_delay_2,
      testProperty "a state-based process produces an infinite sequence"
                                                                  CT.prop_delay_comb_1,
      testProperty "comb1 does not create additional events"      CT.prop_comb_1,      
      testProperty "comb(>1) might create a (finite) # of events during synch"
                                                                  CT.prop_comb_2,      
      testProperty "comb produces increasing tags"                CT.prop_comb_3,
      
      testCase "test comb11"   CT.test_comb_1,
      testCase "test comb22"   CT.test_comb_2,
      testCase "test delay"    CT.test_delay_1,
      testCase "test constant" CT.test_constant,
      testCase "test generate" CT.test_generate,
      testCase "test state"    CT.test_state,
      testCase "test stated"   CT.test_stated,
      testCase "test moore"    CT.test_moore,
      testCase "test mealy"    CT.test_mealy
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
