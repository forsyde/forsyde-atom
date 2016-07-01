{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SDF.Lib
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a set of helpers for properly instantiating
-- process network patterns as process constructors.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Atom.MoC.SDF.Lib where

import qualified ForSyDe.Atom.MoC.Cons as MoC
import           ForSyDe.Atom.MoC.SDF.Core
import           ForSyDe.Atom.Behavior          hiding (value)
import           ForSyDe.Atom.Utility


delay :: a -> Sig a -> Sig a
delay i = MoC.delay (event i)

comb11 :: Int -> Int
       -> ([a1] -> [b1])
       -> Sig a1 -> Sig b1                                
comb12 :: Int -> (Int, Int)
       -> ([a1] -> ([b1], [b2]))
       -> Sig a1 -> (Sig b1, Sig b2)                   
comb13 :: Int -> (Int, Int, Int)
       -> ([a1] -> ([b1], [b2], [b3]))
       -> Sig a1 -> (Sig b1, Sig b2, Sig b3)           
comb14 :: Int -> (Int, Int, Int, Int)
       -> ([a1] -> ([b1], [b2], [b3], [b3]))
       -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b3)                   

comb21 :: (Int, Int) -> Int
       -> ([a1] -> [a2] -> [b1])
       -> Sig a1 -> Sig a2 -> Sig b1                                
comb22 :: (Int, Int) -> (Int, Int)
       -> ([a1] -> [a2] -> ([b1], [b2]))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                   
comb23 :: (Int, Int) -> (Int, Int, Int)
       -> ([a1] -> [a2] -> ([b1], [b2], [b3]))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)           
comb24 :: (Int, Int) -> (Int, Int, Int, Int)
       -> ([a1] -> [a2] -> ([b1], [b2], [b3], [b3]))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b3)                   

comb31 :: (Int, Int, Int) -> Int
       -> ([a1] -> [a2] -> [a3] -> [b1])
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                                
comb32 :: (Int, Int, Int) -> (Int, Int)
       -> ([a1] -> [a2] -> [a3] -> ([b1], [b2]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)                   
comb33 :: (Int, Int, Int) -> (Int, Int, Int)
       -> ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)           
comb34 :: (Int, Int, Int) -> (Int, Int, Int, Int)
       -> ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b3]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b3)                   

comb41 :: (Int, Int, Int, Int) -> Int
       -> ([a1] -> [a2] -> [a3] -> [a4] -> [b1])
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1                                
comb42 :: (Int, Int, Int, Int) -> (Int, Int)
       -> ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)                   
comb43 :: (Int, Int, Int, Int) -> (Int, Int, Int)
       -> ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)           
comb44 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
       -> ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b3]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b3)                   

comb11 c1 p1           f s1 = spit p1                           $      MoC.comb11 (psi11 f) (eat c1 s1)
comb12 c1 (p1,p2)      f s1 = (spit p1,spit p2)                 <**>   MoC.comb12 (psi12 f) (eat c1 s1)
comb13 c1 (p1,p2,p3)   f s1 = (spit p1,spit p2,spit p3)         <***>  MoC.comb13 (psi13 f) (eat c1 s1)
comb14 c1 (p1,p2,p3,p4) f s1 = (spit p1,spit p2,spit p3,spit p4) <****> MoC.comb14 (psi14 f) (eat c1 s1)

comb21 (c1,c2) p1           f s1 s2 = spit p1                           $      MoC.comb21 (psi21 f) (eat c1 s1) (eat c2 s2)
comb22 (c1,c2) (p1,p2)      f s1 s2 = (spit p1,spit p2)                 <**>   MoC.comb22 (psi22 f) (eat c1 s1) (eat c2 s2)
comb23 (c1,c2) (p1,p2,p3)   f s1 s2 = (spit p1,spit p2,spit p3)         <***>  MoC.comb23 (psi23 f) (eat c1 s1) (eat c2 s2)
comb24 (c1,c2) (p1,p2,p3,p4) f s1 s2 = (spit p1,spit p2,spit p3,spit p4) <****> MoC.comb24 (psi24 f) (eat c1 s1) (eat c2 s2)

comb31 (c1,c2,c3) p1           f s1 s2 s3 = spit p1
  $      MoC.comb31 (psi31 f) (eat c1 s1) (eat c2 s2) (eat c3 s3)
comb32 (c1,c2,c3) (p1,p2)      f s1 s2 s3 = (spit p1,spit p2)
  <**>   MoC.comb32 (psi32 f) (eat c1 s1) (eat c2 s2) (eat c3 s3)
comb33 (c1,c2,c3) (p1,p2,p3)   f s1 s2 s3 = (spit p1,spit p2,spit p3)
  <***>  MoC.comb33 (psi33 f) (eat c1 s1) (eat c2 s2) (eat c3 s3)
comb34 (c1,c2,c3) (p1,p2,p3,p4) f s1 s2 s3 = (spit p1,spit p2,spit p3,spit p4)
  <****> MoC.comb34 (psi34 f) (eat c1 s1) (eat c2 s2) (eat c3 s3)


comb41 (c1,c2,c3,c4) p1           f s1 s2 s3 s4 = spit p1
  $      MoC.comb41 (psi41 f) (eat c1 s1) (eat c2 s2) (eat c3 s3) (eat c4 s4)
comb42 (c1,c2,c3,c4) (p1,p2)      f s1 s2 s3 s4 = (spit p1,spit p2)
  <**>   MoC.comb42 (psi42 f) (eat c1 s1) (eat c2 s2) (eat c3 s3) (eat c4 s4)
comb43 (c1,c2,c3,c4) (p1,p2,p3)   f s1 s2 s3 s4 = (spit p1,spit p2,spit p3)
  <***>  MoC.comb43 (psi43 f) (eat c1 s1) (eat c2 s2) (eat c3 s3) (eat c4 s4)
comb44 (c1,c2,c3,c4) (p1,p2,p3,p4) f s1 s2 s3 s4 = (spit p1,spit p2,spit p3,spit p4)
  <****> MoC.comb44 (psi44 f) (eat c1 s1) (eat c2 s2) (eat c3 s3) (eat c4 s4)
