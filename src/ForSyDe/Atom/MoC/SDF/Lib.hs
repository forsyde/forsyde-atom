{-# LANGUAGE TypeFamilies, RankNTypes #-}
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

import           Data.Param.FSVec
import           Data.TypeLevel
import           Prelude hiding (filter, id)

import           ForSyDe.Atom.Behavior hiding (value)
import qualified ForSyDe.Atom.MoC.Cons as MoC
import           ForSyDe.Atom.MoC.SDF.Core
import           ForSyDe.Atom.Utility


-- GHC doesn't yet support impredicative polymorphism <=> illegal poolymorphic type if not explicit Sigr result


delay :: a -> Sig a -> Sig a
delay i = MoC.delay (event i)

comb11 :: (Nat i, Nat c1, Nat p1)
          => (FSVec c1 a1 -> FSVec p1 b1)
          -> Sigr i a1 -> Sigr p1 b1
comb21 :: (Nat i, Nat c1, Nat c2, Nat p1)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec p1 b1)
          -> Sigr i a1 -> Sig a2 -> Sigr p1 b1
comb31 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat p1)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> FSVec p1 b1)
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> Sigr p1 b1
comb41 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat c4, Nat p1)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> FSVec p1 b1)
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sigr p1 b1

comb12 :: (Nat i, Nat c1, Nat p1, Nat p2)
          => (FSVec c1 a1 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sigr i a1 -> (Sigr p1 b1, Sigr p2 b2)
comb22 :: (Nat i, Nat c1, Nat c2, Nat p1, Nat p2)
          => (FSVec c1 a1 -> FSVec c2 a2 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sigr i a1 -> Sig a2 -> (Sigr p1 b1, Sigr p2 b2)
comb32 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat p1, Nat p2)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> (Sigr p1 b1, Sigr p2 b2)
comb42 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sigr p1 b1, Sigr p2 b2)

comb13 :: (Nat i, Nat c1, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sigr i a1 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3)
comb23 :: (Nat i, Nat c1, Nat c2, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> FSVec c2 a2 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sigr i a1 -> Sig a2 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3)
comb33 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3)
comb43 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3)

comb14 :: (Nat i, Nat c1, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sigr i a1 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3, Sigr p4 b4)
comb24 :: (Nat i, Nat c1, Nat c2, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> FSVec c2 a2 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sigr i a1 -> Sig a2 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3, Sigr p4 b4)
comb34 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3, Sigr p4 b4)
comb44 :: (Nat i, Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sigr i a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sigr p1 b1, Sigr p2 b2, Sigr p3 b3, Sigr p4 b4)
   

comb11 f s1          = cat $ MoC.comb11 (psi11 f) s1
comb21 f s1 s2       = cat $ MoC.comb21 (psi21 f) s1 s2
comb31 f s1 s2 s3    = cat $ MoC.comb31 (psi31 f) s1 s2 s3
comb41 f s1 s2 s3 s4 = cat $ MoC.comb41 (psi41 f) s1 s2 s3 s4

comb12 f s1          = (cat,cat) <**> MoC.comb12 (psi12 f) s1
comb22 f s1 s2       = (cat,cat) <**> MoC.comb22 (psi22 f) s1 s2
comb32 f s1 s2 s3    = (cat,cat) <**> MoC.comb32 (psi32 f) s1 s2 s3
comb42 f s1 s2 s3 s4 = (cat,cat) <**> MoC.comb42 (psi42 f) s1 s2 s3 s4

comb13 f s1          = (cat,cat,cat) <***> MoC.comb13 (psi13 f) s1
comb23 f s1 s2       = (cat,cat,cat) <***> MoC.comb23 (psi23 f) s1 s2
comb33 f s1 s2 s3    = (cat,cat,cat) <***> MoC.comb33 (psi33 f) s1 s2 s3
comb43 f s1 s2 s3 s4 = (cat,cat,cat) <***> MoC.comb43 (psi43 f) s1 s2 s3 s4

comb14 f s1          = (cat,cat,cat,cat) <****> MoC.comb14 (psi14 f) s1
comb24 f s1 s2       = (cat,cat,cat,cat) <****> MoC.comb24 (psi24 f) s1 s2
comb34 f s1 s2 s3    = (cat,cat,cat,cat) <****> MoC.comb34 (psi34 f) s1 s2 s3
comb44 f s1 s2 s3 s4 = (cat,cat,cat,cat) <****> MoC.comb44 (psi44 f) s1 s2 s3 s4


-- constant1 :: b1 -> Sigr D1 b1                                
-- constant2 :: (b1, b2) -> (Sig b1, Sig b2)                          
-- constant3 :: (b1, b2, b3) -> (Sig b1, Sig b2, Sig b3)                      
-- constant4 :: (b1, b2, b3, b4) -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

id :: (Nat r) => FSVec r a -> FSVec r a
id a = a

-- constant1 i = cat $ MoC.scanld01 (psi11 id) (event i)
-- constant2 i = MoC.scanld02 (psi11 id) (event i)
-- constant3 i = MoC.scanld03 (psi11 id) (event i)
-- constant4 i = MoC.scanld04 (psi11 id) (event i)


-- generate1 :: (b1 -> b1) -> b1
--           -> Sig b1                                
-- generate2 :: ((b1, b2) -> (b1, b2)) -> (b1, b2)
--           -> (Sig b1, Sig b2)                          
-- generate3 :: ((b1, b2, b3) -> (b1, b2, b3)) -> (b1, b2, b3)
--           -> (Sig b1, Sig b2, Sig b3)                      
-- generate4 :: ((b1, b2, b3, b4) -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--           -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

-- generate1 ns i = MoC.scanld01 (psi11 ns) (event i)
-- generate2 ns i = MoC.scanld02 (psi11 ns) (event i)
-- generate3 ns i = MoC.scanld03 (psi11 ns) (event i)
-- generate4 ns i = MoC.scanld04 (psi11 ns) (event i)


-- scanld11 :: (b1 -> a1 -> b1) -> b1
--         -> Sig a1 -> Sig b1                                
-- scanld12 :: ((b1, b2) -> a1 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> (Sig b1, Sig b2)                          
-- scanld13 :: ((b1, b2, b3) -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
-- scanld14 :: ((b1, b2, b3, b4) -> a1 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
-- scanld21 :: (b1 -> a1 -> a2 -> b1) -> b1
--         -> Sig a1 -> Sig a2 -> Sig b1                          
-- scanld22 :: ((b1, b2) -> a1 -> a2 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
-- scanld23 :: ((b1, b2, b3) -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
-- scanld24 :: ((b1, b2, b3, b4) -> a1 -> a2 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
-- scanld31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> b1
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
-- scanld32 :: ((b1, b2) -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
-- scanld33 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
-- scanld34 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
-- scanld41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> b1
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
-- scanld42 :: ((b1, b2) -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
-- scanld43 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
-- scanld44 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

-- scanld11 ns i = MoC.scanld11 (psi21 ns) (event i)
-- scanld12 ns i = MoC.scanld12 (psi21 ns) (event i)
-- scanld13 ns i = MoC.scanld13 (psi21 ns) (event i)
-- scanld14 ns i = MoC.scanld14 (psi21 ns) (event i)
-- scanld21 ns i = MoC.scanld21 (psi31 ns) (event i)
-- scanld22 ns i = MoC.scanld22 (psi31 ns) (event i)
-- scanld23 ns i = MoC.scanld23 (psi31 ns) (event i)
-- scanld24 ns i = MoC.scanld24 (psi31 ns) (event i)
-- scanld31 ns i = MoC.scanld31 (psi41 ns) (event i)
-- scanld32 ns i = MoC.scanld32 (psi41 ns) (event i)
-- scanld33 ns i = MoC.scanld33 (psi41 ns) (event i)
-- scanld34 ns i = MoC.scanld34 (psi41 ns) (event i)
-- scanld41 ns i = MoC.scanld41 (psi51 ns) (event i)
-- scanld42 ns i = MoC.scanld42 (psi51 ns) (event i)
-- scanld43 ns i = MoC.scanld43 (psi51 ns) (event i)
-- scanld44 ns i = MoC.scanld44 (psi51 ns) (event i)


-- scanl11 :: (b1 -> a1 -> b1) -> b1
--         -> Sig a1 -> Sig b1                                
-- scanl12 :: ((b1, b2) -> a1 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> (Sig b1, Sig b2)                          
-- scanl13 :: ((b1, b2, b3) -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
-- scanl14 :: ((b1, b2, b3, b4) -> a1 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
-- scanl21 :: (b1 -> a1 -> a2 -> b1) -> b1
--         -> Sig a1 -> Sig a2 -> Sig b1                          
-- scanl22 :: ((b1, b2) -> a1 -> a2 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
-- scanl23 :: ((b1, b2, b3) -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
-- scanl24 :: ((b1, b2, b3, b4) -> a1 -> a2 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
-- scanl31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> b1
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
-- scanl32 :: ((b1, b2) -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
-- scanl33 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
-- scanl34 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
-- scanl41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> b1
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
-- scanl42 :: ((b1, b2) -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
-- scanl43 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
-- scanl44 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

-- scanl11 ns i = MoC.scanl11 (psi21 ns) (event i)
-- scanl12 ns i = MoC.scanl12 (psi21 ns) (event i)
-- scanl13 ns i = MoC.scanl13 (psi21 ns) (event i)
-- scanl14 ns i = MoC.scanl14 (psi21 ns) (event i)
-- scanl21 ns i = MoC.scanl21 (psi31 ns) (event i)
-- scanl22 ns i = MoC.scanl22 (psi31 ns) (event i)
-- scanl23 ns i = MoC.scanl23 (psi31 ns) (event i)
-- scanl24 ns i = MoC.scanl24 (psi31 ns) (event i)
-- scanl31 ns i = MoC.scanl31 (psi41 ns) (event i)
-- scanl32 ns i = MoC.scanl32 (psi41 ns) (event i)
-- scanl33 ns i = MoC.scanl33 (psi41 ns) (event i)
-- scanl34 ns i = MoC.scanl34 (psi41 ns) (event i)
-- scanl41 ns i = MoC.scanl41 (psi51 ns) (event i)
-- scanl42 ns i = MoC.scanl42 (psi51 ns) (event i)
-- scanl43 ns i = MoC.scanl43 (psi51 ns) (event i)
-- scanl44 ns i = MoC.scanl44 (psi51 ns) (event i)


-- moore11 :: (st -> a1 -> st) -> (st -> b1) -> st
--         -> Sig a1 -> Sig b1                                
-- moore12 :: (st -> a1 -> st) -> (st -> (b1, b2)) -> st
--         -> Sig a1 -> (Sig b1, Sig b2)                          
-- moore13 :: (st -> a1 -> st) -> (st -> (b1, b2, b3)) -> st
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
-- moore14 :: (st -> a1 -> st) -> (st -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
-- moore21 :: (st -> a1 -> a2 -> st) -> (st -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig b1                          
-- moore22 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2)) -> st
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
-- moore23 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3)) -> st
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
-- moore24 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
-- moore31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
-- moore32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
-- moore33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
-- moore34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
-- moore41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
-- moore42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
-- moore43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
-- moore44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

-- moore11 ns od i = MoC.moore11 (psi21 ns) (psi11 od) (event i)
-- moore12 ns od i = MoC.moore12 (psi21 ns) (psi12 od) (event i)
-- moore13 ns od i = MoC.moore13 (psi21 ns) (psi13 od) (event i)
-- moore14 ns od i = MoC.moore14 (psi21 ns) (psi14 od) (event i)
-- moore21 ns od i = MoC.moore21 (psi31 ns) (psi11 od) (event i)
-- moore22 ns od i = MoC.moore22 (psi31 ns) (psi12 od) (event i)
-- moore23 ns od i = MoC.moore23 (psi31 ns) (psi13 od) (event i)
-- moore24 ns od i = MoC.moore24 (psi31 ns) (psi14 od) (event i)
-- moore31 ns od i = MoC.moore31 (psi41 ns) (psi11 od) (event i)
-- moore32 ns od i = MoC.moore32 (psi41 ns) (psi12 od) (event i)
-- moore33 ns od i = MoC.moore33 (psi41 ns) (psi13 od) (event i)
-- moore34 ns od i = MoC.moore34 (psi41 ns) (psi14 od) (event i)
-- moore41 ns od i = MoC.moore41 (psi51 ns) (psi11 od) (event i)
-- moore42 ns od i = MoC.moore42 (psi51 ns) (psi12 od) (event i)
-- moore43 ns od i = MoC.moore43 (psi51 ns) (psi13 od) (event i)
-- moore44 ns od i = MoC.moore44 (psi51 ns) (psi14 od) (event i)


-- mealy11 :: (st -> a1 -> st) -> (st -> a1 -> b1) -> st
--         -> Sig a1 -> Sig b1                                
-- mealy12 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2)) -> st
--         -> Sig a1 -> (Sig b1, Sig b2)                          
-- mealy13 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3)) -> st
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
-- mealy14 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
-- mealy21 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig b1                          
-- mealy22 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2)) -> st
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
-- mealy23 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3)) -> st
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
-- mealy24 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
-- mealy31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
-- mealy32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
-- mealy33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
-- mealy34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
-- mealy41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
-- mealy42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
-- mealy43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
-- mealy44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

-- mealy11 ns od i = MoC.mealy11 (psi21 ns) (psi21 od) (event i)
-- mealy12 ns od i = MoC.mealy12 (psi21 ns) (psi22 od) (event i)
-- mealy13 ns od i = MoC.mealy13 (psi21 ns) (psi23 od) (event i)
-- mealy14 ns od i = MoC.mealy14 (psi21 ns) (psi24 od) (event i)
-- mealy21 ns od i = MoC.mealy21 (psi31 ns) (psi31 od) (event i)
-- mealy22 ns od i = MoC.mealy22 (psi31 ns) (psi32 od) (event i)
-- mealy23 ns od i = MoC.mealy23 (psi31 ns) (psi33 od) (event i)
-- mealy24 ns od i = MoC.mealy24 (psi31 ns) (psi34 od) (event i)
-- mealy31 ns od i = MoC.mealy31 (psi41 ns) (psi41 od) (event i)
-- mealy32 ns od i = MoC.mealy32 (psi41 ns) (psi42 od) (event i)
-- mealy33 ns od i = MoC.mealy33 (psi41 ns) (psi43 od) (event i)
-- mealy34 ns od i = MoC.mealy34 (psi41 ns) (psi44 od) (event i)
-- mealy41 ns od i = MoC.mealy41 (psi51 ns) (psi51 od) (event i)
-- mealy42 ns od i = MoC.mealy42 (psi51 ns) (psi52 od) (event i)
-- mealy43 ns od i = MoC.mealy43 (psi51 ns) (psi53 od) (event i)
-- mealy44 ns od i = MoC.mealy44 (psi51 ns) (psi54 od) (event i)
