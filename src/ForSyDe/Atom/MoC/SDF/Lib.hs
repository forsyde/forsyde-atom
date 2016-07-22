{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SDF.Lib
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experximental
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


-- GHC doesn't yet support impredicative polymorphism <=> illegal poolymorphic type if not explicit result


delay :: (Nat n) => FSVec n a -> Sig a -> Sig a
delay i = MoC.delay (event i)

comb11 :: (Nat c1, Nat p1)
          => (FSVec c1 a1 -> FSVec p1 b1)
          -> Sig a1 -> Sig b1
comb21 :: (Nat c1, Nat c2, Nat p1)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec p1 b1)
          -> Sig a1 -> Sig a2 -> Sig b1
comb31 :: (Nat c1, Nat c2, Nat c3, Nat p1)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> FSVec p1 b1)
          -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1
comb41 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p1)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> FSVec p1 b1)
          -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1

comb12 :: (Nat c1, Nat p1, Nat p2)
          => (FSVec c1 a1 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sig a1 -> (Sig b1, Sig b2)
comb22 :: (Nat c1, Nat c2, Nat p1, Nat p2)
          => (FSVec c1 a1 -> FSVec c2 a2 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)
comb32 :: (Nat c1, Nat c2, Nat c3, Nat p1, Nat p2)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)
comb42 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> (FSVec p1 b1, FSVec p2 b2))
          -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)

comb13 :: (Nat c1, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sig a1 -> (Sig b1, Sig b2, Sig b3)
comb23 :: (Nat c1, Nat c2, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> FSVec c2 a2 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)
comb33 :: (Nat c1, Nat c2, Nat c3, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)
comb43 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2, Nat p3)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
          -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)

comb14 :: (Nat c1, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)
comb24 :: (Nat c1, Nat c2, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> FSVec c2 a2 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)
comb34 :: (Nat c1, Nat c2, Nat c3, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)
comb44 :: (Nat c1, Nat c2, Nat c3, Nat c4, Nat p1, Nat p2, Nat p3, Nat p4)
          => (FSVec c1 a1 -> FSVec c2 a2 -> FSVec c3 a3 ->  FSVec c4 a4 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
          -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)
   

comb11 f s1          = MoC.comb11 ((isp11 . psi11) f) s1
comb21 f s1 s2       = MoC.comb21 ((isp21 . psi21) f) s1 s2
comb31 f s1 s2 s3    = MoC.comb31 ((isp31 . psi31) f) s1 s2 s3
comb41 f s1 s2 s3 s4 = MoC.comb41 ((isp41 . psi41) f) s1 s2 s3 s4
comb12 f s1          = MoC.comb12 ((isp12 . psi12) f) s1
comb22 f s1 s2       = MoC.comb22 ((isp22 . psi22) f) s1 s2
comb32 f s1 s2 s3    = MoC.comb32 ((isp32 . psi32) f) s1 s2 s3
comb42 f s1 s2 s3 s4 = MoC.comb42 ((isp42 . psi42) f) s1 s2 s3 s4
comb13 f s1          = MoC.comb13 ((isp13 . psi13) f) s1
comb23 f s1 s2       = MoC.comb23 ((isp23 . psi23) f) s1 s2
comb33 f s1 s2 s3    = MoC.comb33 ((isp33 . psi33) f) s1 s2 s3
comb43 f s1 s2 s3 s4 = MoC.comb43 ((isp43 . psi43) f) s1 s2 s3 s4
comb14 f s1          = MoC.comb14 ((isp14 . psi14) f) s1
comb24 f s1 s2       = MoC.comb24 ((isp24 . psi24) f) s1 s2
comb34 f s1 s2 s3    = MoC.comb34 ((isp34 . psi34) f) s1 s2 s3
comb44 f s1 s2 s3 s4 = MoC.comb44 ((isp44 . psi44) f) s1 s2 s3 s4


constant1 :: (Nat n1)
             => FSVec n1 b1 -> Sig b1                                
constant2 :: (Nat n1, Nat n2)
             => (FSVec n1 b1, FSVec n2 b2) -> (Sig b1, Sig b2)                          
constant3 :: (Nat n1, Nat n2, Nat n3)
             => (FSVec n1 b1, FSVec n2 b2, FSVec n3 b3) -> (Sig b1, Sig b2, Sig b3)                      
constant4 :: (Nat n1, Nat n2, Nat n3, Nat n4)
             => (FSVec n1 b1, FSVec n2 b2, FSVec n3 b3, FSVec n4 b4) -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

constant1 i = MoC.scanld01 ((isp11 . psi11) vid1) (event  i)
constant2 i = MoC.scanld02 ((isp22 . psi22) vid2) (event2 i)
constant3 i = MoC.scanld03 ((isp33 . psi33) vid3) (event3 i)
constant4 i = MoC.scanld04 ((isp44 . psi44) vid4) (event4 i)



generate1 :: (Nat n1, Nat c1, Nat p1, n1 :>=: c1)
             => (FSVec c1 b1 -> FSVec p1 b1) -> FSVec n1 b1
             -> Sig b1                                
generate2 :: (Nat n1, Nat c1, Nat p1, n1 :>=: c1,
              Nat n2, Nat c2, Nat p2, n2 :>=: c2)
             => (FSVec c1 b1 -> FSVec c2 b2 -> (FSVec p1 b1, FSVec p2 b2))
             -> (FSVec n1 b1, FSVec n2 b2)
             -> (Sig b1, Sig b2)                          
generate3 :: (Nat n1, Nat c1, Nat p1, n1 :>=: c1,
              Nat n2, Nat c2, Nat p2, n2 :>=: c2,
              Nat n3, Nat c3, Nat p3, n3 :>=: c3)
             => (FSVec c1 b1 -> FSVec c2 b2 -> FSVec c3 b3 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3))
             -> (FSVec n1 b1, FSVec n2 b2, FSVec n3 b3)
             -> (Sig b1, Sig b2, Sig b3)                      
generate4 :: (Nat n1, Nat c1, Nat p1, n1 :>=: c1,
              Nat n2, Nat c2, Nat p2, n2 :>=: c2,
              Nat n3, Nat c3, Nat p3, n3 :>=: c3,
              Nat n4, Nat c4, Nat p4, n4 :>=: c4)
             => (FSVec c1 b1 -> FSVec c2 b2 -> FSVec c3 b3 -> FSVec c4 b4 -> (FSVec p1 b1, FSVec p2 b2, FSVec p3 b3, FSVec p4 b4))
             -> (FSVec n1 b1, FSVec n2 b2, FSVec n3 b3, FSVec n4 b4)
             -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

generate1 ns i = MoC.scanld01 ((isp11 . psi11) ns) (event  i)
generate2 ns i = MoC.scanld02 ((isp22 . psi22) ns) (event2 i)
generate3 ns i = MoC.scanld03 ((isp33 . psi33) ns) (event3 i)
generate4 ns i = MoC.scanld04 ((isp44 . psi44) ns) (event4 i)


-- scanld11 ns i = MoC.scanld11 ((isp21 . psi21) ns) (event i)
-- scanld12 ns i = MoC.scanld12 ((isp21 . psi21) ns) (event i)
-- scanld13 ns i = MoC.scanld13 ((isp21 . psi21) ns) (event i)
-- scanld14 ns i = MoC.scanld14 ((isp21 . psi21) ns) (event i)
-- scanld21 ns i = MoC.scanld21 ((isp31 . psi31) ns) (event i)
-- scanld22 ns i = MoC.scanld22 ((isp31 . psi31) ns) (event i)
-- scanld23 ns i = MoC.scanld23 ((isp31 . psi31) ns) (event i)
-- scanld24 ns i = MoC.scanld24 ((isp31 . psi31) ns) (event i)
-- scanld31 ns i = MoC.scanld31 ((isp41 . psi41) ns) (event i)
-- scanld32 ns i = MoC.scanld32 ((isp41 . psi41) ns) (event i)
-- scanld33 ns i = MoC.scanld33 ((isp41 . psi41) ns) (event i)
-- scanld34 ns i = MoC.scanld34 ((isp41 . psi41) ns) (event i)
-- scanld41 ns i = MoC.scanld41 ((isp51 . psi51) ns) (event i)
-- scanld42 ns i = MoC.scanld42 ((isp51 . psi51) ns) (event i)
-- scanld43 ns i = MoC.scanld43 ((isp51 . psi51) ns) (event i)
-- scanld44 ns i = MoC.scanld44 ((isp51 . psi51) ns) (event i)


-- scanl11 ns i = MoC.scanl11 ((isp21 . psi21) ns) (event i)
-- scanl12 ns i = MoC.scanl12 ((isp21 . psi21) ns) (event i)
-- scanl13 ns i = MoC.scanl13 ((isp21 . psi21) ns) (event i)
-- scanl14 ns i = MoC.scanl14 ((isp21 . psi21) ns) (event i)
-- scanl21 ns i = MoC.scanl21 ((isp31 . psi31) ns) (event i)
-- scanl22 ns i = MoC.scanl22 ((isp31 . psi31) ns) (event i)
-- scanl23 ns i = MoC.scanl23 ((isp31 . psi31) ns) (event i)
-- scanl24 ns i = MoC.scanl24 ((isp31 . psi31) ns) (event i)
-- scanl31 ns i = MoC.scanl31 ((isp41 . psi41) ns) (event i)
-- scanl32 ns i = MoC.scanl32 ((isp41 . psi41) ns) (event i)
-- scanl33 ns i = MoC.scanl33 ((isp41 . psi41) ns) (event i)
-- scanl34 ns i = MoC.scanl34 ((isp41 . psi41) ns) (event i)
-- scanl41 ns i = MoC.scanl41 ((isp51 . psi51) ns) (event i)
-- scanl42 ns i = MoC.scanl42 ((isp51 . psi51) ns) (event i)
-- scanl43 ns i = MoC.scanl43 ((isp51 . psi51) ns) (event i)
-- scanl44 ns i = MoC.scanl44 ((isp51 . psi51) ns) (event i)


-- moore11 ns od i = MoC.moore11 ((isp21 . psi21) ns) ((isp11 . psi11) od) (event i)
-- moore12 ns od i = MoC.moore12 ((isp21 . psi21) ns) ((isp12 . psi12) od) (event i)
-- moore13 ns od i = MoC.moore13 ((isp21 . psi21) ns) ((isp13 . psi13) od) (event i)
-- moore14 ns od i = MoC.moore14 ((isp21 . psi21) ns) ((isp14 . psi14) od) (event i)
-- moore21 ns od i = MoC.moore21 ((isp31 . psi31) ns) ((isp11 . psi11) od) (event i)
-- moore22 ns od i = MoC.moore22 ((isp31 . psi31) ns) ((isp12 . psi12) od) (event i)
-- moore23 ns od i = MoC.moore23 ((isp31 . psi31) ns) ((isp13 . psi13) od) (event i)
-- moore24 ns od i = MoC.moore24 ((isp31 . psi31) ns) ((isp14 . psi14) od) (event i)
-- moore31 ns od i = MoC.moore31 ((isp41 . psi41) ns) ((isp11 . psi11) od) (event i)
-- moore32 ns od i = MoC.moore32 ((isp41 . psi41) ns) ((isp12 . psi12) od) (event i)
-- moore33 ns od i = MoC.moore33 ((isp41 . psi41) ns) ((isp13 . psi13) od) (event i)
-- moore34 ns od i = MoC.moore34 ((isp41 . psi41) ns) ((isp14 . psi14) od) (event i)
-- moore41 ns od i = MoC.moore41 ((isp51 . psi51) ns) ((isp11 . psi11) od) (event i)
-- moore42 ns od i = MoC.moore42 ((isp51 . psi51) ns) ((isp12 . psi12) od) (event i)
-- moore43 ns od i = MoC.moore43 ((isp51 . psi51) ns) ((isp13 . psi13) od) (event i)
-- moore44 ns od i = MoC.moore44 ((isp51 . psi51) ns) ((isp14 . psi14) od) (event i)

-- mealy11 ns od i = MoC.mealy11 ((isp21 . psi21) ns) ((isp21 . psi21) od) (event i)
-- mealy12 ns od i = MoC.mealy12 ((isp21 . psi21) ns) ((isp22 . psi22) od) (event i)
-- mealy13 ns od i = MoC.mealy13 ((isp21 . psi21) ns) ((isp23 . psi23) od) (event i)
-- mealy14 ns od i = MoC.mealy14 ((isp21 . psi21) ns) ((isp24 . psi24) od) (event i)
-- mealy21 ns od i = MoC.mealy21 ((isp31 . psi31) ns) ((isp31 . psi31) od) (event i)
-- mealy22 ns od i = MoC.mealy22 ((isp31 . psi31) ns) ((isp32 . psi32) od) (event i)
-- mealy23 ns od i = MoC.mealy23 ((isp31 . psi31) ns) ((isp33 . psi33) od) (event i)
-- mealy24 ns od i = MoC.mealy24 ((isp31 . psi31) ns) ((isp34 . psi34) od) (event i)
-- mealy31 ns od i = MoC.mealy31 ((isp41 . psi41) ns) ((isp41 . psi41) od) (event i)
-- mealy32 ns od i = MoC.mealy32 ((isp41 . psi41) ns) ((isp42 . psi42) od) (event i)
-- mealy33 ns od i = MoC.mealy33 ((isp41 . psi41) ns) ((isp43 . psi43) od) (event i)
-- mealy34 ns od i = MoC.mealy34 ((isp41 . psi41) ns) ((isp44 . psi44) od) (event i)
-- mealy41 ns od i = MoC.mealy41 ((isp51 . psi51) ns) ((isp51 . psi51) od) (event i)
-- mealy42 ns od i = MoC.mealy42 ((isp51 . psi51) ns) ((isp52 . psi52) od) (event i)
-- mealy43 ns od i = MoC.mealy43 ((isp51 . psi51) ns) ((isp53 . psi53) od) (event i)
-- mealy44 ns od i = MoC.mealy44 ((isp51 . psi51) ns) ((isp54 . psi54) od) (event i)



-- test :: FSVec D1 a -> FSVec D2 a
-- test' :: FSVec D2 a -> (FSVec D1 Int, FSVec D2 a)
-- test'' :: FSVec D3 a -> FSVec D4 a -> FSVec D2 a
-- test'''' :: FSVec D4 a -> FSVec D2 a

-- test x = unsafeVector d2 [Data.Param.FSVec.head x, Data.Param.FSVec.last x]
-- test' x = (unsafeVector d1 [Data.Param.FSVec.length x], unsafeVector d2 [Data.Param.FSVec.head x, Data.Param.FSVec.last x])
-- test'' x y = unsafeVector d2 [Data.Param.FSVec.head x, Data.Param.FSVec.last y]
-- test'''' x = unsafeVector d2 [Data.Param.FSVec.head x, Data.Param.FSVec.last x]

-- i1 = unsafeVector d2 [1,2]
-- s = ForSyDe.Atom.MoC.SDF.Core.signal [1,2,3,4,5,6,7] 
