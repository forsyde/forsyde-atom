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

import           ForSyDe.Atom.Behavior hiding (value)
import qualified ForSyDe.Atom.MoC.AtomLib as MoC
import           ForSyDe.Atom.MoC.SDF.Core
import           ForSyDe.Atom.Utility


-- GHC doesn't yet support impredicative polymorphism <=> illegal poolymorphic type if not explicit result

type Prod = Int
type Cons = Int

------- DELAY -------

delay :: [a] -> Sig a -> Sig a
delay i = MoC.delay (part i)

------- COMB -------

comb11 :: (Cons, Prod,
           [a1] -> [b1])
       -> Sig a1 -> Sig b1
comb21 :: ((Cons,Cons), Prod,
           [a1] -> [a2] -> [b1])
       -> Sig a1 -> Sig a2 -> Sig b1
comb31 :: ((Cons,Cons,Cons), Prod,
           [a1] -> [a2] -> [a3] -> [b1])
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1
comb41 :: ((Cons,Cons,Cons,Cons), Prod,
           [a1] -> [a2] -> [a3] -> [a4] -> [b1])
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1

comb12 :: (Cons, (Prod,Prod),
           [a1] -> ([b1], [b2]))
       -> Sig a1 -> (Sig b1, Sig b2)
comb22 :: ((Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2]))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)
comb32 :: ((Cons,Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)
comb42 :: ((Cons,Cons,Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)

comb13 :: (Cons, (Prod,Prod,Prod),
           [a1] -> ([b1], [b2], [b3]))
       -> Sig a1 -> (Sig b1, Sig b2, Sig b3)
comb23 :: ((Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2], [b3]))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)
comb33 :: ((Cons,Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)
comb43 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)

comb14 :: (Cons, (Prod,Prod,Prod,Prod),
           [a1] -> ([b1], [b2], [b3], [b4]))
       -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)
comb24 :: ((Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)
comb34 :: ((Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)
comb44 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3], [b4]))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)
   

comb11 (c,p,f) s1          = MoC.comb11 (wrap11 c p (psi11 f)) s1
comb21 (c,p,f) s1 s2       = MoC.comb21 (wrap21 c p (psi21 f)) s1 s2
comb31 (c,p,f) s1 s2 s3    = MoC.comb31 (wrap31 c p (psi31 f)) s1 s2 s3
comb41 (c,p,f) s1 s2 s3 s4 = MoC.comb41 (wrap41 c p (psi41 f)) s1 s2 s3 s4
comb12 (c,p,f) s1          = MoC.comb12 (wrap12 c p (psi12 f)) s1
comb22 (c,p,f) s1 s2       = MoC.comb22 (wrap22 c p (psi22 f)) s1 s2
comb32 (c,p,f) s1 s2 s3    = MoC.comb32 (wrap32 c p (psi32 f)) s1 s2 s3
comb42 (c,p,f) s1 s2 s3 s4 = MoC.comb42 (wrap42 c p (psi42 f)) s1 s2 s3 s4
comb13 (c,p,f) s1          = MoC.comb13 (wrap13 c p (psi13 f)) s1
comb23 (c,p,f) s1 s2       = MoC.comb23 (wrap23 c p (psi23 f)) s1 s2
comb33 (c,p,f) s1 s2 s3    = MoC.comb33 (wrap33 c p (psi33 f)) s1 s2 s3
comb43 (c,p,f) s1 s2 s3 s4 = MoC.comb43 (wrap43 c p (psi43 f)) s1 s2 s3 s4
comb14 (c,p,f) s1          = MoC.comb14 (wrap14 c p (psi14 f)) s1
comb24 (c,p,f) s1 s2       = MoC.comb24 (wrap24 c p (psi24 f)) s1 s2
comb34 (c,p,f) s1 s2 s3    = MoC.comb34 (wrap34 c p (psi34 f)) s1 s2 s3
comb44 (c,p,f) s1 s2 s3 s4 = MoC.comb44 (wrap44 c p (psi44 f)) s1 s2 s3 s4

------- CONSTANT -------

constant1 :: ([b1])                   -> (Sig b1)                                
constant2 :: ([b1], [b2])             -> (Sig b1, Sig b2)                          
constant3 :: ([b1], [b2], [b3])       -> (Sig b1, Sig b2, Sig b3)                      
constant4 :: ([b1], [b2], [b3], [b4]) -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

constant1 i = MoC.stated01 (wrap11  1         1        (psi11 id1)) (part  i)
constant2 i = MoC.stated02 (wrap22 (1,1)     (1,1)     (psi22 id2)) (part2 i)
constant3 i = MoC.stated03 (wrap33 (1,1,1)   (1,1,1)   (psi33 id3)) (part3 i)
constant4 i = MoC.stated04 (wrap44 (1,1,1,1) (1,1,1,1) (psi44 id4)) (part4 i)

------- GENERATE -------

generate1 :: (Cons, Prod,
              [b1] -> [b1])
          -> [b1] -> Sig b1                                
generate2 :: ((Cons,Cons), (Prod,Prod),
              [b1] -> [b2] -> ([b1], [b2]))
          -> ([b1], [b2]) -> (Sig b1, Sig b2)                          
generate3 :: ((Cons,Cons,Cons), (Prod,Prod,Prod),
              [b1] -> [b2] -> [b3] -> ([b1], [b2], [b3]))
          -> ([b1], [b2], [b3]) -> (Sig b1, Sig b2, Sig b3)                      
generate4 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
              [b1] -> [b2] -> [b3] -> [b4] -> ([b1], [b2], [b3], [b4]))
          -> ([b1], [b2], [b3], [b4]) -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

generate1 (c,p,f) i = MoC.stated01 (wrap11 c p (psi11 f)) (part  i)
generate2 (c,p,f) i = MoC.stated02 (wrap22 c p (psi22 f)) (part2 i)
generate3 (c,p,f) i = MoC.stated03 (wrap33 c p (psi33 f)) (part3 i)
generate4 (c,p,f) i = MoC.stated04 (wrap44 c p (psi44 f)) (part4 i)


------- STATE -------

state11 :: ((Cons,Cons), Prod,
             [b1] -> [a1] -> [b1])
         ->  [b1] -> Sig a1 -> Sig b1
state12 :: ((Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> (Sig b1, Sig b2)
state13 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> (Sig b1, Sig b2, Sig b3)
state14 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)

state21 :: ((Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [b1])
         ->  [b1] -> Sig a1 -> Sig a2 -> Sig b1
state22 :: ((Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)
state23 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)
state24 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)

state31 :: ((Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [b1])
         ->  [b1] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1
state32 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)
state33 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)
state34 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)

state41 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [a4] -> [b1])
         ->  [b1] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1
state42 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)
state43 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)
state44 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
state11 (cns,pns,ns) i = MoC.state11 (wrap21 cns pns (psi21 ns)) (part  i)
state12 (cns,pns,ns) i = MoC.state12 (wrap32 cns pns (psi32 ns)) (part2 i)
state13 (cns,pns,ns) i = MoC.state13 (wrap43 cns pns (psi43 ns)) (part3 i)
state14 (cns,pns,ns) i = MoC.state14 (wrap54 cns pns (psi54 ns)) (part4 i)
state21 (cns,pns,ns) i = MoC.state21 (wrap31 cns pns (psi31 ns)) (part  i)
state22 (cns,pns,ns) i = MoC.state22 (wrap42 cns pns (psi42 ns)) (part2 i)
state23 (cns,pns,ns) i = MoC.state23 (wrap53 cns pns (psi53 ns)) (part3 i)
state24 (cns,pns,ns) i = MoC.state24 (wrap64 cns pns (psi64 ns)) (part4 i)
state31 (cns,pns,ns) i = MoC.state31 (wrap41 cns pns (psi41 ns)) (part  i)
state32 (cns,pns,ns) i = MoC.state32 (wrap52 cns pns (psi52 ns)) (part2 i)
state33 (cns,pns,ns) i = MoC.state33 (wrap63 cns pns (psi63 ns)) (part3 i)
state34 (cns,pns,ns) i = MoC.state34 (wrap74 cns pns (psi74 ns)) (part4 i)
state41 (cns,pns,ns) i = MoC.state41 (wrap51 cns pns (psi51 ns)) (part  i)
state42 (cns,pns,ns) i = MoC.state42 (wrap62 cns pns (psi62 ns)) (part2 i)
state43 (cns,pns,ns) i = MoC.state43 (wrap73 cns pns (psi73 ns)) (part3 i)
state44 (cns,pns,ns) i = MoC.state44 (wrap84 cns pns (psi84 ns)) (part4 i)

------- STATED -------

stated11 :: ((Cons,Cons), Prod,
             [b1] -> [a1] -> [b1])
         ->  [b1] -> Sig a1 -> Sig b1
stated12 :: ((Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> (Sig b1, Sig b2)
stated13 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> (Sig b1, Sig b2, Sig b3)
stated14 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)

stated21 :: ((Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [b1])
         ->  [b1] -> Sig a1 -> Sig a2 -> Sig b1
stated22 :: ((Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)
stated23 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)
stated24 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)

stated31 :: ((Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [b1])
         ->  [b1] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1
stated32 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)
stated33 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)
stated34 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)

stated41 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [a4] -> [b1])
         ->  [b1] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1
stated42 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
         -> ([b1], [b2]) -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)
stated43 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3]) -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)
stated44 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4]) -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
stated11 (cns,pns,ns) i = MoC.stated11 (wrap21 cns pns (psi21 ns)) (part  i)
stated12 (cns,pns,ns) i = MoC.stated12 (wrap32 cns pns (psi32 ns)) (part2 i)
stated13 (cns,pns,ns) i = MoC.stated13 (wrap43 cns pns (psi43 ns)) (part3 i)
stated14 (cns,pns,ns) i = MoC.stated14 (wrap54 cns pns (psi54 ns)) (part4 i)
stated21 (cns,pns,ns) i = MoC.stated21 (wrap31 cns pns (psi31 ns)) (part  i)
stated22 (cns,pns,ns) i = MoC.stated22 (wrap42 cns pns (psi42 ns)) (part2 i)
stated23 (cns,pns,ns) i = MoC.stated23 (wrap53 cns pns (psi53 ns)) (part3 i)
stated24 (cns,pns,ns) i = MoC.stated24 (wrap64 cns pns (psi64 ns)) (part4 i)
stated31 (cns,pns,ns) i = MoC.stated31 (wrap41 cns pns (psi41 ns)) (part  i)
stated32 (cns,pns,ns) i = MoC.stated32 (wrap52 cns pns (psi52 ns)) (part2 i)
stated33 (cns,pns,ns) i = MoC.stated33 (wrap63 cns pns (psi63 ns)) (part3 i)
stated34 (cns,pns,ns) i = MoC.stated34 (wrap74 cns pns (psi74 ns)) (part4 i)
stated41 (cns,pns,ns) i = MoC.stated41 (wrap51 cns pns (psi51 ns)) (part  i)
stated42 (cns,pns,ns) i = MoC.stated42 (wrap62 cns pns (psi62 ns)) (part2 i)
stated43 (cns,pns,ns) i = MoC.stated43 (wrap73 cns pns (psi73 ns)) (part3 i)
stated44 (cns,pns,ns) i = MoC.stated44 (wrap84 cns pns (psi84 ns)) (part4 i)

------- MOORE -------

moore11 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ( Cons,        Prod,                 [st] -> [b1])
        -> [st] -> Sig a1 -> Sig b1                                
moore12 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ( Cons,       (Prod,Prod),           [st] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> (Sig b1, Sig b2)                          
moore13 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ( Cons,       (Prod,Prod,Prod),      [st] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
moore14 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ( Cons      , (Prod,Prod,Prod,Prod), [st] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
moore21 :: ((Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ( Cons,        Prod,                     [st] -> [b1])
        -> [st] -> Sig a1 -> Sig a2 -> Sig b1                          
moore22 :: ((Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ( Cons,           (Prod,Prod),           [st] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
moore23 :: ((Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ( Cons,           (Prod,Prod,Prod),      [st] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
moore24 :: ((Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ( Cons,           (Prod,Prod,Prod,Prod), [st] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
moore31 :: ((Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons,                 Prod,                 [st] -> [b1])
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
moore32 :: ((Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons,                (Prod,Prod),           [st] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
moore33 :: ((Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons,                (Prod,Prod,Prod),      [st] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
moore34 :: ((Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons,                (Prod,Prod,Prod,Prod), [st] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
moore41 :: ((Cons,Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ( Cons,                      Prod,                 [st] -> [b1])
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
moore42 :: ((Cons,Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ( Cons,                     (Prod,Prod),           [st] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
moore43 :: ((Cons,Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ( Cons,                     (Prod,Prod,Prod),      [st] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
moore44 :: ((Cons,Cons,Cons,Cons,Cons), Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ( Cons,                     (Prod,Prod,Prod,Prod), [st] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

moore11 (cns,pns,ns) (cod,pod,od) i = MoC.moore11 (wrap21 cns pns (psi21 ns)) (wrap11 cod pod (psi11 od)) (part i)
moore12 (cns,pns,ns) (cod,pod,od) i = MoC.moore12 (wrap21 cns pns (psi21 ns)) (wrap12 cod pod (psi12 od)) (part i)
moore13 (cns,pns,ns) (cod,pod,od) i = MoC.moore13 (wrap21 cns pns (psi21 ns)) (wrap13 cod pod (psi13 od)) (part i)
moore14 (cns,pns,ns) (cod,pod,od) i = MoC.moore14 (wrap21 cns pns (psi21 ns)) (wrap14 cod pod (psi14 od)) (part i)
moore21 (cns,pns,ns) (cod,pod,od) i = MoC.moore21 (wrap31 cns pns (psi31 ns)) (wrap11 cod pod (psi11 od)) (part i)
moore22 (cns,pns,ns) (cod,pod,od) i = MoC.moore22 (wrap31 cns pns (psi31 ns)) (wrap12 cod pod (psi12 od)) (part i)
moore23 (cns,pns,ns) (cod,pod,od) i = MoC.moore23 (wrap31 cns pns (psi31 ns)) (wrap13 cod pod (psi13 od)) (part i)
moore24 (cns,pns,ns) (cod,pod,od) i = MoC.moore24 (wrap31 cns pns (psi31 ns)) (wrap14 cod pod (psi14 od)) (part i)
moore31 (cns,pns,ns) (cod,pod,od) i = MoC.moore31 (wrap41 cns pns (psi41 ns)) (wrap11 cod pod (psi11 od)) (part i)
moore32 (cns,pns,ns) (cod,pod,od) i = MoC.moore32 (wrap41 cns pns (psi41 ns)) (wrap12 cod pod (psi12 od)) (part i)
moore33 (cns,pns,ns) (cod,pod,od) i = MoC.moore33 (wrap41 cns pns (psi41 ns)) (wrap13 cod pod (psi13 od)) (part i)
moore34 (cns,pns,ns) (cod,pod,od) i = MoC.moore34 (wrap41 cns pns (psi41 ns)) (wrap14 cod pod (psi14 od)) (part i)
moore41 (cns,pns,ns) (cod,pod,od) i = MoC.moore41 (wrap51 cns pns (psi51 ns)) (wrap11 cod pod (psi11 od)) (part i)
moore42 (cns,pns,ns) (cod,pod,od) i = MoC.moore42 (wrap51 cns pns (psi51 ns)) (wrap12 cod pod (psi12 od)) (part i)
moore43 (cns,pns,ns) (cod,pod,od) i = MoC.moore43 (wrap51 cns pns (psi51 ns)) (wrap13 cod pod (psi13 od)) (part i)
moore44 (cns,pns,ns) (cod,pod,od) i = MoC.moore44 (wrap51 cns pns (psi51 ns)) (wrap14 cod pod (psi14 od)) (part i)


------- MEALY -------

mealy11 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ((Cons,Cons),  Prod,                 [st] -> [a1] -> [b1])
        -> [st] -> Sig a1 -> Sig b1                                
mealy12 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ((Cons,Cons), (Prod,Prod),           [st] -> [a1] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> (Sig b1, Sig b2)                          
mealy13 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ((Cons,Cons), (Prod,Prod,Prod),      [st] -> [a1] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
mealy14 :: ((Cons,Cons),  Prod,                 [st] -> [a1] -> [st])
        -> ((Cons,Cons), (Prod,Prod,Prod,Prod), [st] -> [a1] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
mealy21 :: ((Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ((Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [b1])
        -> [st] -> Sig a1 -> Sig a2 -> Sig b1                          
mealy22 :: ((Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ((Cons,Cons,Cons), (Prod,Prod),           [st] -> [a1] -> [a2] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
mealy23 :: ((Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ((Cons,Cons,Cons), (Prod,Prod,Prod),      [st] -> [a1] -> [a2] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
mealy24 :: ((Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [st])
        -> ((Cons,Cons,Cons), (Prod,Prod,Prod,Prod), [st] -> [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
mealy31 :: ((Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [b1])
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
mealy32 :: ((Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons), (Prod,Prod),           [st] -> [a1] -> [a2] -> [a3] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
mealy33 :: ((Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),      [st] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
mealy34 :: ((Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod), [st] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)
           
mealy41 :: ((Cons,Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [b1])
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
mealy42 :: ((Cons,Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod),           [st] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
mealy43 :: ((Cons,Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),      [st] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
mealy44 :: ((Cons,Cons,Cons,Cons,Cons),  Prod,                 [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod), [st] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)


mealy11 (cns,pns,ns) (cod,pod,od) i = MoC.mealy11 (wrap21 cns pns (psi21 ns)) (wrap21 cod pod (psi21 od)) (part i)
mealy12 (cns,pns,ns) (cod,pod,od) i = MoC.mealy12 (wrap21 cns pns (psi21 ns)) (wrap22 cod pod (psi22 od)) (part i)
mealy13 (cns,pns,ns) (cod,pod,od) i = MoC.mealy13 (wrap21 cns pns (psi21 ns)) (wrap23 cod pod (psi23 od)) (part i)
mealy14 (cns,pns,ns) (cod,pod,od) i = MoC.mealy14 (wrap21 cns pns (psi21 ns)) (wrap24 cod pod (psi24 od)) (part i)
mealy21 (cns,pns,ns) (cod,pod,od) i = MoC.mealy21 (wrap31 cns pns (psi31 ns)) (wrap31 cod pod (psi31 od)) (part i)
mealy22 (cns,pns,ns) (cod,pod,od) i = MoC.mealy22 (wrap31 cns pns (psi31 ns)) (wrap32 cod pod (psi32 od)) (part i)
mealy23 (cns,pns,ns) (cod,pod,od) i = MoC.mealy23 (wrap31 cns pns (psi31 ns)) (wrap33 cod pod (psi33 od)) (part i)
mealy24 (cns,pns,ns) (cod,pod,od) i = MoC.mealy24 (wrap31 cns pns (psi31 ns)) (wrap34 cod pod (psi34 od)) (part i)
mealy31 (cns,pns,ns) (cod,pod,od) i = MoC.mealy31 (wrap41 cns pns (psi41 ns)) (wrap41 cod pod (psi41 od)) (part i)
mealy32 (cns,pns,ns) (cod,pod,od) i = MoC.mealy32 (wrap41 cns pns (psi41 ns)) (wrap42 cod pod (psi42 od)) (part i)
mealy33 (cns,pns,ns) (cod,pod,od) i = MoC.mealy33 (wrap41 cns pns (psi41 ns)) (wrap43 cod pod (psi43 od)) (part i)
mealy34 (cns,pns,ns) (cod,pod,od) i = MoC.mealy34 (wrap41 cns pns (psi41 ns)) (wrap44 cod pod (psi44 od)) (part i)
mealy41 (cns,pns,ns) (cod,pod,od) i = MoC.mealy41 (wrap51 cns pns (psi51 ns)) (wrap51 cod pod (psi51 od)) (part i)
mealy42 (cns,pns,ns) (cod,pod,od) i = MoC.mealy42 (wrap51 cns pns (psi51 ns)) (wrap52 cod pod (psi52 od)) (part i)
mealy43 (cns,pns,ns) (cod,pod,od) i = MoC.mealy43 (wrap51 cns pns (psi51 ns)) (wrap53 cod pod (psi53 od)) (part i)
mealy44 (cns,pns,ns) (cod,pod,od) i = MoC.mealy44 (wrap51 cns pns (psi51 ns)) (wrap54 cod pod (psi54 od)) (part i)


--tst1 a b = [head a + head b, last a - last b]
