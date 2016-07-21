{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.DE.Lib
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
module ForSyDe.Atom.MoC.DE.Lib where

import qualified ForSyDe.Atom.MoC.Cons as MoC
import           ForSyDe.Atom.MoC.DE.Core
import           ForSyDe.Atom.Utility
import           ForSyDe.Atom.Behavior hiding (value)



delay :: Int -> a -> Sig a -> Sig a
delay t v = MoC.delay (event (t, v))

comb11 :: (a1 -> b1)
       -> Sig a1 -> Sig b1                                
comb12 :: (a1 -> (b1, b2))
       -> Sig a1 -> (Sig b1, Sig b2)                          
comb13 :: (a1 -> (b1, b2, b3))
       -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
comb14 :: (a1 -> (b1, b2, b3, b4))
       -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
comb21 :: (a1 -> a2 -> b1)
       -> Sig a1 -> Sig a2 -> Sig b1                          
comb22 :: (a1 -> a2 -> (b1, b2))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
comb23 :: (a1 -> a2 -> (b1, b2, b3))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
comb24 :: (a1 -> a2 -> (b1, b2, b3, b4))
       -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)            
comb31 :: (a1 -> a2 -> a3 -> b1)
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
comb32 :: (a1 -> a2 -> a3 -> (b1, b2))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
comb33 :: (a1 -> a2 -> a3 -> (b1, b2, b3))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
comb34 :: (a1 -> a2 -> a3 -> (b1, b2, b3, b4))
       -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
comb41 :: (a1 -> a2 -> a3 -> a4 -> b1)
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
comb42 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
comb43 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
comb44 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
       -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

comb11 f = MoC.comb11 ((psi11 . psi11) f)
comb12 f = MoC.comb12 ((psi12 . psi12) f)
comb13 f = MoC.comb13 ((psi13 . psi13) f)
comb14 f = MoC.comb14 ((psi14 . psi14) f)
comb21 f = MoC.comb21 ((psi21 . psi21) f)
comb22 f = MoC.comb22 ((psi22 . psi22) f)
comb23 f = MoC.comb23 ((psi23 . psi23) f)
comb24 f = MoC.comb24 ((psi24 . psi24) f)
comb31 f = MoC.comb31 ((psi31 . psi31) f)
comb32 f = MoC.comb32 ((psi32 . psi32) f)
comb33 f = MoC.comb33 ((psi33 . psi33) f)
comb34 f = MoC.comb34 ((psi34 . psi34) f)
comb41 f = MoC.comb41 ((psi41 . psi41) f)
comb42 f = MoC.comb42 ((psi42 . psi42) f)
comb43 f = MoC.comb43 ((psi43 . psi43) f)
comb44 f = MoC.comb44 ((psi44 . psi44) f)


constant1 :: b1 -> Sig b1                                
constant2 :: (b1, b2) -> (Sig b1, Sig b2)                          
constant3 :: (b1, b2, b3) -> (Sig b1, Sig b2, Sig b3)                      
constant4 :: (b1, b2, b3, b4) -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

constant1 i =  signal [(0, i)]
constant2 i = (signal [(0, i)] |||<)
constant3 i = (signal [(0, i)] |||<<)
constant4 i = (signal [(0, i)] |||<<<)


scanld11 :: (b1 -> a1 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig b1                                
scanld12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> (Sig b1, Sig b2)                          
scanld13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
scanld14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
scanld21 :: (b1 -> a1 -> a2 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig a2 -> Sig b1                          
scanld22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
scanld23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
scanld24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
scanld31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
scanld32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
scanld33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
scanld34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
scanld41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
scanld42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
scanld43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
scanld44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

scanld11 ns i = MoC.scanld11 ((psi21 . psi21) ns) (event  i)
scanld12 ns i = MoC.scanld12 ((psi32 . psi32) ns) (event2 i)
scanld13 ns i = MoC.scanld13 ((psi43 . psi43) ns) (event3 i)
scanld14 ns i = MoC.scanld14 ((psi54 . psi54) ns) (event4 i)
scanld21 ns i = MoC.scanld21 ((psi31 . psi31) ns) (event  i)
scanld22 ns i = MoC.scanld22 ((psi42 . psi42) ns) (event2 i)
scanld23 ns i = MoC.scanld23 ((psi53 . psi53) ns) (event3 i)
scanld24 ns i = MoC.scanld24 ((psi64 . psi64) ns) (event4 i)
scanld31 ns i = MoC.scanld31 ((psi41 . psi41) ns) (event  i)
scanld32 ns i = MoC.scanld32 ((psi52 . psi52) ns) (event2 i)
scanld33 ns i = MoC.scanld33 ((psi63 . psi63) ns) (event3 i)
scanld34 ns i = MoC.scanld34 ((psi74 . psi74) ns) (event4 i)
scanld41 ns i = MoC.scanld41 ((psi51 . psi51) ns) (event  i)
scanld42 ns i = MoC.scanld42 ((psi62 . psi62) ns) (event2 i)
scanld43 ns i = MoC.scanld43 ((psi73 . psi73) ns) (event3 i)
scanld44 ns i = MoC.scanld44 ((psi84 . psi84) ns) (event4 i)


scanl11 :: (b1 -> a1 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig b1                                
scanl12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> (Sig b1, Sig b2)                          
scanl13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
scanl14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
scanl21 :: (b1 -> a1 -> a2 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig a2 -> Sig b1                          
scanl22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
scanl23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
scanl24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
scanl31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
scanl32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
scanl33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
scanl34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
scanl41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> (Int, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
scanl42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> ((Int, b1), (Int, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
scanl43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> ((Int, b1), (Int, b2), (Int, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
scanl44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> ((Int, b1), (Int, b2), (Int, b3), (Int, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

scanl11 ns i = MoC.scanl11 ((psi21 . psi21) ns) (event  i)
scanl12 ns i = MoC.scanl12 ((psi32 . psi32) ns) (event2 i)
scanl13 ns i = MoC.scanl13 ((psi43 . psi43) ns) (event3 i)
scanl14 ns i = MoC.scanl14 ((psi54 . psi54) ns) (event4 i)
scanl21 ns i = MoC.scanl21 ((psi31 . psi31) ns) (event  i)
scanl22 ns i = MoC.scanl22 ((psi42 . psi42) ns) (event2 i)
scanl23 ns i = MoC.scanl23 ((psi53 . psi53) ns) (event3 i)
scanl24 ns i = MoC.scanl24 ((psi64 . psi64) ns) (event4 i)
scanl31 ns i = MoC.scanl31 ((psi41 . psi41) ns) (event  i)
scanl32 ns i = MoC.scanl32 ((psi52 . psi52) ns) (event2 i)
scanl33 ns i = MoC.scanl33 ((psi63 . psi63) ns) (event3 i)
scanl34 ns i = MoC.scanl34 ((psi74 . psi74) ns) (event4 i)
scanl41 ns i = MoC.scanl41 ((psi51 . psi51) ns) (event  i)
scanl42 ns i = MoC.scanl42 ((psi62 . psi62) ns) (event2 i)
scanl43 ns i = MoC.scanl43 ((psi73 . psi73) ns) (event3 i)
scanl44 ns i = MoC.scanl44 ((psi84 . psi84) ns) (event4 i)


moore11 :: (st -> a1 -> st) -> (st -> b1) -> (Int, st)
        -> Sig a1 -> Sig b1                                
moore12 :: (st -> a1 -> st) -> (st -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> (Sig b1, Sig b2)                          
moore13 :: (st -> a1 -> st) -> (st -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
moore14 :: (st -> a1 -> st) -> (st -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
moore21 :: (st -> a1 -> a2 -> st) -> (st -> b1) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig b1                          
moore22 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
moore23 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
moore24 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
moore31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> b1) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
moore32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
moore33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
moore34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
moore41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> b1) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
moore42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
moore43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
moore44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

moore11 ns od i = MoC.moore11 ((psi21 . psi21) ns) ((psi11 . psi11) od) (event i)
moore12 ns od i = MoC.moore12 ((psi21 . psi21) ns) ((psi12 . psi12) od) (event i)
moore13 ns od i = MoC.moore13 ((psi21 . psi21) ns) ((psi13 . psi13) od) (event i)
moore14 ns od i = MoC.moore14 ((psi21 . psi21) ns) ((psi14 . psi14) od) (event i)
moore21 ns od i = MoC.moore21 ((psi31 . psi31) ns) ((psi11 . psi11) od) (event i)
moore22 ns od i = MoC.moore22 ((psi31 . psi31) ns) ((psi12 . psi12) od) (event i)
moore23 ns od i = MoC.moore23 ((psi31 . psi31) ns) ((psi13 . psi13) od) (event i)
moore24 ns od i = MoC.moore24 ((psi31 . psi31) ns) ((psi14 . psi14) od) (event i)
moore31 ns od i = MoC.moore31 ((psi41 . psi41) ns) ((psi11 . psi11) od) (event i)
moore32 ns od i = MoC.moore32 ((psi41 . psi41) ns) ((psi12 . psi12) od) (event i)
moore33 ns od i = MoC.moore33 ((psi41 . psi41) ns) ((psi13 . psi13) od) (event i)
moore34 ns od i = MoC.moore34 ((psi41 . psi41) ns) ((psi14 . psi14) od) (event i)
moore41 ns od i = MoC.moore41 ((psi51 . psi51) ns) ((psi11 . psi11) od) (event i)
moore42 ns od i = MoC.moore42 ((psi51 . psi51) ns) ((psi12 . psi12) od) (event i)
moore43 ns od i = MoC.moore43 ((psi51 . psi51) ns) ((psi13 . psi13) od) (event i)
moore44 ns od i = MoC.moore44 ((psi51 . psi51) ns) ((psi14 . psi14) od) (event i)


mealy11 :: (st -> a1 -> st) -> (st -> a1 -> b1) -> (Int, st)
        -> Sig a1 -> Sig b1                                
mealy12 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> (Sig b1, Sig b2)                          
mealy13 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
mealy14 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
mealy21 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> b1) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig b1                          
mealy22 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
mealy23 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
mealy24 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
mealy31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> b1) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
mealy32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
mealy33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
mealy34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
mealy41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> b1) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
mealy42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
mealy43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
mealy44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (Int, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

mealy11 ns od i = MoC.mealy11 ((psi21 . psi21) ns) ((psi21 . psi21) od) (event i)
mealy12 ns od i = MoC.mealy12 ((psi21 . psi21) ns) ((psi22 . psi22) od) (event i)
mealy13 ns od i = MoC.mealy13 ((psi21 . psi21) ns) ((psi23 . psi23) od) (event i)
mealy14 ns od i = MoC.mealy14 ((psi21 . psi21) ns) ((psi24 . psi24) od) (event i)
mealy21 ns od i = MoC.mealy21 ((psi31 . psi31) ns) ((psi31 . psi31) od) (event i)
mealy22 ns od i = MoC.mealy22 ((psi31 . psi31) ns) ((psi32 . psi32) od) (event i)
mealy23 ns od i = MoC.mealy23 ((psi31 . psi31) ns) ((psi33 . psi33) od) (event i)
mealy24 ns od i = MoC.mealy24 ((psi31 . psi31) ns) ((psi34 . psi34) od) (event i)
mealy31 ns od i = MoC.mealy31 ((psi41 . psi41) ns) ((psi41 . psi41) od) (event i)
mealy32 ns od i = MoC.mealy32 ((psi41 . psi41) ns) ((psi42 . psi42) od) (event i)
mealy33 ns od i = MoC.mealy33 ((psi41 . psi41) ns) ((psi43 . psi43) od) (event i)
mealy34 ns od i = MoC.mealy34 ((psi41 . psi41) ns) ((psi44 . psi44) od) (event i)
mealy41 ns od i = MoC.mealy41 ((psi51 . psi51) ns) ((psi51 . psi51) od) (event i)
mealy42 ns od i = MoC.mealy42 ((psi51 . psi51) ns) ((psi52 . psi52) od) (event i)
mealy43 ns od i = MoC.mealy43 ((psi51 . psi51) ns) ((psi53 . psi53) od) (event i)
mealy44 ns od i = MoC.mealy44 ((psi51 . psi51) ns) ((psi54 . psi54) od) (event i)

sync2 :: Sig a1 -> Sig a2 -> (Sig a1, Sig a2)                           
sync3 :: Sig a1 -> Sig a2 -> Sig a3 -> (Sig a1, Sig a2, Sig a3)             
sync4 :: Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig a1, Sig a2, Sig a3, Sig a4)

sync2 = comb22 (,)
sync3 = comb33 (,,)
sync4 = comb44 (,,,)
