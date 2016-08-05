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

import           ForSyDe.Atom.Behavior hiding (value)
import qualified ForSyDe.Atom.MoC.AtomLib as MoC
import           ForSyDe.Atom.MoC.DE.Core
import           ForSyDe.Atom.Utility

import           Numeric.Natural

delay :: Natural -> a -> Sig a -> Sig a
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

comb11 f = MoC.comb11 (wrap11 (psi11 f))
comb12 f = MoC.comb12 (wrap12 (psi12 f))
comb13 f = MoC.comb13 (wrap13 (psi13 f))
comb14 f = MoC.comb14 (wrap14 (psi14 f))
comb21 f = MoC.comb21 (wrap21 (psi21 f))
comb22 f = MoC.comb22 (wrap22 (psi22 f))
comb23 f = MoC.comb23 (wrap23 (psi23 f))
comb24 f = MoC.comb24 (wrap24 (psi24 f))
comb31 f = MoC.comb31 (wrap31 (psi31 f))
comb32 f = MoC.comb32 (wrap32 (psi32 f))
comb33 f = MoC.comb33 (wrap33 (psi33 f))
comb34 f = MoC.comb34 (wrap34 (psi34 f))
comb41 f = MoC.comb41 (wrap41 (psi41 f))
comb42 f = MoC.comb42 (wrap42 (psi42 f))
comb43 f = MoC.comb43 (wrap43 (psi43 f))
comb44 f = MoC.comb44 (wrap44 (psi44 f))


constant1 :: b1 -> Sig b1                                
constant2 :: (b1, b2) -> (Sig b1, Sig b2)                          
constant3 :: (b1, b2, b3) -> (Sig b1, Sig b2, Sig b3)                      
constant4 :: (b1, b2, b3, b4) -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

constant1 i =  signal [(0, i)]
constant2 i = (signal [(0, i)] ||||<)
constant3 i = (signal [(0, i)] ||||<<)
constant4 i = (signal [(0, i)] ||||<<<)



generate1 :: (b1 -> b1) -> (Tag, b1)
          -> Sig b1                                
generate2 :: (b1 -> b2 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
          -> (Sig b1, Sig b2)                          
generate3 :: (b1 -> b2 -> b3 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
          -> (Sig b1, Sig b2, Sig b3)                      
generate4 :: (b1 -> b2 -> b3 -> b4 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
          -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

generate1 ns i = MoC.stated01 (wrap11 (psi11 ns)) (event  i)
generate2 ns i = MoC.stated02 (wrap22 (psi22 ns)) (event2 i)
generate3 ns i = MoC.stated03 (wrap33 (psi33 ns)) (event3 i)
generate4 ns i = MoC.stated04 (wrap44 (psi44 ns)) (event4 i)


stated11 :: (b1 -> a1 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig b1                                
stated12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> (Sig b1, Sig b2)                          
stated13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
stated14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
stated21 :: (b1 -> a1 -> a2 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig a2 -> Sig b1                          
stated22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
stated23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
stated24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
stated31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
stated32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
stated33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
stated34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
stated41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
stated42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
stated43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
stated44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

stated11 ns i = MoC.stated11 (wrap21 (psi21 ns)) (event  i)
stated12 ns i = MoC.stated12 (wrap32 (psi32 ns)) (event2 i)
stated13 ns i = MoC.stated13 (wrap43 (psi43 ns)) (event3 i)
stated14 ns i = MoC.stated14 (wrap54 (psi54 ns)) (event4 i)
stated21 ns i = MoC.stated21 (wrap31 (psi31 ns)) (event  i)
stated22 ns i = MoC.stated22 (wrap42 (psi42 ns)) (event2 i)
stated23 ns i = MoC.stated23 (wrap53 (psi53 ns)) (event3 i)
stated24 ns i = MoC.stated24 (wrap64 (psi64 ns)) (event4 i)
stated31 ns i = MoC.stated31 (wrap41 (psi41 ns)) (event  i)
stated32 ns i = MoC.stated32 (wrap52 (psi52 ns)) (event2 i)
stated33 ns i = MoC.stated33 (wrap63 (psi63 ns)) (event3 i)
stated34 ns i = MoC.stated34 (wrap74 (psi74 ns)) (event4 i)
stated41 ns i = MoC.stated41 (wrap51 (psi51 ns)) (event  i)
stated42 ns i = MoC.stated42 (wrap62 (psi62 ns)) (event2 i)
stated43 ns i = MoC.stated43 (wrap73 (psi73 ns)) (event3 i)
stated44 ns i = MoC.stated44 (wrap84 (psi84 ns)) (event4 i)


state11 :: (b1 -> a1 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig b1                                
state12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> (Sig b1, Sig b2)                          
state13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
state14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
state21 :: (b1 -> a1 -> a2 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig a2 -> Sig b1                          
state22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
state23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
state24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
state31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
state32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
state33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
state34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
state41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> (Tag, b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
state42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> ((Tag, b1), (Tag, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
state43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> ((Tag, b1), (Tag, b2), (Tag, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
state44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> ((Tag, b1), (Tag, b2), (Tag, b3), (Tag, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

state11 ns i = MoC.state11 (wrap21 (psi21 ns)) (event  i)
state12 ns i = MoC.state12 (wrap32 (psi32 ns)) (event2 i)
state13 ns i = MoC.state13 (wrap43 (psi43 ns)) (event3 i)
state14 ns i = MoC.state14 (wrap54 (psi54 ns)) (event4 i)
state21 ns i = MoC.state21 (wrap31 (psi31 ns)) (event  i)
state22 ns i = MoC.state22 (wrap42 (psi42 ns)) (event2 i)
state23 ns i = MoC.state23 (wrap53 (psi53 ns)) (event3 i)
state24 ns i = MoC.state24 (wrap64 (psi64 ns)) (event4 i)
state31 ns i = MoC.state31 (wrap41 (psi41 ns)) (event  i)
state32 ns i = MoC.state32 (wrap52 (psi52 ns)) (event2 i)
state33 ns i = MoC.state33 (wrap63 (psi63 ns)) (event3 i)
state34 ns i = MoC.state34 (wrap74 (psi74 ns)) (event4 i)
state41 ns i = MoC.state41 (wrap51 (psi51 ns)) (event  i)
state42 ns i = MoC.state42 (wrap62 (psi62 ns)) (event2 i)
state43 ns i = MoC.state43 (wrap73 (psi73 ns)) (event3 i)
state44 ns i = MoC.state44 (wrap84 (psi84 ns)) (event4 i)


moore11 :: (st -> a1 -> st) -> (st -> b1) -> (Tag, st)
        -> Sig a1 -> Sig b1                                
moore12 :: (st -> a1 -> st) -> (st -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> (Sig b1, Sig b2)                          
moore13 :: (st -> a1 -> st) -> (st -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
moore14 :: (st -> a1 -> st) -> (st -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
moore21 :: (st -> a1 -> a2 -> st) -> (st -> b1) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig b1                          
moore22 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
moore23 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
moore24 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
moore31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> b1) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
moore32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
moore33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
moore34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
moore41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> b1) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
moore42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
moore43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
moore44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

moore11 ns od i = MoC.moore11 (wrap21 (psi21 ns)) (wrap11 (psi11 od)) (event i)
moore12 ns od i = MoC.moore12 (wrap21 (psi21 ns)) (wrap12 (psi12 od)) (event i)
moore13 ns od i = MoC.moore13 (wrap21 (psi21 ns)) (wrap13 (psi13 od)) (event i)
moore14 ns od i = MoC.moore14 (wrap21 (psi21 ns)) (wrap14 (psi14 od)) (event i)
moore21 ns od i = MoC.moore21 (wrap31 (psi31 ns)) (wrap11 (psi11 od)) (event i)
moore22 ns od i = MoC.moore22 (wrap31 (psi31 ns)) (wrap12 (psi12 od)) (event i)
moore23 ns od i = MoC.moore23 (wrap31 (psi31 ns)) (wrap13 (psi13 od)) (event i)
moore24 ns od i = MoC.moore24 (wrap31 (psi31 ns)) (wrap14 (psi14 od)) (event i)
moore31 ns od i = MoC.moore31 (wrap41 (psi41 ns)) (wrap11 (psi11 od)) (event i)
moore32 ns od i = MoC.moore32 (wrap41 (psi41 ns)) (wrap12 (psi12 od)) (event i)
moore33 ns od i = MoC.moore33 (wrap41 (psi41 ns)) (wrap13 (psi13 od)) (event i)
moore34 ns od i = MoC.moore34 (wrap41 (psi41 ns)) (wrap14 (psi14 od)) (event i)
moore41 ns od i = MoC.moore41 (wrap51 (psi51 ns)) (wrap11 (psi11 od)) (event i)
moore42 ns od i = MoC.moore42 (wrap51 (psi51 ns)) (wrap12 (psi12 od)) (event i)
moore43 ns od i = MoC.moore43 (wrap51 (psi51 ns)) (wrap13 (psi13 od)) (event i)
moore44 ns od i = MoC.moore44 (wrap51 (psi51 ns)) (wrap14 (psi14 od)) (event i)


mealy11 :: (st -> a1 -> st) -> (st -> a1 -> b1) -> (Tag, st)
        -> Sig a1 -> Sig b1                                
mealy12 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> (Sig b1, Sig b2)                          
mealy13 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
mealy14 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
mealy21 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> b1) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig b1                          
mealy22 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
mealy23 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
mealy24 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
mealy31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> b1) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
mealy32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
mealy33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
mealy34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
mealy41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> b1) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
mealy42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
mealy43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
mealy44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (Tag, st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

mealy11 ns od i = MoC.mealy11 (wrap21 (psi21 ns)) (wrap21 (psi21 od)) (event i)
mealy12 ns od i = MoC.mealy12 (wrap21 (psi21 ns)) (wrap22 (psi22 od)) (event i)
mealy13 ns od i = MoC.mealy13 (wrap21 (psi21 ns)) (wrap23 (psi23 od)) (event i)
mealy14 ns od i = MoC.mealy14 (wrap21 (psi21 ns)) (wrap24 (psi24 od)) (event i)
mealy21 ns od i = MoC.mealy21 (wrap31 (psi31 ns)) (wrap31 (psi31 od)) (event i)
mealy22 ns od i = MoC.mealy22 (wrap31 (psi31 ns)) (wrap32 (psi32 od)) (event i)
mealy23 ns od i = MoC.mealy23 (wrap31 (psi31 ns)) (wrap33 (psi33 od)) (event i)
mealy24 ns od i = MoC.mealy24 (wrap31 (psi31 ns)) (wrap34 (psi34 od)) (event i)
mealy31 ns od i = MoC.mealy31 (wrap41 (psi41 ns)) (wrap41 (psi41 od)) (event i)
mealy32 ns od i = MoC.mealy32 (wrap41 (psi41 ns)) (wrap42 (psi42 od)) (event i)
mealy33 ns od i = MoC.mealy33 (wrap41 (psi41 ns)) (wrap43 (psi43 od)) (event i)
mealy34 ns od i = MoC.mealy34 (wrap41 (psi41 ns)) (wrap44 (psi44 od)) (event i)
mealy41 ns od i = MoC.mealy41 (wrap51 (psi51 ns)) (wrap51 (psi51 od)) (event i)
mealy42 ns od i = MoC.mealy42 (wrap51 (psi51 ns)) (wrap52 (psi52 od)) (event i)
mealy43 ns od i = MoC.mealy43 (wrap51 (psi51 ns)) (wrap53 (psi53 od)) (event i)
mealy44 ns od i = MoC.mealy44 (wrap51 (psi51 ns)) (wrap54 (psi54 od)) (event i)

sync2 :: Sig a1 -> Sig a2 -> (Sig a1, Sig a2)                           
sync3 :: Sig a1 -> Sig a2 -> Sig a3 -> (Sig a1, Sig a2, Sig a3)             
sync4 :: Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig a1, Sig a2, Sig a3, Sig a4)

sync2 = comb22 (,)
sync3 = comb33 (,,)
sync4 = comb44 (,,,)


-- (s1,s2) = (S.takeS 5 k1, S.takeS 7 k2) :: (Sig Int, Sig Int)
--   where (k1, k2) = generate2 (\a b -> (a+1,b+2)) ((9,2),(24,5))
