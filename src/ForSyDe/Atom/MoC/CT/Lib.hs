{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.CT.Lib
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
module ForSyDe.Atom.MoC.CT.Lib where

import qualified ForSyDe.Atom.MoC.Cons as MoC
import           ForSyDe.Atom.MoC.CT.Core
import           ForSyDe.Atom.Utility
import           ForSyDe.Atom.Behavior hiding (value)



delay :: Time -> (Time -> a) -> Sig a -> Sig a
delay t f = MoC.delay (event (t, f))

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

comb11 f = MoC.comb11 (psi11 f)
comb12 f = MoC.comb12 (psi12 f)
comb13 f = MoC.comb13 (psi13 f)
comb14 f = MoC.comb14 (psi14 f)
comb21 f = MoC.comb21 (psi21 f)
comb22 f = MoC.comb22 (psi22 f)
comb23 f = MoC.comb23 (psi23 f)
comb24 f = MoC.comb24 (psi24 f)
comb31 f = MoC.comb31 (psi31 f)
comb32 f = MoC.comb32 (psi32 f)
comb33 f = MoC.comb33 (psi33 f)
comb34 f = MoC.comb34 (psi34 f)
comb41 f = MoC.comb41 (psi41 f)
comb42 f = MoC.comb42 (psi42 f)
comb43 f = MoC.comb43 (psi43 f)
comb44 f = MoC.comb44 (psi44 f)


constant1 :: b1 -> Sig b1                                
constant2 :: (b1, b2) -> (Sig b1, Sig b2)                          
constant3 :: (b1, b2, b3) -> (Sig b1, Sig b2, Sig b3)                      
constant4 :: (b1, b2, b3, b4) -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

constant1 i =  signal [(0, \_ -> i)]
constant2 i = (signal [(0, \_ -> i)] |||<)
constant3 i = (signal [(0, \_ -> i)] |||<<)
constant4 i = (signal [(0, \_ -> i)] |||<<<)

scanld11 :: (b1 -> a1 -> b1)                   -> Time -> (Time -> b1)
         -> Sig a1 -> Sig b1                                
scanld21 :: (b1 -> a1 -> a2 -> b1)             -> Time -> (Time -> b1)
         -> Sig a1 -> Sig a2 -> Sig b1                          
scanld31 :: (b1 -> a1 -> a2 -> a3 -> b1)       -> Time -> (Time -> b1)
         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
scanld41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> Time -> (Time -> b1)
         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
scanld12 :: ((b1, b2) -> a1 -> (b1, b2))                   -> Time -> (Time -> (b1, b2))
         -> Sig a1 -> (Sig b1, Sig b2)                          
scanld22 :: ((b1, b2) -> a1 -> a2 -> (b1, b2))             -> Time -> (Time -> (b1, b2))
         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
scanld32 :: ((b1, b2) -> a1 -> a2 -> a3 -> (b1, b2))       -> Time -> (Time -> (b1, b2))
         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
scanld42 :: ((b1, b2) -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> Time -> (Time -> (b1, b2))
         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
scanld13 :: ((b1, b2, b3) -> a1 -> (b1, b2, b3))                   -> Time -> (Time -> (b1, b2, b3))
         -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
scanld23 :: ((b1, b2, b3) -> a1 -> a2 -> (b1, b2, b3))             -> Time -> (Time -> (b1, b2, b3))
         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
scanld33 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> (b1, b2, b3))       -> Time -> (Time -> (b1, b2, b3))
         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
scanld43 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> Time -> (Time -> (b1, b2, b3))
         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
scanld14 :: ((b1, b2, b3, b4) -> a1 -> (b1, b2, b3, b4))                   -> Time -> (Time -> (b1, b2, b3, b4))
         -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
scanld24 :: ((b1, b2, b3, b4) -> a1 -> a2 -> (b1, b2, b3, b4))             -> Time -> (Time -> (b1, b2, b3, b4))
         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
scanld34 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> (b1, b2, b3, b4))       -> Time -> (Time -> (b1, b2, b3, b4))
         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
scanld44 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> Time -> (Time -> (b1, b2, b3, b4))
         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

scanld11 ns t i = MoC.scanld11 (psi21 ns) (event (t, i))
scanld12 ns t i = MoC.scanld12 (psi21 ns) (event (t, i))
scanld13 ns t i = MoC.scanld13 (psi21 ns) (event (t, i))
scanld14 ns t i = MoC.scanld14 (psi21 ns) (event (t, i))
scanld21 ns t i = MoC.scanld21 (psi31 ns) (event (t, i))
scanld22 ns t i = MoC.scanld22 (psi31 ns) (event (t, i))
scanld23 ns t i = MoC.scanld23 (psi31 ns) (event (t, i))
scanld24 ns t i = MoC.scanld24 (psi31 ns) (event (t, i))
scanld31 ns t i = MoC.scanld31 (psi41 ns) (event (t, i))
scanld32 ns t i = MoC.scanld32 (psi41 ns) (event (t, i))
scanld33 ns t i = MoC.scanld33 (psi41 ns) (event (t, i))
scanld34 ns t i = MoC.scanld34 (psi41 ns) (event (t, i))
scanld41 ns t i = MoC.scanld41 (psi51 ns) (event (t, i))
scanld42 ns t i = MoC.scanld42 (psi51 ns) (event (t, i))
scanld43 ns t i = MoC.scanld43 (psi51 ns) (event (t, i))
scanld44 ns t i = MoC.scanld44 (psi51 ns) (event (t, i))


scanl11 :: (b1 -> a1 -> b1)                   -> Time -> (Time -> b1)
        -> Sig a1 -> Sig b1                                
scanl21 :: (b1 -> a1 -> a2 -> b1)             -> Time -> (Time -> b1)
        -> Sig a1 -> Sig a2 -> Sig b1                          
scanl31 :: (b1 -> a1 -> a2 -> a3 -> b1)       -> Time -> (Time -> b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
scanl41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> Time -> (Time -> b1)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
scanl12 :: ((b1, b2) -> a1 -> (b1, b2))                   -> Time -> (Time -> (b1, b2))
        -> Sig a1 -> (Sig b1, Sig b2)                          
scanl22 :: ((b1, b2) -> a1 -> a2 -> (b1, b2))             -> Time -> (Time -> (b1, b2))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
scanl32 :: ((b1, b2) -> a1 -> a2 -> a3 -> (b1, b2))       -> Time -> (Time -> (b1, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
scanl42 :: ((b1, b2) -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> Time -> (Time -> (b1, b2))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
scanl13 :: ((b1, b2, b3) -> a1 -> (b1, b2, b3))                   -> Time -> (Time -> (b1, b2, b3))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
scanl23 :: ((b1, b2, b3) -> a1 -> a2 -> (b1, b2, b3))             -> Time -> (Time -> (b1, b2, b3))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
scanl33 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> (b1, b2, b3))       -> Time -> (Time -> (b1, b2, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
scanl43 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> Time -> (Time -> (b1, b2, b3))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
scanl14 :: ((b1, b2, b3, b4) -> a1 -> (b1, b2, b3, b4))                   -> Time -> (Time -> (b1, b2, b3, b4))
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
scanl24 :: ((b1, b2, b3, b4) -> a1 -> a2 -> (b1, b2, b3, b4))             -> Time -> (Time -> (b1, b2, b3, b4))
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
scanl34 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> (b1, b2, b3, b4))       -> Time -> (Time -> (b1, b2, b3, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
scanl44 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> Time -> (Time -> (b1, b2, b3, b4))
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

scanl11 ns t i = MoC.scanl11 (psi21 ns) (event (t, i))
scanl12 ns t i = MoC.scanl12 (psi21 ns) (event (t, i))
scanl13 ns t i = MoC.scanl13 (psi21 ns) (event (t, i))
scanl14 ns t i = MoC.scanl14 (psi21 ns) (event (t, i))
scanl21 ns t i = MoC.scanl21 (psi31 ns) (event (t, i))
scanl22 ns t i = MoC.scanl22 (psi31 ns) (event (t, i))
scanl23 ns t i = MoC.scanl23 (psi31 ns) (event (t, i))
scanl24 ns t i = MoC.scanl24 (psi31 ns) (event (t, i))
scanl31 ns t i = MoC.scanl31 (psi41 ns) (event (t, i))
scanl32 ns t i = MoC.scanl32 (psi41 ns) (event (t, i))
scanl33 ns t i = MoC.scanl33 (psi41 ns) (event (t, i))
scanl34 ns t i = MoC.scanl34 (psi41 ns) (event (t, i))
scanl41 ns t i = MoC.scanl41 (psi51 ns) (event (t, i))
scanl42 ns t i = MoC.scanl42 (psi51 ns) (event (t, i))
scanl43 ns t i = MoC.scanl43 (psi51 ns) (event (t, i))
scanl44 ns t i = MoC.scanl44 (psi51 ns) (event (t, i))


moore11 :: (st -> a1 -> st) -> (st -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig b1                                
moore12 :: (st -> a1 -> st) -> (st -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> (Sig b1, Sig b2)                          
moore13 :: (st -> a1 -> st) -> (st -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
moore14 :: (st -> a1 -> st) -> (st -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
moore21 :: (st -> a1 -> a2 -> st) -> (st -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig b1                          
moore22 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
moore23 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
moore24 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
moore31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
moore32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
moore33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
moore34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
moore41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
moore42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
moore43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
moore44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

moore11 ns od t i = MoC.moore11 (psi21 ns) (psi11 od) (event (t, i))
moore12 ns od t i = MoC.moore12 (psi21 ns) (psi12 od) (event (t, i))
moore13 ns od t i = MoC.moore13 (psi21 ns) (psi13 od) (event (t, i))
moore14 ns od t i = MoC.moore14 (psi21 ns) (psi14 od) (event (t, i))
moore21 ns od t i = MoC.moore21 (psi31 ns) (psi11 od) (event (t, i))
moore22 ns od t i = MoC.moore22 (psi31 ns) (psi12 od) (event (t, i))
moore23 ns od t i = MoC.moore23 (psi31 ns) (psi13 od) (event (t, i))
moore24 ns od t i = MoC.moore24 (psi31 ns) (psi14 od) (event (t, i))
moore31 ns od t i = MoC.moore31 (psi41 ns) (psi11 od) (event (t, i))
moore32 ns od t i = MoC.moore32 (psi41 ns) (psi12 od) (event (t, i))
moore33 ns od t i = MoC.moore33 (psi41 ns) (psi13 od) (event (t, i))
moore34 ns od t i = MoC.moore34 (psi41 ns) (psi14 od) (event (t, i))
moore41 ns od t i = MoC.moore41 (psi51 ns) (psi11 od) (event (t, i))
moore42 ns od t i = MoC.moore42 (psi51 ns) (psi12 od) (event (t, i))
moore43 ns od t i = MoC.moore43 (psi51 ns) (psi13 od) (event (t, i))
moore44 ns od t i = MoC.moore44 (psi51 ns) (psi14 od) (event (t, i))


mealy11 :: (st -> a1 -> st) -> (st -> a1 -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig b1                                
mealy12 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> (Sig b1, Sig b2)                          
mealy13 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
mealy14 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
mealy21 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig b1                          
mealy22 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
mealy23 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
mealy24 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
mealy31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
mealy32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
mealy33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
mealy34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
mealy41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> b1) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
mealy42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
mealy43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
mealy44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> Time -> (Time -> st)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

mealy11 ns od t i = MoC.mealy11 (psi21 ns) (psi21 od) (event (t, i))
mealy12 ns od t i = MoC.mealy12 (psi21 ns) (psi22 od) (event (t, i))
mealy13 ns od t i = MoC.mealy13 (psi21 ns) (psi23 od) (event (t, i))
mealy14 ns od t i = MoC.mealy14 (psi21 ns) (psi24 od) (event (t, i))
mealy21 ns od t i = MoC.mealy21 (psi31 ns) (psi31 od) (event (t, i))
mealy22 ns od t i = MoC.mealy22 (psi31 ns) (psi32 od) (event (t, i))
mealy23 ns od t i = MoC.mealy23 (psi31 ns) (psi33 od) (event (t, i))
mealy24 ns od t i = MoC.mealy24 (psi31 ns) (psi34 od) (event (t, i))
mealy31 ns od t i = MoC.mealy31 (psi41 ns) (psi41 od) (event (t, i))
mealy32 ns od t i = MoC.mealy32 (psi41 ns) (psi42 od) (event (t, i))
mealy33 ns od t i = MoC.mealy33 (psi41 ns) (psi43 od) (event (t, i))
mealy34 ns od t i = MoC.mealy34 (psi41 ns) (psi44 od) (event (t, i))
mealy41 ns od t i = MoC.mealy41 (psi51 ns) (psi51 od) (event (t, i))
mealy42 ns od t i = MoC.mealy42 (psi51 ns) (psi52 od) (event (t, i))
mealy43 ns od t i = MoC.mealy43 (psi51 ns) (psi53 od) (event (t, i))
mealy44 ns od t i = MoC.mealy44 (psi51 ns) (psi54 od) (event (t, i))

