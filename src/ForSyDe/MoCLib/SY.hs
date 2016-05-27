-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronuous library defines process constructors, processes and a signal conduit
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process. 
-----------------------------------------------------------------------------

module ForSyDe.MoCLib.SY where

import           ForSyDe.Core.MoC (MoC(..))
import qualified ForSyDe.Core.MoC as MoC
import           ForSyDe.Core.Signal as S
import           ForSyDe.Core.ValueExt hiding (value)

data SY a  = SY a
type Sig a = S.Signal (SY (Value a))
-----------------------------------------------------------------------------

instance MoC SY where
  (-$-) = (<$>).(<$>)
  ---------------------
  _ -*- NullS = NullS
  NullS -*- _ = NullS
  (f:-fs) -*- (x:-xs) = f <*> x :- fs -*- xs
  ---------------------
  (->-) = (:-)
  ---------------------
  (-&-) _ a = a
  ---------------------
  
instance Show a => Show (SY a) where
  showsPrec _ (SY x) = (++) (show x)

instance Read a => Read (SY a) where
  readsPrec _ s       = [(SY x, r) | (x, r) <- reads s]

instance Functor SY where
  fmap f (SY a) = SY (f a)

instance Applicative SY where
  pure = SY 
  (SY a) <*> (SY b) = SY (a b)


-----------------------------------------------------------------------------

value    = SY . Value

signal l = S.signal ((SY . Value) <$> l)

-----------------------------------------------------------------------------

delay :: a -> Sig a -> Sig a
delay i = MoC.delay (value i)

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


-- scanl11 :: (s1 -> a1 -> b1) -> st
--         -> Sig a1 -> Sig b1                                
-- scanl12 :: ((s1, s2) -> a1 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> (Sig b1, Sig b2)                          
-- scanl13 :: ((s1, s2, s3) -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
-- scanl14 :: ((s1, s2, s3, s4) -> a1 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
-- scanl21 :: (s1 -> a1 -> a2 -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig b1                          
-- scanl22 :: ((s1, s2) -> a1 -> a2 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
-- scanl23 :: ((s1, s2, s3) -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
-- scanl24 :: ((s1, s2, s3, s4) -> a1 -> a2 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
-- scanl31 :: (s1 -> a1 -> a2 -> a3 -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
-- scanl32 :: ((s1, s2) -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
-- scanl33 :: ((s1, s2, s3) -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
-- scanl34 :: ((s1, s2, s3, s4) -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
-- scanl41 :: (s1 -> a1 -> a2 -> a3 -> a4 -> b1) -> st
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
-- scanl42 :: ((s1, s2) -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (b1, b2)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
-- scanl43 :: ((s1, s2, s3) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (b1, b2, b3)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
-- scanl44 :: ((s1, s2, s3, s4) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
--         -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

-- scanl11 ns od i = MoC.scanl11 (psi21 ns) (value i)
-- scanl12 ns od i = MoC.scanl12 (psi22 ns) (value i)
-- scanl13 ns od i = MoC.scanl13 (psi23 ns) (value i)
-- scanl14 ns od i = MoC.scanl14 (psi24 ns) (value i)
-- scanl21 ns od i = MoC.scanl21 (psi31 ns) (value i)
-- scanl22 ns od i = MoC.scanl22 (psi31 ns) (value i)
-- scanl23 ns od i = MoC.scanl23 (psi31 ns) (value i)
-- scanl24 ns od i = MoC.scanl24 (psi31 ns) (value i)
-- scanl31 ns od i = MoC.scanl31 (psi41 ns) (value i)
-- scanl32 ns od i = MoC.scanl32 (psi41 ns) (value i)
-- scanl33 ns od i = MoC.scanl33 (psi41 ns) (value i)
-- scanl34 ns od i = MoC.scanl34 (psi41 ns) (value i)
-- scanl41 ns od i = MoC.scanl41 (psi51 ns) (value i)
-- scanl42 ns od i = MoC.scanl42 (psi51 ns) (value i)
-- scanl43 ns od i = MoC.scanl43 (psi51 ns) (value i)
-- scanl44 ns od i = MoC.scanl44 (psi51 ns) (value i)


moore11 :: (st -> a1 -> st) -> (st -> b1) -> st
        -> Sig a1 -> Sig b1                                
moore12 :: (st -> a1 -> st) -> (st -> (b1, b2)) -> st
        -> Sig a1 -> (Sig b1, Sig b2)                          
moore13 :: (st -> a1 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
moore14 :: (st -> a1 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
moore21 :: (st -> a1 -> a2 -> st) -> (st -> b1) -> st
        -> Sig a1 -> Sig a2 -> Sig b1                          
moore22 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2)) -> st
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
moore23 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
moore24 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
moore31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> b1) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
moore32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
moore33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
moore34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
moore41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> b1) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
moore42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
moore43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
moore44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

moore11 ns od i = MoC.moore11 (psi21 ns) (psi11 od) (value i)
moore12 ns od i = MoC.moore12 (psi21 ns) (psi12 od) (value i)
moore13 ns od i = MoC.moore13 (psi21 ns) (psi13 od) (value i)
moore14 ns od i = MoC.moore14 (psi21 ns) (psi14 od) (value i)
moore21 ns od i = MoC.moore21 (psi31 ns) (psi11 od) (value i)
moore22 ns od i = MoC.moore22 (psi31 ns) (psi12 od) (value i)
moore23 ns od i = MoC.moore23 (psi31 ns) (psi13 od) (value i)
moore24 ns od i = MoC.moore24 (psi31 ns) (psi14 od) (value i)
moore31 ns od i = MoC.moore31 (psi41 ns) (psi11 od) (value i)
moore32 ns od i = MoC.moore32 (psi41 ns) (psi12 od) (value i)
moore33 ns od i = MoC.moore33 (psi41 ns) (psi13 od) (value i)
moore34 ns od i = MoC.moore34 (psi41 ns) (psi14 od) (value i)
moore41 ns od i = MoC.moore41 (psi51 ns) (psi11 od) (value i)
moore42 ns od i = MoC.moore42 (psi51 ns) (psi12 od) (value i)
moore43 ns od i = MoC.moore43 (psi51 ns) (psi13 od) (value i)
moore44 ns od i = MoC.moore44 (psi51 ns) (psi14 od) (value i)


mealy11 :: (st -> a1 -> st) -> (st -> a1 -> b1) -> st
        -> Sig a1 -> Sig b1                                
mealy12 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2)) -> st
        -> Sig a1 -> (Sig b1, Sig b2)                          
mealy13 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3)) -> st
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
mealy14 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
mealy21 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> b1) -> st
        -> Sig a1 -> Sig a2 -> Sig b1                          
mealy22 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2)) -> st
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
mealy23 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3)) -> st
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
mealy24 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
mealy31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> b1) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
mealy32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
mealy33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
mealy34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
mealy41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> b1) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
mealy42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
mealy43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
mealy44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> st
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

mealy11 ns od i = MoC.mealy11 (psi21 ns) (psi21 od) (value i)
mealy12 ns od i = MoC.mealy12 (psi21 ns) (psi22 od) (value i)
mealy13 ns od i = MoC.mealy13 (psi21 ns) (psi23 od) (value i)
mealy14 ns od i = MoC.mealy14 (psi21 ns) (psi24 od) (value i)
mealy21 ns od i = MoC.mealy21 (psi31 ns) (psi31 od) (value i)
mealy22 ns od i = MoC.mealy22 (psi31 ns) (psi32 od) (value i)
mealy23 ns od i = MoC.mealy23 (psi31 ns) (psi33 od) (value i)
mealy24 ns od i = MoC.mealy24 (psi31 ns) (psi34 od) (value i)
mealy31 ns od i = MoC.mealy31 (psi41 ns) (psi41 od) (value i)
mealy32 ns od i = MoC.mealy32 (psi41 ns) (psi42 od) (value i)
mealy33 ns od i = MoC.mealy33 (psi41 ns) (psi43 od) (value i)
mealy34 ns od i = MoC.mealy34 (psi41 ns) (psi44 od) (value i)
mealy41 ns od i = MoC.mealy41 (psi51 ns) (psi51 od) (value i)
mealy42 ns od i = MoC.mealy42 (psi51 ns) (psi52 od) (value i)
mealy43 ns od i = MoC.mealy43 (psi51 ns) (psi53 od) (value i)
mealy44 ns od i = MoC.mealy44 (psi51 ns) (psi54 od) (value i)

-- FILTER, FILL, HOLD


when :: (a -> Bool) -- ^ Predicate function
     -> Sig a       -- ^ Input signal
     -> Sig a       -- ^ Output signal
when p s = MoC.comb21 (replace Abst) (comb11 p s) s


filter :: (a -> Bool) -- ^ Predicate function
       -> Sig a       -- ^ Input signal
       -> Sig a       -- ^ Output signal
filter p s = MoC.comb21 (replace Undef) (comb11 p s) s

fill :: a -> Sig a -> Sig a
fill x s = MoC.comb21 (unsafeReplace (Value x)) (MoC.comb11 isAbsent s) s

-- -- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing as5ent values by the preceding present value. Only in s64es, where no preceding value exists, the as5ent value is replaced by a default value. The output signal is not any more of the type 'Arg'.
-- hold :: a -- ^Default value
--        -> Sig (Arg a) -- ^As5ent extended input signal
--        -> Sig a -- ^Output signa

-- fill   a = (-$-) (replaceAs5t a)
--   where replaceAs5t a' As5t     = a'
--         replaceAs5t _  (Prst x) = x
-- hold   a s1 = s
--   where s = holdf -$- (s ->- a) -*- s1
--         holdf a' As5t     = a'
--         holdf _  (Prst x) = x

