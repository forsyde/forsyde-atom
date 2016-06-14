-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @SY@ library implements the atoms holding the sematics for the
-- synchronous computation model. It also provides a set of helpers
-- for properly instantiating process network patterns as process
-- constructors.
--
-- 
-----------------------------------------------------------------------------

module ForSyDe.MoCLib.SY (

  -- * Synchronous (@SY@) event

  -- | According to <#lee98 [1]>, "two events are synchronous if they
  -- have the same tag, and two signals are synchronous if all events
  -- in one signal are synchronous to an event in the second signal
  -- and vice-versa. A system is synchronous if every signals in the
  -- system is synchronous to all the other signals in the system."
  --
  -- The synchronous (@SY@) MoC defines no notion of physical time,
  -- its tag system suggesting in fact the precedence among events. To
  -- further demystify its mechanisms, we can formulate the following
  -- proposition:
  --
  -- [The SY MoC] is abstracting the execution semantics of a system
  -- where computation is assumed to perform instantaneously (with
  -- zero delay), at certain synchronization points, when data is
  -- assumed to be available.
  --
  -- Below is a /possible/ behavior in time of the input and the
  -- output signals of a SY process, to illustrate these semantics:
  --
  -- <<includes/figs/sy-example.png>>
  --
  -- Implementing the SY tag system is straightforward if we consider
  -- signals as infinite lists. In this case the tags are implicitly
  -- defined by the position of events in a signal. In this case
  -- /t&#8320;/ would correspond with the event at the head of a
  -- signal /t&#8321;/ with the second event, and so on. Thus we do
  -- not need to explicitate tags, the only explicit argument passed
  -- to a SY event constructor being its value &#8712;
  -- /V&#8337;/. Thus we have the definition of the SY event
  -- implementing (among others) the 'MoC' class:
  
  SY(..),

  -- | For convenience we also provide a type alias for a SY signal
  -- which stands for "signal of synchronous events where the values
  -- are extended", and a couple of utilities:

  Sig, event, ForSyDe.MoCLib.SY.signal, 

  -- * @SY@ process constuctors

  -- | These SY-specific process constructors are basically
  -- specific instantiations of the network patterns defined in
  -- "ForSyDe.Core.MoC" also wrapping functions in a behavioural
  -- model as defined in "ForSyDe.Core.ValueExt".
  
  -- ** @comb@

  -- | @comb@ processes map combinatorial functions on signals and
  -- take care of synchronization between input signals. It
  -- instantiates the @combXY@ process constructor
  -- (eg. 'ForSyDe.Core.MoC.comb11').
  --
  -- <<includes/figs/sy-comb-graph.png>>

  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  -- ** @delay@

  -- | The @delay@ process "delays" a signal with one event. It is an
  -- instantiation of the 'ForSyDe.Core.MoC.delay' constructor.
  --
  -- <<includes/figs/sy-delay-graph.png>>
  delay,


  -- ** @constant@

  -- | A signal generator which keeps a value constant. It
  -- is actually an instantiation of the @scanl0X@ constructor
  -- (eg. 'ForSyDe.Core.MoC.scanl01').
  --
  -- <<includes/figs/sy-constant-graph.png>>
  
  constant1, constant2, constant3, constant4,
  
  -- ** @generate@

  -- | A signal generator based on a function and a kernel value. It
  -- is actually an instantiation of the @scanl0X@ constructor
  -- (eg. 'ForSyDe.Core.MoC.scanl01').
  --
  -- <<includes/figs/sy-generate-graph.png>>
  
  generate1, generate2, generate3, generate4,
  
  -- ** @scanl@

  -- | @scanl@ is a state machine without an output decoder. It is an
  -- instantiation of the @scanXY@ MoC constructor
  -- (eg. 'ForSyDe.Core.MoC.scanl11').
  --
  -- <<includes/figs/sy-scanl-graph.png>>

  scanl11, scanl12, scanl13, scanl14,
  scanl21, scanl22, scanl23, scanl24,
  scanl31, scanl32, scanl33, scanl34,
  scanl41, scanl42, scanl43, scanl44,
  
  -- ** @moore@

  -- | @moore@ processes model Moore state machines. It is an
  -- instantiation of the @mooreXY@ MoC constructor
  -- (eg. 'ForSyDe.Core.MoC.moore11').
  --
  -- <<includes/figs/sy-moore-graph.png>>

  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  -- ** @mealy@

  -- | @mealy@ processes model Mealy state machines. It is an
  -- instantiation of the @mealyXY@ MoC constructor
  -- (eg. 'ForSyDe.Core.MoC.mealy11').
  --
  -- <<includes/figs/sy-mealy-graph.png>>

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,

  -- ** Predicate processes

  -- | These processes manipulate the behavior of a signal based on
  -- predicates on their status.

  when, filter, fill, hold,
         
  -- * Bibliography

  -- | #lee98# [1] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /Computer-Aided Design of Integrated Circuits and Systems, IEEE Transactions on, 17(12)/, 1217-1229.
  
  -- | #hal91# [2] Halbwachs, N., Caspi, P., Raymond, P., & Pilaud, D. (1991). The synchronous data flow programming language LUSTRE. /Proceedings of the IEEE, 79(9)/, 1305-1320.
  
  ) where

import           Prelude hiding (filter)
import           ForSyDe.Core.MoC (MoC(..))
import qualified ForSyDe.Core.MoC as MoC
import           ForSyDe.Core.Signal as S
import           ForSyDe.Core.ValueExt hiding (value)

-- | Type alias for a SY signal
type Sig a = S.Signal (SY (Value a))

-- | The SY event. It identifies a synchronous signal and implemets an
-- instance of the 'MoC' class. Since SY tags are implicit, this data
-- type only wraps values inside a constructor that identifies it as a
-- "synchronous event".
data SY a  = SY a deriving Eq
-----------------------------------------------------------------------------

-- | Implenents the SY semantics for the MoC atoms.
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

-- | Shows the (extended) value wrapped
instance Show a => Show (SY a) where
  showsPrec _ (SY x) = (++) (show x)

-- | Reads the (extended) value wrapped
instance Read a => Read (SY a) where
  readsPrec _ s       = [(SY x, r) | (x, r) <- reads s]

-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor SY where
  fmap f (SY a) = SY (f a)

-- | Needed for the implementation of the '-*-' atom
instance Applicative SY where
  pure = SY 
  (SY a) <*> (SY b) = SY (a b)


-----------------------------------------------------------------------------

-- | Wraps a base value into a SY event of extended values
event    :: a -> SY (Value a)
event    = SY . Value

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal ((SY . Value) <$> l)

-----------------------------------------------------------------------------

delay :: a -> Sig a -> Sig a
delay i = MoC.delay (event i)

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

constant1 i = MoC.scanl01 (psi11 id) (event i)
constant2 i = MoC.scanl02 (psi11 id) (event i)
constant3 i = MoC.scanl03 (psi11 id) (event i)
constant4 i = MoC.scanl04 (psi11 id) (event i)


generate1 :: (b1 -> b1) -> b1
          -> Sig b1                                
generate2 :: ((b1, b2) -> (b1, b2)) -> (b1, b2)
          -> (Sig b1, Sig b2)                          
generate3 :: ((b1, b2, b3) -> (b1, b2, b3)) -> (b1, b2, b3)
          -> (Sig b1, Sig b2, Sig b3)                      
generate4 :: ((b1, b2, b3, b4) -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
          -> (Sig b1, Sig b2, Sig b3, Sig b4)                  

generate1 ns i = MoC.scanl01 (psi11 ns) (event i)
generate2 ns i = MoC.scanl02 (psi11 ns) (event i)
generate3 ns i = MoC.scanl03 (psi11 ns) (event i)
generate4 ns i = MoC.scanl04 (psi11 ns) (event i)


scanl11 :: (b1 -> a1 -> b1) -> b1
        -> Sig a1 -> Sig b1                                
scanl12 :: ((b1, b2) -> a1 -> (b1, b2)) -> (b1, b2)
        -> Sig a1 -> (Sig b1, Sig b2)                          
scanl13 :: ((b1, b2, b3) -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3)                      
scanl14 :: ((b1, b2, b3, b4) -> a1 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Sig a1 -> (Sig b1, Sig b2, Sig b3, Sig b4)                  
scanl21 :: (b1 -> a1 -> a2 -> b1) -> b1
        -> Sig a1 -> Sig a2 -> Sig b1                          
scanl22 :: ((b1, b2) -> a1 -> a2 -> (b1, b2)) -> (b1, b2)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2)                    
scanl23 :: ((b1, b2, b3) -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3)                
scanl24 :: ((b1, b2, b3, b4) -> a1 -> a2 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Sig a1 -> Sig a2 -> (Sig b1, Sig b2, Sig b3, Sig b4)                     
scanl31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> b1
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig b1                    
scanl32 :: ((b1, b2) -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2)              
scanl33 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3)          
scanl34 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Sig a1 -> Sig a2 -> Sig a3 -> (Sig b1, Sig b2, Sig b3, Sig b4)     
scanl41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> b1
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> Sig b1              
scanl42 :: ((b1, b2) -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (b1, b2)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2)        
scanl43 :: ((b1, b2, b3) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3)    
scanl44 :: ((b1, b2, b3, b4) -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Sig a1 -> Sig a2 -> Sig a3 -> Sig a4 -> (Sig b1, Sig b2, Sig b3, Sig b4)

scanl11 ns i = MoC.scanl11 (psi21 ns) (event i)
scanl12 ns i = MoC.scanl12 (psi21 ns) (event i)
scanl13 ns i = MoC.scanl13 (psi21 ns) (event i)
scanl14 ns i = MoC.scanl14 (psi21 ns) (event i)
scanl21 ns i = MoC.scanl21 (psi31 ns) (event i)
scanl22 ns i = MoC.scanl22 (psi31 ns) (event i)
scanl23 ns i = MoC.scanl23 (psi31 ns) (event i)
scanl24 ns i = MoC.scanl24 (psi31 ns) (event i)
scanl31 ns i = MoC.scanl31 (psi41 ns) (event i)
scanl32 ns i = MoC.scanl32 (psi41 ns) (event i)
scanl33 ns i = MoC.scanl33 (psi41 ns) (event i)
scanl34 ns i = MoC.scanl34 (psi41 ns) (event i)
scanl41 ns i = MoC.scanl41 (psi51 ns) (event i)
scanl42 ns i = MoC.scanl42 (psi51 ns) (event i)
scanl43 ns i = MoC.scanl43 (psi51 ns) (event i)
scanl44 ns i = MoC.scanl44 (psi51 ns) (event i)


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

moore11 ns od i = MoC.moore11 (psi21 ns) (psi11 od) (event i)
moore12 ns od i = MoC.moore12 (psi21 ns) (psi12 od) (event i)
moore13 ns od i = MoC.moore13 (psi21 ns) (psi13 od) (event i)
moore14 ns od i = MoC.moore14 (psi21 ns) (psi14 od) (event i)
moore21 ns od i = MoC.moore21 (psi31 ns) (psi11 od) (event i)
moore22 ns od i = MoC.moore22 (psi31 ns) (psi12 od) (event i)
moore23 ns od i = MoC.moore23 (psi31 ns) (psi13 od) (event i)
moore24 ns od i = MoC.moore24 (psi31 ns) (psi14 od) (event i)
moore31 ns od i = MoC.moore31 (psi41 ns) (psi11 od) (event i)
moore32 ns od i = MoC.moore32 (psi41 ns) (psi12 od) (event i)
moore33 ns od i = MoC.moore33 (psi41 ns) (psi13 od) (event i)
moore34 ns od i = MoC.moore34 (psi41 ns) (psi14 od) (event i)
moore41 ns od i = MoC.moore41 (psi51 ns) (psi11 od) (event i)
moore42 ns od i = MoC.moore42 (psi51 ns) (psi12 od) (event i)
moore43 ns od i = MoC.moore43 (psi51 ns) (psi13 od) (event i)
moore44 ns od i = MoC.moore44 (psi51 ns) (psi14 od) (event i)


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

mealy11 ns od i = MoC.mealy11 (psi21 ns) (psi21 od) (event i)
mealy12 ns od i = MoC.mealy12 (psi21 ns) (psi22 od) (event i)
mealy13 ns od i = MoC.mealy13 (psi21 ns) (psi23 od) (event i)
mealy14 ns od i = MoC.mealy14 (psi21 ns) (psi24 od) (event i)
mealy21 ns od i = MoC.mealy21 (psi31 ns) (psi31 od) (event i)
mealy22 ns od i = MoC.mealy22 (psi31 ns) (psi32 od) (event i)
mealy23 ns od i = MoC.mealy23 (psi31 ns) (psi33 od) (event i)
mealy24 ns od i = MoC.mealy24 (psi31 ns) (psi34 od) (event i)
mealy31 ns od i = MoC.mealy31 (psi41 ns) (psi41 od) (event i)
mealy32 ns od i = MoC.mealy32 (psi41 ns) (psi42 od) (event i)
mealy33 ns od i = MoC.mealy33 (psi41 ns) (psi43 od) (event i)
mealy34 ns od i = MoC.mealy34 (psi41 ns) (psi44 od) (event i)
mealy41 ns od i = MoC.mealy41 (psi51 ns) (psi51 od) (event i)
mealy42 ns od i = MoC.mealy42 (psi51 ns) (psi52 od) (event i)
mealy43 ns od i = MoC.mealy43 (psi51 ns) (psi53 od) (event i)
mealy44 ns od i = MoC.mealy44 (psi51 ns) (psi54 od) (event i)


buffer1 :: [a] -> Sig a -> Sig [a]
buffer2 :: [a] -> Sig a -> Sig a -> Sig [a]
buffer3 :: [a] -> Sig a -> Sig a -> Sig a -> Sig [a]
buffer4 :: [a] -> Sig a -> Sig a -> Sig a -> Sig a -> Sig [a]

buffer1 i = MoC.scanl11 store1 (event i)
buffer2 i = MoC.scanl21 store2 (event i)
buffer3 i = MoC.scanl31 store3 (event i)
buffer4 i = MoC.scanl41 store4 (event i)


-- FILTER, FILL, HOLD

-- | This process predicates the existence of events in a signal based
-- on a signal of boolean values (conditions). It is similar to the
-- @when@ construct in the synchronous language Lustre <#hal91 [2]>,
-- based on which clock calculus can be performed.
--
-- >>> let s = signal [1,2,3,4,5,6]
-- >>> let p = comb11 (>3) s
-- >>> s
-- {1,2,3,4,5,6}
-- >>> p
-- {False,False,False,True,True,True}
-- >>> when p s
-- {⟂,⟂,⟂,4,5,6}
when :: Sig Bool    -- ^ Signal of predicates
     -> Sig a       -- ^ Input signal
     -> Sig a       -- ^ Output signal
when p s = MoC.comb21 (replaceA . psi11 not) p s

-- | Filters out values to unknown if they do not fulfill a predicate
-- function.
--
-- >>> let s = signal [1,2,3,4,5,6]
-- >>> filter (>3) s
-- {?,?,?,4,5,6}
filter :: (a -> Bool) -- ^ Predicate function
       -> Sig a       -- ^ Input signal
       -> Sig a       -- ^ Output signal
filter p s = MoC.comb21 replaceU (comb11 p s) s

-- | Fills absent events with a pre-defined value. 
--
-- >>> let s = filter (>3) $ signal [1,2,3,4,5,6]
-- >>> fill 5 s
-- {5,5,5,4,5,6}
fill :: a -> Sig a -> Sig a
fill x s = MoC.comb21 (unsafeReplaceV (Value x)) (MoC.comb11 isAbsent s) s


-- | Similar to 'fill', but holds the last non-absent value if there
-- was one.
--
-- >>> let s = filter (<3) $ signal [1,2,3,4,5,6]
-- >>> fill 5 s
-- {1,2,3,3,3,3}
hold :: a -> Sig a -> Sig a
hold init = MoC.scanl11 fillF (event init)
  where fillF st inp = (unsafeReplaceV st) (isAbsent inp) inp


