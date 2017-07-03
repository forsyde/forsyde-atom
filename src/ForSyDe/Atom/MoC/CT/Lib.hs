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

import qualified ForSyDe.Atom.MoC as MoC
import           ForSyDe.Atom.MoC.CT.Core
import           ForSyDe.Atom.MoC.TimeStamp
import           ForSyDe.Atom.Utility
import           Prelude hiding (const)


------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)
-- >>> import qualified Data.Number.FixedFunctions as RatF
-- >>> let pi'  = realToFrac pi
-- >>> let sin' = RatF.sin 0.001
-- >>> let cos' = RatF.cos 0.001

------- DELAY -------

-- | The @delay@ process "delays" a signal with one
-- event. Instantiates the 'ForSyDe.Atom.MoC.delay' pattern. In the CT
-- MoC, this process can be interpreted as a "delay line" (e.g. an
-- ideal capacitor).
--
-- >>> let s  = infinite (sin')
-- >>> let s' = delay 2 (\_ -> 0) s
-- >>> plot 0.5 5 s'
-- {0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,473 % 985,132 % 157,783 % 785,2881 % 3169,54340 % 90821}
--
-- <<docfiles/figs/moc-ct-pattern-delay.png>>
delay :: TimeStamp   -- ^ time delay
      -> (Time -> a) -- ^ initial value
      -> Signal a    -- ^ input signal
      -> Signal a    -- ^ output signal

delay t v = MoC.delay (unit (t, v))

-- | Similar to the previous, but this is the raw instantiation of the
-- 'ForSyDe.Atom.MoC.delay' pattern. It "borrows" the first event from
-- one signal and appends it at the head of another signal.
--
-- >>> let s  = infinite (sin')
-- >>> let s' = signal [(0, \_ -> 0), (2, \_ -> 1)]
-- >>> plot 0.5 5 $ delay' s' s
-- {0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,473 % 985,132 % 157,783 % 785,2881 % 3169,54340 % 90821}
--
-- <<docfiles/figs/moc-ct-pattern-delayp.png>>
delay' :: Signal a  -- ^ signal "borrowing" the initial event
      -> Signal a   -- ^ input signal
      -> Signal a   -- ^ output signal

delay' = MoC.delay

------- COMB -------

-- | @comb@ processes map combinatorial functions on signals and take
-- care of synchronization between input signals. It instantiates the
-- @comb@ pattern (see 'ForSyDe.Atom.MoC.comb22').
-- 
-- The following constructors are provided:
--
-- > comb11, comb12, comb13, comb14,
-- > comb21, comb22, comb23, comb24,
-- > comb31, comb32, comb33, comb34,
-- > comb41, comb42, comb43, comb44,
--
-- >>> let s1 = infinite (sin')
-- >>> let s2 = signal [(0,\_->0),(pi',\_->1),(2*pi',\_->0),(3*pi',\_->1)]
-- >>> plot 0.5 5 $ comb11 (+1) s2
-- {1,1,1,1,1,1,1,2,2,2}
-- >>> plot2 0.5 5 $ ForSyDe.Atom.MoC.CT.Lib.comb22 (\a b-> (a+b,a*b)) s1 s2
-- ({0 % 1,473 % 985,132 % 157,783 % 785,2881 % 3169,54340 % 90821,33859 % 239941,5408 % 8329,512 % 2105,25 % 1117},{0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,(-2921) % 8329,(-1593) % 2105,(-1092) % 1117})
--
-- <<docfiles/figs/moc-ct-pattern-comb.png>>
comb22 :: (a1 -> a2 -> (b1, b2)) -- ^ function on values
       -> Signal a1                 -- ^ first input signal
       -> Signal a2                 -- ^ second input signal
       -> (Signal b1, Signal b2)       -- ^ two output signals
comb11 :: (a1 -> b1)
       -> Signal a1 -> Signal b1                                
comb12 :: (a1 -> (b1, b2))
       -> Signal a1 -> (Signal b1, Signal b2)                          
comb13 :: (a1 -> (b1, b2, b3))
       -> Signal a1 -> (Signal b1, Signal b2, Signal b3)                      
comb14 :: (a1 -> (b1, b2, b3, b4))
       -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)                  
comb21 :: (a1 -> a2 -> b1)
       -> Signal a1 -> Signal a2 -> Signal b1                          
comb23 :: (a1 -> a2 -> (b1, b2, b3))
       -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)                
comb24 :: (a1 -> a2 -> (b1, b2, b3, b4))
       -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3, Signal b4)            
comb31 :: (a1 -> a2 -> a3 -> b1)
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1                    
comb32 :: (a1 -> a2 -> a3 -> (b1, b2))
       -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)              
comb33 :: (a1 -> a2 -> a3 -> (b1, b2, b3))
       -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)          
comb34 :: (a1 -> a2 -> a3 -> (b1, b2, b3, b4))
       -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3, Signal b4)     
comb41 :: (a1 -> a2 -> a3 -> a4 -> b1)
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> Signal b1              
comb42 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2)        
comb43 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3)    
comb44 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3, Signal b4)

comb11 = MoC.comb11 
comb12 = MoC.comb12 
comb13 = MoC.comb13 
comb14 = MoC.comb14 
comb21 = MoC.comb21 
comb22 = MoC.comb22 
comb23 = MoC.comb23 
comb24 = MoC.comb24 
comb31 = MoC.comb31 
comb32 = MoC.comb32 
comb33 = MoC.comb33 
comb34 = MoC.comb34 
comb41 = MoC.comb41 
comb42 = MoC.comb42 
comb43 = MoC.comb43 
comb44 = MoC.comb44 


------- CONSTANT -------

-- | A generator for a constant signal. As compared with the
-- 'ForSyDe.Atom.MoC.SY.constant2', it just constructs an infinite
-- signal with constant value (i.e. a signal with one event starting
-- from time 0).
--
-- The following constructors are provided:
--
-- > constant1, constant2, constant3, constant4,
--
-- >>> plot 0.5 5 $ constant1 2
-- {2,2,2,2,2,2,2,2,2,2}
--
-- <<docfiles/figs/moc-ct-pattern-constant.png>>
constant2 :: (b1, b2)         -- ^ values to be repeated
          -> (Signal b1, Signal b2) -- ^ generated signals
constant1 :: b1 -> Signal b1                                
constant3 :: (b1, b2, b3)
          -> (Signal b1, Signal b2, Signal b3)
constant4 :: (b1, b2, b3, b4)
          -> (Signal b1, Signal b2, Signal b3, Signal b4)

constant1 = infinite . const 
constant2 = ($$) (constant1, constant1)
constant3 = ($$$) (constant1,constant1,constant1)
constant4 = ($$$$) (constant1,constant1,constant1,constant1)


------- INFINITE -------

-- | A generator for an infinite signal. Similar to 'constant2'.
--
-- The following constructors are provided:
--
-- > infinite1, infinite2, infinite3, infinite4,
--
-- >>> plot2 0.5 5 $ infinite2 (sin', cos')
-- ({0 % 1,473 % 985,132 % 157,783 % 785,2881 % 3169,54340 % 90821,33859 % 239941,(-2921) % 8329,(-1593) % 2105,(-1092) % 1117},{1 % 1,864 % 985,85 % 157,56 % 785,(-1320) % 3169,(-72771) % 90821,(-237540) % 239941,(-7800) % 8329,(-1376) % 2105,(-235) % 1117})
--
-- <<docfiles/figs/moc-ct-pattern-infinite.png>>
infinite2 :: (Time -> b1, Time -> b2)         -- ^ values to be repeated
          -> (Signal b1, Signal b2) -- ^ generated signals
infinite1 :: (Time -> b1) -> Signal b1                                
infinite3 :: (Time -> b1, Time -> b2, Time -> b3)
          -> (Signal b1, Signal b2, Signal b3)
infinite4 :: (Time -> b1, Time -> b2, Time -> b3, Time -> b4)
          -> (Signal b1, Signal b2, Signal b3, Signal b4)

infinite1 = infinite
infinite2 = ($$) (infinite, infinite)
infinite3 = ($$$) (infinite,infinite,infinite)
infinite4 = ($$$$) (infinite,infinite,infinite,infinite)

------- GENERATE -------

-- | A signal generator based on a function and a kernel value. It
-- is actually an instantiation of the @stated0X@ constructor
-- (check 'ForSyDe.Atom.MoC.stated22').
--
-- The following constructors are provided:
--
-- > generate1, generate2, generate3, generate4,
--
-- >>> let { osc 0 = 1 ; osc 1 = 0 }
-- >>> plot 1 10 $ generate1 osc (pi', \_->0)
-- {0,0,0,0,1,1,1,0,0,0}
--
-- <<docfiles/figs/moc-ct-pattern-generate.png>>
--
-- Another example simulating an RC oscillator:
--
-- >>> let vs = 2      -- Vs : supply voltage
-- >>> let r  = 100    -- R : resistance
-- >>> let c  = 0.0005 -- C : capacitance
-- >>> let vc t = vs * (1 - exp 1 ** ((-fromRational t) / (r * c))) -- Vc(t) : voltage charging through capacitor
-- >>> let ns v = vs + (-1 * v)
-- >>> let rcOsc = generate1 ns (milisec 500, vc)
-- >>> plot 0.2 2 rcOsc
-- {0.0,1.9633687222225316,1.999329074744195,0.2706705664732254,4.957504353332753e-3,0.0,1.9633687222225316,1.999329074744195,0.2706705664732254,4.957504353332753e-3}
--
-- <<docfiles/figs/moc-ct-pattern-generate1.png>>
generate2 :: (b1 -> b2 -> (b1, b2))
          -- ^ function to generate next value
          -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
          -- ^ kernel values tupled with their generation rate.
          -> (Signal b1, Signal b2) -- ^ generated signals
generate1 :: (b1 -> b1) -> (TimeStamp, Time -> b1)
          -> Signal b1                                
generate3 :: (b1 -> b2 -> b3 -> (b1, b2, b3))
          -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
          -> (Signal b1, Signal b2, Signal b3)                      
generate4 :: (b1 -> b2 -> b3 -> b4 -> (b1, b2, b3, b4))
          -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
          -> (Signal b1, Signal b2, Signal b3, Signal b4)                  

generate1 ns i = MoC.stated01 ns (unit  i)
generate2 ns i = MoC.stated02 ns (unit2 i)
generate3 ns i = MoC.stated03 ns (unit3 i)
generate4 ns i = MoC.stated04 ns (unit4 i)

------- STATED -------

-- | @stated@ is a state machine without an output decoder. It is an
-- instantiation of the @state@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.stated22').
--
-- The following constructors are provided:
--
-- > stated11, stated12, stated13, stated14,
-- > stated21, stated22, stated23, stated24,
-- > stated31, stated32, stated33, stated34,
-- > stated41, stated42, stated43, stated44,
--
-- >>> let { osc 0 a = a; osc _ a = 0 }   
-- >>> let s1 = signal [(0,\_->1), (6,\_->0)]
-- >>> plot 0.5 10 $ stated11 osc (1,\_->0) s1
-- {0,0,1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0}
--
-- <<docfiles/figs/moc-ct-pattern-stated.png>>
stated22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2))
            -- ^ next state function
           -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
           -- ^ initial state values tupled with their initial delay
            -> Signal a1
            -- ^ first input signal
            -> Signal a2
            -- ^ second input signal
            -> (Signal b1, Signal b2) -- ^ output signals
stated11 :: (b1 -> a1 -> b1)
         -> (TimeStamp, Time -> b1)
         -> Signal a1
         -> Signal b1 
stated12 :: (b1 -> b2 -> a1 -> (b1, b2))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
         -> Signal a1
         -> (Signal b1, Signal b2) 
stated13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
         -> Signal a1
         -> (Signal b1, Signal b2, Signal b3) 
stated14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
         -> Signal a1
         -> (Signal b1, Signal b2, Signal b3, Signal b4) 
stated21 :: (b1 -> a1 -> a2 -> b1)
         -> (TimeStamp, Time -> b1)
         -> Signal a1 -> Signal a2
         -> Signal b1 
stated23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3) 
stated24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3, Signal b4) 
stated31 :: (b1 -> a1 -> a2 -> a3 -> b1)
         -> (TimeStamp, Time -> b1)
         -> Signal a1 -> Signal a2 -> Signal a3
         -> Signal b1 
stated32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2) 
stated33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3) 
stated34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3, Signal b4) 
stated41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1)
         -> (TimeStamp, Time -> b1)
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> Signal b1 
stated42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2) 
stated43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3) 
stated44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
         -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3, Signal b4)

stated11 ns i = MoC.stated11 ns (unit  i)
stated12 ns i = MoC.stated12 ns (unit2 i)
stated13 ns i = MoC.stated13 ns (unit3 i)
stated14 ns i = MoC.stated14 ns (unit4 i)
stated21 ns i = MoC.stated21 ns (unit  i)
stated22 ns i = MoC.stated22 ns (unit2 i)
stated23 ns i = MoC.stated23 ns (unit3 i)
stated24 ns i = MoC.stated24 ns (unit4 i)
stated31 ns i = MoC.stated31 ns (unit  i)
stated32 ns i = MoC.stated32 ns (unit2 i)
stated33 ns i = MoC.stated33 ns (unit3 i)
stated34 ns i = MoC.stated34 ns (unit4 i)
stated41 ns i = MoC.stated41 ns (unit  i)
stated42 ns i = MoC.stated42 ns (unit2 i)
stated43 ns i = MoC.stated43 ns (unit3 i)
stated44 ns i = MoC.stated44 ns (unit4 i)

------- STATE -------

-- | @state@ is a state machine without an output decoder, and the
-- state non-transparent. It is an instantiation of the @state@ MoC
-- constructor (see 'ForSyDe.Atom.MoC.state22').
--
-- The following constructors are provided:
--
-- > state11, state12, state13, state14,
-- > state21, state22, state23, state24,
-- > state31, state32, state33, state34,
-- > state41, state42, state43, state44,
--
-- >>> let { osc 0 a = a; osc _ a = 0 }   
-- >>> let s1 = signal [(0,\_->1), (6,\_->0)]
-- >>> plot 0.5 10 $ state11 osc (1,\_->0) s1
-- {1,1,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0}
--
-- <<docfiles/figs/moc-ct-pattern-state.png>>                   
state22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2))
           -- ^ next state function
           -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
           -- ^ initial state values tupled with their initial delay
           -> Signal a1
           -- ^ first input signal
           -> Signal a2
           -- ^ second input signal
           -> (Signal b1, Signal b2) -- ^ output signals
state11 :: (b1 -> a1 -> b1)
        -> (TimeStamp, Time -> b1)
        -> Signal a1
        -> Signal b1                                
state12 :: (b1 -> b2 -> a1 -> (b1, b2)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
        -> Signal a1
        -> (Signal b1, Signal b2)                          
state13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3)                      
state14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
        -> Signal a1 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)                  
state21 :: (b1 -> a1 -> a2 -> b1)
        -> (TimeStamp, Time -> b1)
        -> Signal a1 -> Signal a2
        -> Signal b1                          
state23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
        -> Signal a1 -> Signal a2 
        -> (Signal b1, Signal b2, Signal b3)                
state24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
        -> Signal a1 -> Signal a2 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)                     
state31 :: (b1 -> a1 -> a2 -> a3 -> b1)
        -> (TimeStamp, Time -> b1)
        -> Signal a1 -> Signal a2 -> Signal a3
        -> Signal b1                    
state32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
        -> Signal a1 -> Signal a2 -> Signal a3 
        -> (Signal b1, Signal b2)              
state33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
        -> Signal a1 -> Signal a2 -> Signal a3 
        -> (Signal b1, Signal b2, Signal b3)          
state34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
        -> Signal a1 -> Signal a2 -> Signal a3 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)     
state41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1)
        -> (TimeStamp, Time -> b1)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> Signal b1
state42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2))
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 
        -> (Signal b1, Signal b2)        
state43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3))
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 
        -> (Signal b1, Signal b2, Signal b3)    
state44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) 
        -> ((TimeStamp, Time -> b1), (TimeStamp, Time -> b2), (TimeStamp, Time -> b3), (TimeStamp, Time -> b4))
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)

state11 ns i = MoC.state11 ns (unit  i)
state12 ns i = MoC.state12 ns (unit2 i)
state13 ns i = MoC.state13 ns (unit3 i)
state14 ns i = MoC.state14 ns (unit4 i)
state21 ns i = MoC.state21 ns (unit  i)
state22 ns i = MoC.state22 ns (unit2 i)
state23 ns i = MoC.state23 ns (unit3 i)
state24 ns i = MoC.state24 ns (unit4 i)
state31 ns i = MoC.state31 ns (unit  i)
state32 ns i = MoC.state32 ns (unit2 i)
state33 ns i = MoC.state33 ns (unit3 i)
state34 ns i = MoC.state34 ns (unit4 i)
state41 ns i = MoC.state41 ns (unit  i)
state42 ns i = MoC.state42 ns (unit2 i)
state43 ns i = MoC.state43 ns (unit3 i)
state44 ns i = MoC.state44 ns (unit4 i)


------- MOORE -------

-- | @moore@ processes model Moore state machines. It is an
-- instantiation of the @moore@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.moore22').
--
-- The following constructors are provided:
--
-- > moore11, moore12, moore13, moore14,
-- > moore21, moore22, moore23, moore24,
-- > moore31, moore32, moore33, moore34,
-- > moore41, moore42, moore43, moore44,
--
-- >>> let { osc 0 a = a; osc _ a = 0 }   
-- >>> let s1 = signal [(0,\_->1), (6,\_->0)]
-- >>> plot 0.5 10 $ moore11 osc (*3) (1,\_->0) s1
-- {0,0,3,3,0,0,3,3,0,0,3,3,0,0,0,0,0,0,0,0}
--
-- <<docfiles/figs/moc-ct-pattern-moore.png>>          
moore22 :: (st -> a1 -> a2 -> st)
           -- ^ next state function
           -> (st -> (b1, b2))
           -- ^ output decoder
           -> (TimeStamp, Time -> st)
           -- ^ initial state: tag and value
           -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2)
moore11 :: (st -> a1 -> st)
        -> (st -> b1)
        -> (TimeStamp, Time -> st)
        -> Signal a1
        -> Signal b1                                
moore12 :: (st -> a1 -> st)
        -> (st -> (b1, b2))
        -> (TimeStamp, Time -> st)
        -> Signal a1
        -> (Signal b1, Signal b2)                          
moore13 :: (st -> a1 -> st)
        -> (st -> (b1, b2, b3))
        -> (TimeStamp, Time -> st)
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3)                      
moore14 :: (st -> a1 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (TimeStamp, Time -> st)
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3, Signal b4)                  
moore21 :: (st -> a1 -> a2 -> st)
        -> (st -> b1)
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2
        -> Signal b1                          
moore23 :: (st -> a1 -> a2 -> st)
        -> (st -> (b1, b2, b3))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3)                
moore24 :: (st -> a1 -> a2 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3, Signal b4)                     
moore31 :: (st -> a1 -> a2 -> a3 -> st)
        -> (st -> b1)
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3
        -> Signal b1                    
moore32 :: (st -> a1 -> a2 -> a3 -> st)
        -> (st -> (b1, b2))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2)              
moore33 :: (st -> a1 -> a2 -> a3 -> st)
        -> (st -> (b1, b2, b3))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3)          
moore34 :: (st -> a1 -> a2 -> a3 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3, Signal b4)     
moore41 :: (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> b1)
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> Signal b1              
moore42 :: (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> (b1, b2))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2)        
moore43 :: (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> (b1, b2, b3))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3)    
moore44 :: (st -> a1 -> a2 -> a3 -> a4 -> st)
        -> (st -> (b1, b2, b3, b4))
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3, Signal b4)

moore11 ns od i = MoC.moore11 ns od (unit i)
moore12 ns od i = MoC.moore12 ns od (unit i)
moore13 ns od i = MoC.moore13 ns od (unit i)
moore14 ns od i = MoC.moore14 ns od (unit i)
moore21 ns od i = MoC.moore21 ns od (unit i)
moore22 ns od i = MoC.moore22 ns od (unit i)
moore23 ns od i = MoC.moore23 ns od (unit i)
moore24 ns od i = MoC.moore24 ns od (unit i)
moore31 ns od i = MoC.moore31 ns od (unit i)
moore32 ns od i = MoC.moore32 ns od (unit i)
moore33 ns od i = MoC.moore33 ns od (unit i)
moore34 ns od i = MoC.moore34 ns od (unit i)
moore41 ns od i = MoC.moore41 ns od (unit i)
moore42 ns od i = MoC.moore42 ns od (unit i)
moore43 ns od i = MoC.moore43 ns od (unit i)
moore44 ns od i = MoC.moore44 ns od (unit i)

------- MEALY -------

-- | @mealy@ processes model Mealy state machines. It is an
-- instantiation of the @mealy@ MoC constructor
-- (see 'ForSyDe.Atom.MoC.mealy22').
--
-- The following constructors are provided:
--
-- > mealy11, mealy12, mealy13, mealy14,
-- > mealy21, mealy22, mealy23, mealy24,
-- > mealy31, mealy32, mealy33, mealy34,
-- > mealy41, mealy42, mealy43, mealy44,
--
-- >>> let { osc (-1) _ = 1; osc 1 _ = (-1) } 
-- >>> let s1 = infinite sin'
-- >>> plotFloat 0.5 5 $ mealy11 osc (*) (pi',\_->1) s1
-- {0.0,0.48020304568527916,0.8407643312101911,0.9974522292993631,0.9091195960870937,0.5983197718589313,0.14111385715655098,0.35070236522991954,0.7567695961995249,0.9776186213070726}
--
-- <<docfiles/figs/moc-ct-pattern-mealy.png>>
mealy22 :: (st -> a1 -> a2 -> st)
        -- ^ next state function
        -> (st -> a1 -> a2 -> (b1, b2))
        -- ^ outpt decoder
        -> (TimeStamp, Time -> st)
        -- ^ initial state: tag and value
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2)
mealy11 :: (st -> a1 -> st) 
        -> (st -> a1 -> b1) 
        -> (TimeStamp, Time -> st)
        -> Signal a1
        -> Signal b1                                
mealy12 :: (st -> a1 -> st) 
        -> (st -> a1 -> (b1, b2)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 
        -> (Signal b1, Signal b2)                          
mealy13 :: (st -> a1 -> st) 
        -> (st -> a1 -> (b1, b2, b3)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 
        -> (Signal b1, Signal b2, Signal b3)                      
mealy14 :: (st -> a1 -> st) 
        -> (st -> a1 -> (b1, b2, b3, b4)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)                  
mealy21 :: (st -> a1 -> a2 -> st) 
        -> (st -> a1 -> a2 -> b1) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2
        -> Signal b1                          
mealy23 :: (st -> a1 -> a2 -> st) 
        -> (st -> a1 -> a2 -> (b1, b2, b3)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 
        -> (Signal b1, Signal b2, Signal b3)                
mealy24 :: (st -> a1 -> a2 -> st) 
        -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)                     
mealy31 :: (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> b1) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3
        -> Signal b1  
mealy32 :: (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> (b1, b2)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 
        -> (Signal b1, Signal b2)              
mealy33 :: (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 
        -> (Signal b1, Signal b2, Signal b3)          
mealy34 :: (st -> a1 -> a2 -> a3 -> st) 
        -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)     
mealy41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> b1) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> Signal b1
mealy42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 
        -> (Signal b1, Signal b2)        
mealy43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 
        -> (Signal b1, Signal b2, Signal b3)    
mealy44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) 
        -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) 
        -> (TimeStamp, Time -> st)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 
        -> (Signal b1, Signal b2, Signal b3, Signal b4)

mealy11 ns od i = MoC.mealy11 ns od (unit i)
mealy12 ns od i = MoC.mealy12 ns od (unit i)
mealy13 ns od i = MoC.mealy13 ns od (unit i)
mealy14 ns od i = MoC.mealy14 ns od (unit i)
mealy21 ns od i = MoC.mealy21 ns od (unit i)
mealy22 ns od i = MoC.mealy22 ns od (unit i)
mealy23 ns od i = MoC.mealy23 ns od (unit i)
mealy24 ns od i = MoC.mealy24 ns od (unit i)
mealy31 ns od i = MoC.mealy31 ns od (unit i)
mealy32 ns od i = MoC.mealy32 ns od (unit i)
mealy33 ns od i = MoC.mealy33 ns od (unit i)
mealy34 ns od i = MoC.mealy34 ns od (unit i)
mealy41 ns od i = MoC.mealy41 ns od (unit i)
mealy42 ns od i = MoC.mealy42 ns od (unit i)
mealy43 ns od i = MoC.mealy43 ns od (unit i)
mealy44 ns od i = MoC.mealy44 ns od (unit i)

------- SYNC -------

-- | @sync@ synchronizes multiple signals, so that they have the same
-- set of tags, and consequently, the same number of events. It
-- instantiates the @comb@ atom pattern (see
-- 'ForSyDe.Atom.MoC.comb22').
--
-- The following constructors are provided:
--
-- > sync2, sync3, sync4,
sync2 :: Signal a1                    -- ^ first input signal
      -> Signal a2                    -- ^ second input signal
      -> (Signal a1, Signal a2)       -- ^ two output signals
sync3 :: Signal a1 -> Signal a2 -> Signal a3
      -> (Signal a1, Signal a2, Signal a3)             
sync4 :: Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
      -> (Signal a1, Signal a2, Signal a3, Signal a4)

sync2 = comb22 (,)
sync3 = comb33 (,,)
sync4 = comb44 (,,,)
