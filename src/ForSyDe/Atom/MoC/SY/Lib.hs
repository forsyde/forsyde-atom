{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Lib
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
module ForSyDe.Atom.MoC.SY.Lib where


import qualified ForSyDe.Atom.ExB as B
import           ForSyDe.Atom.ExB.Absent (AbstExt(..))
import qualified ForSyDe.Atom.MoC as MoC
import           ForSyDe.Atom.MoC.SY.Core
import           Prelude hiding (filter)


------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)

------- DELAY -------

-- | The @delay@ process "delays" a signal with one
-- event. Instantiates the 'ForSyDe.Atom.MoC.delay' pattern.
--
-- >>> let s = signal [1,2,3,4,5]
-- >>> delay 0 s
-- {0,1,2,3,4,5}
--
-- <<fig/moc-sy-pattern-delay.png>>
delay :: a     -- ^ initial value
      -> Signal a -- ^ input signal
      -> Signal a -- ^ output signal
delay i = MoC.delay (unit i)

------- COMB -------

-- | @comb@ processes map combinatorial functions on signals and take care of
-- synchronization between input signals. It implements the @comb@ pattern (see
-- 'ForSyDe.Atom.MoC.comb22').
-- 
-- Constructors: @comb[1-4][1-4]@.
--
-- >>> let s1 = signal [1..]
-- >>> let s2 = signal [1,1,1,1,1]
-- >>> comb11 (+1) s2
-- {2,2,2,2,2}
-- >>> comb22 (\a b-> (a+b,a-b)) s1 s2
-- ({2,3,4,5,6},{0,1,2,3,4})
--
-- <<fig/moc-sy-pattern-comb.png>>
comb22 :: (a1 -> a2 -> (b1, b2)) -- ^ function on values
       -> Signal a1              -- ^ first input signal
       -> Signal a2              -- ^ second input signal
       -> (Signal b1, Signal b2) -- ^ two output signals
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

------- RECONFIG -------

-- | @reconfig@ creates an synchronous adaptive process where the first signal carries
-- functions and the other carry the arguments. It imlements the @reconfig@ pattern
-- (see 'ForSyDe.Atom.MoC.reconfig22').
--
-- Constructors: @reconfig[1-4][1-4]@.
--
-- >>> let sf = signal [(+1),(*2),(+1),(*2),(+1),(*2),(+1)] 
-- >>> let s1 = signal [1..]
-- >>> reconfig11 sf s1
-- {2,4,4,8,6,12,8}
--
-- <<fig/moc-sy-pattern-reconfig.png>>
reconfig22 :: Signal (a1 -> a2 -> (b1, b2))
           -- ^ signal carrying functions
           -> Signal a1
           -- ^ first input signal carrying arguments
           -> Signal a2
           -- ^ second input signal carrying arguments
           -> (Signal b1, Signal b2) -- ^ two output signals
reconfig11 :: Signal (a1 -> b1)
           -> Signal a1 -> Signal b1
reconfig12 :: Signal(a1 -> (b1, b2))
           -> Signal a1 -> (Signal b1, Signal b2)
reconfig13 :: Signal(a1 -> (b1, b2, b3))
           -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
reconfig14 :: Signal(a1 -> (b1, b2, b3, b4))
           -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
reconfig21 :: Signal(a1 -> a2 -> b1)
           -> Signal a1 -> Signal a2 -> Signal b1
reconfig23 :: Signal(a1 -> a2 -> (b1, b2, b3))
           -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
reconfig24 :: Signal(a1 -> a2 -> (b1, b2, b3, b4))
           -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3, Signal b4)
reconfig31 :: Signal(a1 -> a2 -> a3 -> b1)
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
reconfig32 :: Signal(a1 -> a2 -> a3 -> (b1, b2))
           -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
reconfig33 :: Signal(a1 -> a2 -> a3 -> (b1, b2, b3))
           -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)
reconfig34 :: Signal(a1 -> a2 -> a3 -> (b1, b2, b3, b4))
           -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3, Signal b4)
reconfig41 :: Signal(a1 -> a2 -> a3 -> a4 -> b1)
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> Signal b1
reconfig42 :: Signal(a1 -> a2 -> a3 -> a4 -> (b1, b2))
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2)
reconfig43 :: Signal(a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3)
reconfig44 :: Signal(a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3, Signal b4)

reconfig11 = MoC.reconfig11 
reconfig12 = MoC.reconfig12 
reconfig13 = MoC.reconfig13 
reconfig14 = MoC.reconfig14 
reconfig21 = MoC.reconfig21 
reconfig22 = MoC.reconfig22 
reconfig23 = MoC.reconfig23 
reconfig24 = MoC.reconfig24 
reconfig31 = MoC.reconfig31 
reconfig32 = MoC.reconfig32 
reconfig33 = MoC.reconfig33 
reconfig34 = MoC.reconfig34 
reconfig41 = MoC.reconfig41 
reconfig42 = MoC.reconfig42 
reconfig43 = MoC.reconfig43 
reconfig44 = MoC.reconfig44 

------- CONSTANT -------

-- | A signal generator which keeps a value constant. It implements rgw @stated0X@
-- pattern (see 'ForSyDe.Atom.MoC.stated22').
--
-- Constructors: @constant[1-4]@.
--
-- >>> let (s1, s2) = constant2 (1,2)
-- >>> takeS 3 s1
-- {1,1,1}
-- >>> takeS 5 s2
-- {2,2,2,2,2}
--
-- <<fig/moc-sy-pattern-constant.png>>
constant2 :: (b1, b2)               -- ^ values to be repeated
          -> (Signal b1, Signal b2) -- ^ generated signals
constant1 :: b1 -> Signal b1
constant3 :: (b1, b2, b3) -> (Signal b1, Signal b2, Signal b3)
constant4 :: (b1, b2, b3, b4) -> (Signal b1, Signal b2, Signal b3, Signal b4)

constant1 i = MoC.stated01 id (unit  i)
constant2 i = MoC.stated02 (,) (unit2 i)
constant3 i = MoC.stated03 (,,) (unit3 i)
constant4 i = MoC.stated04 (,,,) (unit4 i)


------- GENERATE -------

-- | A signal generator based on a function and a kernel value. It implements the
-- @stated0X@ pattern (check 'ForSyDe.Atom.MoC.stated22').
--
-- Constructors: @generate[1-4]@.
--
-- >>> let (s1,s2) = generate2 (\a b -> (a+1,b+2)) (1,2)  
-- >>> takeS 5 s1
-- {1,2,3,4,5}
-- >>> takeS 7 s2
-- {2,4,6,8,10,12,14}
--
-- <<fig/moc-sy-pattern-generate.png>>
generate2 :: (b1 -> b2 -> (b1, b2))
             -- ^ function to generate next value
             -> (b1, b2)
             -- ^ kernel values
             -> (Signal b1, Signal b2) -- ^ generated signals
generate1 :: (b1 -> b1) -> b1
          -> Signal b1
generate3 :: (b1 -> b2 -> b3 -> (b1, b2, b3)) -> (b1, b2, b3)
          -> (Signal b1, Signal b2, Signal b3)
generate4 :: (b1 -> b2 -> b3 -> b4 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
          -> (Signal b1, Signal b2, Signal b3, Signal b4)

generate1 ns i = MoC.stated01 ns (unit  i)
generate2 ns i = MoC.stated02 ns (unit2 i)
generate3 ns i = MoC.stated03 ns (unit3 i)
generate4 ns i = MoC.stated04 ns (unit4 i)


------- STATED -------

-- | @stated@ is a state machine without an output decoder. It implements the @stated@
-- pattern (see 'ForSyDe.Atom.MoC.stated22').
--
-- Constructors: @stated[1-4][1-4]@.
--
-- >>> let s1 = signal [1,2,3,4,5]  
-- >>> stated11 (+) 1 s1
-- {1,2,4,7,11,16}
--
-- <<fig/moc-sy-pattern-stated.png>>
stated22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2))
            -- ^ next state function
            -> (b1, b2)
            -- ^ initial state values
            -> Signal a1
            -- ^ first input signal
            -> Signal a2
            -- ^ second input signal
            -> (Signal b1, Signal b2) -- ^ output signals
stated11 :: (b1 -> a1 -> b1) -> b1
        -> Signal a1 -> Signal b1
stated12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> (Signal b1, Signal b2)
stated13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
stated14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
stated21 :: (b1 -> a1 -> a2 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal b1
stated23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
stated24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3, Signal b4)
stated31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
stated32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
stated33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)
stated34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3, Signal b4)
stated41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> Signal b1
stated42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2)
stated43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3)
stated44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3, Signal b4)

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
                 
-- | @state@ is a state machine without an output decoder. It implements the @stated@
-- pattern (see 'ForSyDe.Atom.MoC.state22').
--
-- Constructors: @state[1-4][1-4]@.
--
-- >>> let s1 = signal [1,2,3,4,5]  
-- >>> state11 (+) 1 s1
-- {2,4,7,11,16}
--
-- <<fig/moc-sy-pattern-state.png>>
state22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2))
           -- ^ next state function
           -> (b1, b2)
           -- ^ initial state values
           -> Signal a1
           -- ^ first input signal
           -> Signal a2
           -- ^ second input signal
           -> (Signal b1, Signal b2) -- ^ output signals
state11 :: (b1 -> a1 -> b1) -> b1
        -> Signal a1 -> Signal b1
state12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> (Signal b1, Signal b2)
state13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
state14 :: (b1 -> b2 -> b3 -> b4 -> a1 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
state21 :: (b1 -> a1 -> a2 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal b1
state23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
state24 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3, Signal b4)
state31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
state32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
state33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)
state34 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3, Signal b4)
state41 :: (b1 -> a1 -> a2 -> a3 -> a4 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> Signal b1
state42 :: (b1 -> b2 -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2)
state43 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3)
state44 :: (b1 -> b2 -> b3 -> b4 -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> (b1, b2, b3, b4)
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3, Signal b4)

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

-- | @moore@ processes model Moore state machines. It implements the @moore@ patterns
-- (see 'ForSyDe.Atom.MoC.moore22').
--
-- Constructors: @moore[1-4][1-4]@.
--
-- >>> let s1 = signal [1,2,3,4,5]  
-- >>> moore11 (+) (+1) 1 s1
-- {2,3,5,8,12,17}
--
-- <<fig/moc-sy-pattern-moore.png>>
moore22 :: (st -> a1 -> a2 -> st)
           -- ^ next state function
           -> (st -> (b1, b2))
           -- ^ output decoder
           -> st
           -- ^ initial state
           -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2)
moore11 :: (st -> a1 -> st) -> (st -> b1) -> st
        -> Signal a1 -> Signal b1
moore12 :: (st -> a1 -> st) -> (st -> (b1, b2)) -> st
        -> Signal a1 -> (Signal b1, Signal b2)
moore13 :: (st -> a1 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
moore14 :: (st -> a1 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
moore21 :: (st -> a1 -> a2 -> st) -> (st -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal b1
moore23 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
moore24 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3, Signal b4)
moore31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
moore32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
moore33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)
moore34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3, Signal b4)
moore41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> Signal b1
moore42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2)
moore43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3)
moore44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3, Signal b4)

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

-- | @mealy@ processes model Mealy state machines. It implements the @mealy@ pattern
-- (see 'ForSyDe.Atom.MoC.mealy22').
--
-- Constructors: @mealy[1-4][1-4]@.
--
-- >>> let s1 = signal [1,2,3,4,5]  
-- >>> mealy11 (+) (-) 1 s1
-- {0,0,1,3,6}
--
-- <<fig/moc-sy-pattern-mealy.png>>
mealy22 :: (st -> a1 -> a2 -> st)
           -- ^ next state function
           -> (st -> a1 -> a2 -> (b1, b2))
           -- ^ outpt decoder
           -> st
           -- ^ initial state
           -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2)
mealy11 :: (st -> a1 -> st) -> (st -> a1 -> b1) -> st
        -> Signal a1 -> Signal b1
mealy12 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2)) -> st
        -> Signal a1 -> (Signal b1, Signal b2)
mealy13 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3)) -> st
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
mealy14 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
mealy21 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal b1
mealy23 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
mealy24 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3, Signal b4)
mealy31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
mealy32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
mealy33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)
mealy34 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3, Signal b4)
mealy41 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> Signal b1
mealy42 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2)
mealy43 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3)
mealy44 :: (st -> a1 -> a2 -> a3 -> a4 -> st) -> (st -> a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3, Signal b4)

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

     
--------------- END DOCUMENTATION ---------------
