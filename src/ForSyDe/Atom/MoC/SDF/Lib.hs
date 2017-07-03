{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SDF.Lib
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2017
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

import qualified ForSyDe.Atom.MoC as MoC
import           ForSyDe.Atom.MoC.SDF.Core
import           ForSyDe.Atom.Utility

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)

------- DELAY -------

-- | The @delay@ process "delays" a signal with initial events built
-- from a list. It is an instantiation of the 'ForSyDe.Atom.MoC.delay'
-- constructor.
--
-- >>> let s = signal [1,2,3,4,5]
-- >>> delay [0,0,0] s
-- {0,0,0,1,2,3,4,5}
-- 
-- <<docfiles/figs/moc-sdf-pattern-delay.png>>
delay :: [a]      -- ^ list of initial values
      -> Signal a -- ^ input signal
      -> Signal a -- ^ output signal

delay i = MoC.delay (signal i)

-- | Similar to the previous, but this is the raw instantiation of the
-- 'ForSyDe.Atom.MoC.delay' pattern. It appends the contents of one
-- signal at the head of another signal.
--
-- >>> let s1 = signal [0,0,0]
-- >>> let s2 = signal [1,2,3,4,5]
-- >>> delay' s1 s2
-- {0,0,0,1,2,3,4,5}
--
-- <<docfiles/figs/moc-sdf-pattern-delayp.png>>
delay' :: Signal a  -- ^ signal "borrowing" the initial event
      -> Signal a   -- ^ input signal
      -> Signal a   -- ^ output signal

delay' = MoC.delay

------- COMB -------

-- | @comb@ processes map combinatorial functions on signals and take
-- care of synchronization between input signals. It instantiates the
-- @comb@ atom pattern (see 'ForSyDe.Atom.MoC.comb22').
--
-- The following constructors are provided:
--
-- > comb11, comb12, comb13, comb14,
-- > comb21, comb22, comb23, comb24,
-- > comb31, comb32, comb33, comb34,
-- > comb41, comb42, comb43, comb44,
--
-- >>> let s1 = signal [1..]
-- >>> let s2 = signal [1,1,1,1,1,1,1]
-- >>> let f [a,b,c] [d,e] = [a+d, c+e] 
-- >>> comb21 ((3,2),2,f) s1 s2
-- {2,4,5,7,8,10}
-- >>> comb21 ((3,2),3,f) s1 s2
-- *** Exception: [MoC.SDF] Wrong production
--
-- <<docfiles/figs/moc-sdf-pattern-comb.png>>
comb22 :: ((Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2]))
          -- ^ function on lists of values, tupled with consumption /
          -- production rates
       -> Signal a1              -- ^ first input signal
       -> Signal a2              -- ^ second input signal
       -> (Signal b1, Signal b2) -- ^ two output signals
comb11 :: (Cons, Prod,
           [a1] -> [b1])
       -> Signal a1
       -> Signal b1
comb21 :: ((Cons,Cons), Prod,
           [a1] -> [a2] -> [b1])
       -> Signal a1 -> Signal a2
       -> Signal b1
comb31 :: ((Cons,Cons,Cons), Prod,
           [a1] -> [a2] -> [a3] -> [b1])
       -> Signal a1 -> Signal a2 -> Signal a3
       -> Signal b1
comb41 :: ((Cons,Cons,Cons,Cons), Prod,
           [a1] -> [a2] -> [a3] -> [a4] -> [b1])
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> Signal b1
comb12 :: (Cons, (Prod,Prod),
           [a1] -> ([b1], [b2]))
       -> Signal a1
       -> (Signal b1, Signal b2)
comb32 :: ((Cons,Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2]))
       -> Signal a1 -> Signal a2 -> Signal a3
       -> (Signal b1, Signal b2)
comb42 :: ((Cons,Cons,Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2]))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> (Signal b1, Signal b2)
comb13 :: (Cons, (Prod,Prod,Prod),
           [a1] -> ([b1], [b2], [b3]))
       -> Signal a1
       -> (Signal b1, Signal b2, Signal b3)
comb23 :: ((Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2], [b3]))
       -> Signal a1 -> Signal a2
       -> (Signal b1, Signal b2, Signal b3)
comb33 :: ((Cons,Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
       -> Signal a1 -> Signal a2 -> Signal a3
       -> (Signal b1, Signal b2, Signal b3)
comb43 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3]))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> (Signal b1, Signal b2, Signal b3)
comb14 :: (Cons, (Prod,Prod,Prod,Prod),
           [a1] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
comb24 :: ((Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> Signal a2
       -> (Signal b1, Signal b2, Signal b3, Signal b4)
comb34 :: ((Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> Signal a2 -> Signal a3
       -> (Signal b1, Signal b2, Signal b3, Signal b4)
comb44 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> (Signal b1, Signal b2, Signal b3, Signal b4)
       
comb11 (c,p,f) s1          = MoC.comb11 (MoC.ctxt11 c p f) s1
comb21 (c,p,f) s1 s2       = MoC.comb21 (MoC.ctxt21 c p f) s1 s2
comb31 (c,p,f) s1 s2 s3    = MoC.comb31 (MoC.ctxt31 c p f) s1 s2 s3
comb41 (c,p,f) s1 s2 s3 s4 = MoC.comb41 (MoC.ctxt41 c p f) s1 s2 s3 s4
comb12 (c,p,f) s1          = MoC.comb12 (MoC.ctxt12 c p f) s1
comb22 (c,p,f) s1 s2       = MoC.comb22 (MoC.ctxt22 c p f) s1 s2
comb32 (c,p,f) s1 s2 s3    = MoC.comb32 (MoC.ctxt32 c p f) s1 s2 s3
comb42 (c,p,f) s1 s2 s3 s4 = MoC.comb42 (MoC.ctxt42 c p f) s1 s2 s3 s4
comb13 (c,p,f) s1          = MoC.comb13 (MoC.ctxt13 c p f) s1
comb23 (c,p,f) s1 s2       = MoC.comb23 (MoC.ctxt23 c p f) s1 s2
comb33 (c,p,f) s1 s2 s3    = MoC.comb33 (MoC.ctxt33 c p f) s1 s2 s3
comb43 (c,p,f) s1 s2 s3 s4 = MoC.comb43 (MoC.ctxt43 c p f) s1 s2 s3 s4
comb14 (c,p,f) s1          = MoC.comb14 (MoC.ctxt14 c p f) s1
comb24 (c,p,f) s1 s2       = MoC.comb24 (MoC.ctxt24 c p f) s1 s2
comb34 (c,p,f) s1 s2 s3    = MoC.comb34 (MoC.ctxt34 c p f) s1 s2 s3
comb44 (c,p,f) s1 s2 s3 s4 = MoC.comb44 (MoC.ctxt44 c p f) s1 s2 s3 s4

------- CONSTANT -------

-- | A signal generator which repeats the initial tokens
-- indefinitely. It is actually an instantiation of the @stated0X@
-- constructor (check 'ForSyDe.Atom.MoC.stated22').
--
-- The following constructors are provided:
--
-- > constant1, constant2, constant3, constant4,
--
-- >>> let (s1, s2) = constant2 ([1,2,3],[2,1])
-- >>> takeS 7 s1
-- {1,2,3,1,2,3,1}
-- >>> takeS 5 s2
-- {2,1,2,1,2}
--
-- <<docfiles/figs/moc-sdf-pattern-constant.png>>
constant2 :: ([b1], [b2])           -- ^ values to be repeated
          -> (Signal b1, Signal b2) -- ^ generated signals
constant1 :: ([b1])
          -> (Signal b1)                                  
constant3 :: ([b1], [b2], [b3])
          -> (Signal b1, Signal b2, Signal b3)                      
constant4 :: ([b1], [b2], [b3], [b4])
          -> (Signal b1, Signal b2, Signal b3, Signal b4)

constant1 i = MoC.stated01 (MoC.ctxt11  1         1        id) (signal  i)
constant2 i = MoC.stated02 (MoC.ctxt22 (1,1)     (1,1)     (,)) (signal2 i)
constant3 i = MoC.stated03 (MoC.ctxt33 (1,1,1)   (1,1,1)   (,,)) (signal3 i)
constant4 i = MoC.stated04 (MoC.ctxt44 (1,1,1,1) (1,1,1,1) (,,,)) (signal4 i)

------- GENERATE -------

-- | A signal generator based on a function and a kernel value. It
-- is actually an instantiation of the @stated0X@ constructor
-- (check 'ForSyDe.Atom.MoC.stated22').
--
-- The following constructors are provided:
--
-- > generate1, generate2, generate3, generate4,
--
-- >>> let f a b = ([sum a, sum a],[sum b, sum b, sum b])
-- >>> let (s1,s2) = generate2 ((2,3),(2,3),f) ([1,1],[2,2,2])
-- >>> takeS 7 s1
-- {1,1,2,2,4,4,8}
-- >>> takeS 8 s2
-- {2,2,2,6,6,6,18,18}
--
-- <<docfiles/figs/moc-sdf-pattern-generate.png>>
generate2 :: ((Cons,Cons), (Prod,Prod),
              [b1] -> [b2] -> ([b1], [b2]))
             -- ^ function to generate next value, tupled with
             -- consumption / production rates
             -> ([b1], [b2])
             -- ^ values of initial tokens
             -> (Signal b1, Signal b2)
             -- ^ generated signals
generate1 :: (Cons, Prod,
              [b1] -> [b1])
          -> [b1]
          -> Signal b1                                
generate3 :: ((Cons,Cons,Cons), (Prod,Prod,Prod),
              [b1] -> [b2] -> [b3] -> ([b1], [b2], [b3]))
          -> ([b1], [b2], [b3])
          -> (Signal b1, Signal b2, Signal b3)                      
generate4 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
              [b1] -> [b2] -> [b3] -> [b4] -> ([b1], [b2], [b3], [b4]))
          -> ([b1], [b2], [b3], [b4])
          -> (Signal b1, Signal b2, Signal b3, Signal b4) 

generate1 (c,p,f) i = MoC.stated01 (MoC.ctxt11 c p f) (signal  i)
generate2 (c,p,f) i = MoC.stated02 (MoC.ctxt22 c p f) (signal2 i)
generate3 (c,p,f) i = MoC.stated03 (MoC.ctxt33 c p f) (signal3 i)
generate4 (c,p,f) i = MoC.stated04 (MoC.ctxt44 c p f) (signal4 i)

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
-- >>> let f [a] [b,c] = [a+b+c]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> stated11 ((1,2),1,f) [1] s
-- {1,4,11,22}
--
-- <<docfiles/figs/moc-sdf-pattern-stated.png>>
stated22 :: ((Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> ([b1], [b2]))
         -- ^ next state function, tupled with
         -- consumption / production rates
         -> ([b1], [b2])
         -- ^ initial state partitions of values
         -> Signal a1
         -- ^ first input signal
         -> Signal a2
         -- ^ second input signal
         -> (Signal b1, Signal b2)
         -- ^ output signals
stated11 :: ((Cons,Cons), Prod,
             [b1] -> [a1] -> [b1])
         ->  [b1]
         -> Signal a1
         -> Signal b1
stated12 :: ((Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> ([b1], [b2]))
         -> ([b1], [b2])
         -> Signal a1
         -> (Signal b1, Signal b2)
stated13 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3])
         -> Signal a1
         -> (Signal b1, Signal b2, Signal b3)
stated14 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1]
             -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4])
         -> Signal a1
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
stated21 :: ((Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [b1])
         ->  [b1]
         -> Signal a1 -> Signal a2
         -> Signal b1
stated23 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3])
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3)
stated24 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2]
             -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4])
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
stated31 :: ((Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [b1])
         ->  [b1]
         -> Signal a1 -> Signal a2 -> Signal a3
         -> Signal b1
stated32 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> ([b1], [b2]))
         -> ([b1], [b2])
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2)
stated33 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3]
             -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3])
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3)
stated34 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3]
             -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4])
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
stated41 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [a4] -> [b1])
         ->  [b1]
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> Signal b1
stated42 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> [a4]
             -> ([b1], [b2]))
         -> ([b1], [b2])
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2)
stated43 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3] -> [a4]
             -> ([b1], [b2], [b3]))
         -> ([b1], [b2], [b3])
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3)
stated44 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3]
             -> [a4] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4])
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
           
stated11 (cns,pns,ns) i = MoC.stated11 (MoC.ctxt21 cns pns ns) (signal  i)
stated12 (cns,pns,ns) i = MoC.stated12 (MoC.ctxt32 cns pns ns) (signal2 i)
stated13 (cns,pns,ns) i = MoC.stated13 (MoC.ctxt43 cns pns ns) (signal3 i)
stated14 (cns,pns,ns) i = MoC.stated14 (MoC.ctxt54 cns pns ns) (signal4 i)
stated21 (cns,pns,ns) i = MoC.stated21 (MoC.ctxt31 cns pns ns) (signal  i)
stated22 (cns,pns,ns) i = MoC.stated22 (MoC.ctxt42 cns pns ns) (signal2 i)
stated23 (cns,pns,ns) i = MoC.stated23 (MoC.ctxt53 cns pns ns) (signal3 i)
stated24 (cns,pns,ns) i = MoC.stated24 (MoC.ctxt64 cns pns ns) (signal4 i)
stated31 (cns,pns,ns) i = MoC.stated31 (MoC.ctxt41 cns pns ns) (signal  i)
stated32 (cns,pns,ns) i = MoC.stated32 (MoC.ctxt52 cns pns ns) (signal2 i)
stated33 (cns,pns,ns) i = MoC.stated33 (MoC.ctxt63 cns pns ns) (signal3 i)
stated34 (cns,pns,ns) i = MoC.stated34 (MoC.ctxt74 cns pns ns) (signal4 i)
stated41 (cns,pns,ns) i = MoC.stated41 (MoC.ctxt51 cns pns ns) (signal  i)
stated42 (cns,pns,ns) i = MoC.stated42 (MoC.ctxt62 cns pns ns) (signal2 i)
stated43 (cns,pns,ns) i = MoC.stated43 (MoC.ctxt73 cns pns ns) (signal3 i)
stated44 (cns,pns,ns) i = MoC.stated44 (MoC.ctxt84 cns pns ns) (signal4 i)

------- STATE -------

-- | @state@ is a state machine without an output decoder. It is an
-- instantiation of the @stated@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.state22').
--
-- The following constructors are provided:
--
-- > state11, state12, state13, state14,
-- > state21, state22, state23, state24,
-- > state31, state32, state33, state34,
-- > state41, state42, state43, state44,
--
-- >>> let f [a] [b,c] = [a+b+c]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> state11 ((1,2),1,f) [1] s
-- {4,11,22}
--
-- <<docfiles/figs/moc-sdf-pattern-state.png>>
state22 :: ((Cons,Cons,Cons,Cons), (Prod,Prod),
            [b1] -> [b2] -> [a1] -> [a2] -> ([b1], [b2]))
        -- ^ next state function, tupled with consumption /
        -- production rates
        -> ([b1], [b2])
        -- ^ initial partitions of values
        -> Signal a1
        -- ^ first input signal
        -> Signal a2
        -- ^ second input signal
        -> (Signal b1, Signal b2)
        -- ^ output signals
state11 :: ((Cons,Cons), Prod,
             [b1] -> [a1] -> [b1])
        ->  [b1]
        -> Signal a1
        -> Signal b1
state12 :: ((Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> ([b1], [b2]))
        -> ([b1], [b2])
        -> Signal a1
        -> (Signal b1, Signal b2)
state13 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> ([b1], [b2], [b3]))
        -> ([b1], [b2], [b3])
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3)
state14 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> ([b1], [b2], [b3], [b4]))
         -> ([b1], [b2], [b3], [b4])
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
state21 :: ((Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [b1])
        ->  [b1]
        -> Signal a1 -> Signal a2
        -> Signal b1
state23 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> ([b1], [b2], [b3]))
        -> ([b1], [b2], [b3])
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3)
state24 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
        -> ([b1], [b2], [b3], [b4])
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
state31 :: ((Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [b1])
         ->  [b1]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> Signal b1
state32 :: ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> ([b1], [b2]))
        -> ([b1], [b2])
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2)
state33 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
        -> ([b1], [b2], [b3])
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3)
state34 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
        -> ([b1], [b2], [b3], [b4])
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
state41 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [b1] -> [a1] -> [a2] -> [a3] -> [a4] -> [b1])
        ->  [b1]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> Signal b1
state42 :: ((Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [b1] -> [b2] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
        -> ([b1], [b2])
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2)
state43 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
        -> ([b1], [b2], [b3])
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3)
state44 :: ((Cons,Cons,Cons,Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [b1] -> [b2] -> [b3] -> [b4] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
        -> ([b1], [b2], [b3], [b4])
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
           
state11 (cns,pns,ns) i = MoC.state11 (MoC.ctxt21 cns pns ns) (signal  i)
state12 (cns,pns,ns) i = MoC.state12 (MoC.ctxt32 cns pns ns) (signal2 i)
state13 (cns,pns,ns) i = MoC.state13 (MoC.ctxt43 cns pns ns) (signal3 i)
state14 (cns,pns,ns) i = MoC.state14 (MoC.ctxt54 cns pns ns) (signal4 i)
state21 (cns,pns,ns) i = MoC.state21 (MoC.ctxt31 cns pns ns) (signal  i)
state22 (cns,pns,ns) i = MoC.state22 (MoC.ctxt42 cns pns ns) (signal2 i)
state23 (cns,pns,ns) i = MoC.state23 (MoC.ctxt53 cns pns ns) (signal3 i)
state24 (cns,pns,ns) i = MoC.state24 (MoC.ctxt64 cns pns ns) (signal4 i)
state31 (cns,pns,ns) i = MoC.state31 (MoC.ctxt41 cns pns ns) (signal  i)
state32 (cns,pns,ns) i = MoC.state32 (MoC.ctxt52 cns pns ns) (signal2 i)
state33 (cns,pns,ns) i = MoC.state33 (MoC.ctxt63 cns pns ns) (signal3 i)
state34 (cns,pns,ns) i = MoC.state34 (MoC.ctxt74 cns pns ns) (signal4 i)
state41 (cns,pns,ns) i = MoC.state41 (MoC.ctxt51 cns pns ns) (signal  i)
state42 (cns,pns,ns) i = MoC.state42 (MoC.ctxt62 cns pns ns) (signal2 i)
state43 (cns,pns,ns) i = MoC.state43 (MoC.ctxt73 cns pns ns) (signal3 i)
state44 (cns,pns,ns) i = MoC.state44 (MoC.ctxt84 cns pns ns) (signal4 i)

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
-- >>> let ns [a] [b,c] = [a+b+c]
-- >>> let od [a]       = [a+1,a*2]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> moore11 ((1,2),1,ns) (1,2,od) [1] s
-- {2,2,5,8,12,22,23,44}
--
-- <<docfiles/figs/moc-sdf-pattern-moore.png>>
moore22 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -- ^ next state function, tupled with consumption / production
        -- rates
        -> (Cons, (Prod,Prod), [st] -> ([b1], [b2]))
        -- ^ output decoder, tupled with consumption / production
        -- rates
        -> [st]
        -- ^ initial state values
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2)
moore11 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> (Cons, Prod, [st] -> [b1])
        -> [st]
        -> Signal a1
        -> Signal b1 
moore12 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> (Cons, (Prod,Prod), [st] -> ([b1], [b2]))
        -> [st]
        -> Signal a1
        -> (Signal b1, Signal b2) 
moore13 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> ( Cons, (Prod,Prod,Prod), [st] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3) 
moore14 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> ( Cons , (Prod,Prod,Prod,Prod),
             [st] -> ([b1], [b2], [b3], [b4]))
        -> [st]
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
moore21 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -> ( Cons, Prod, [st] -> [b1])
        -> [st]
        -> Signal a1 -> Signal a2
        -> Signal b1 
moore23 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -> ( Cons, (Prod,Prod,Prod), [st] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3) 
moore24 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -> (Cons, (Prod,Prod,Prod,Prod),
             [st] -> ([b1], [b2], [b3], [b4]))
        -> [st]
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
moore31 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons, Prod, [st] -> [b1])
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> Signal b1 
moore32 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons, (Prod,Prod), [st] -> ([b1], [b2]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2) 
moore33 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons, (Prod,Prod,Prod), [st] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3) 
moore34 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ( Cons, (Prod,Prod,Prod,Prod),
             [st] -> ([b1], [b2], [b3], [b4]))
        -> [st] -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
moore41 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ( Cons, Prod, [st] -> [b1])
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> Signal b1 
moore42 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ( Cons, (Prod,Prod), [st] -> ([b1], [b2]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2) 
moore43 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
            [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ( Cons, (Prod,Prod,Prod), [st] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3) 
moore44 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> (Cons, (Prod,Prod,Prod,Prod),
             [st] -> ([b1], [b2], [b3], [b4]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3, Signal b4)

moore11 (cns,pns,ns) (cod,pod,od) i = MoC.moore11 (MoC.ctxt21 cns pns ns) (MoC.ctxt11 cod pod od) (signal i)
moore12 (cns,pns,ns) (cod,pod,od) i = MoC.moore12 (MoC.ctxt21 cns pns ns) (MoC.ctxt12 cod pod od) (signal i)
moore13 (cns,pns,ns) (cod,pod,od) i = MoC.moore13 (MoC.ctxt21 cns pns ns) (MoC.ctxt13 cod pod od) (signal i)
moore14 (cns,pns,ns) (cod,pod,od) i = MoC.moore14 (MoC.ctxt21 cns pns ns) (MoC.ctxt14 cod pod od) (signal i)
moore21 (cns,pns,ns) (cod,pod,od) i = MoC.moore21 (MoC.ctxt31 cns pns ns) (MoC.ctxt11 cod pod od) (signal i)
moore22 (cns,pns,ns) (cod,pod,od) i = MoC.moore22 (MoC.ctxt31 cns pns ns) (MoC.ctxt12 cod pod od) (signal i)
moore23 (cns,pns,ns) (cod,pod,od) i = MoC.moore23 (MoC.ctxt31 cns pns ns) (MoC.ctxt13 cod pod od) (signal i)
moore24 (cns,pns,ns) (cod,pod,od) i = MoC.moore24 (MoC.ctxt31 cns pns ns) (MoC.ctxt14 cod pod od) (signal i)
moore31 (cns,pns,ns) (cod,pod,od) i = MoC.moore31 (MoC.ctxt41 cns pns ns) (MoC.ctxt11 cod pod od) (signal i)
moore32 (cns,pns,ns) (cod,pod,od) i = MoC.moore32 (MoC.ctxt41 cns pns ns) (MoC.ctxt12 cod pod od) (signal i)
moore33 (cns,pns,ns) (cod,pod,od) i = MoC.moore33 (MoC.ctxt41 cns pns ns) (MoC.ctxt13 cod pod od) (signal i)
moore34 (cns,pns,ns) (cod,pod,od) i = MoC.moore34 (MoC.ctxt41 cns pns ns) (MoC.ctxt14 cod pod od) (signal i)
moore41 (cns,pns,ns) (cod,pod,od) i = MoC.moore41 (MoC.ctxt51 cns pns ns) (MoC.ctxt11 cod pod od) (signal i)
moore42 (cns,pns,ns) (cod,pod,od) i = MoC.moore42 (MoC.ctxt51 cns pns ns) (MoC.ctxt12 cod pod od) (signal i)
moore43 (cns,pns,ns) (cod,pod,od) i = MoC.moore43 (MoC.ctxt51 cns pns ns) (MoC.ctxt13 cod pod od) (signal i)
moore44 (cns,pns,ns) (cod,pod,od) i = MoC.moore44 (MoC.ctxt51 cns pns ns) (MoC.ctxt14 cod pod od) (signal i)


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
-- >>> let ns [a] [b,c] = [a+b+c]
-- >>> let od [a] [b]   = [a+b,a*b]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> mealy11 ((1,2),1,ns) ((1,1),2,od) [1] s
-- {2,1,6,8,14,33,26,88}
--
-- <<docfiles/figs/moc-sdf-pattern-mealy.png>>
mealy22 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -- ^ next state function, tupled with consumption / production
        -- rates
        -> ((Cons,Cons,Cons), (Prod,Prod),
            [st] -> [a1] -> [a2] -> ([b1], [b2]))
        -- ^ outpt decoder, tupled with consumption / production rates
        -> [st]
        -- ^ initial state values
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2)           
mealy11 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> ((Cons,Cons), Prod, [st] -> [a1] -> [b1])
        -> [st]
        -> Signal a1
        -> Signal b1 
mealy12 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> ((Cons,Cons), (Prod,Prod), [st] -> [a1] -> ([b1], [b2]))
        -> [st]
        -> Signal a1
        -> (Signal b1, Signal b2) 
mealy13 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> ((Cons,Cons), (Prod,Prod,Prod),
             [st] -> [a1] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3) 
mealy14 :: ((Cons,Cons), Prod, [st] -> [a1] -> [st])
        -> ((Cons,Cons), (Prod,Prod,Prod,Prod),
             [st] -> [a1] -> ([b1], [b2], [b3], [b4]))
        -> [st]
        -> Signal a1
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
mealy21 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -> ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [b1])
        -> [st]
        -> Signal a1 -> Signal a2
        -> Signal b1 
mealy23 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -> ((Cons,Cons,Cons), (Prod,Prod,Prod),
             [st] -> [a1] -> [a2] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3) 
mealy24 :: ((Cons,Cons,Cons), Prod, [st] -> [a1] -> [a2] -> [st])
        -> ((Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [st] -> [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
        -> [st]
        -> Signal a1 -> Signal a2
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
mealy31 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [b1])
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> Signal b1 
mealy32 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons), (Prod,Prod),
             [st] -> [a1] -> [a2] -> [a3] -> ([b1], [b2]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2) 
mealy33 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [st] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3) 
mealy34 :: ((Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [st])
        -> ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [st] -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> (Signal b1, Signal b2, Signal b3, Signal b4)
mealy41 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [b1])
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> Signal b1 
mealy42 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod),
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2) 
mealy43 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3) 
mealy44 :: ((Cons,Cons,Cons,Cons,Cons), Prod,
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> [st])
        -> ((Cons,Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
             [st] -> [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
        -> [st]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> (Signal b1, Signal b2, Signal b3, Signal b4)

mealy11 (cns,pns,ns) (cod,pod,od) i = MoC.mealy11 (MoC.ctxt21 cns pns ns) (MoC.ctxt21 cod pod od) (signal i)
mealy12 (cns,pns,ns) (cod,pod,od) i = MoC.mealy12 (MoC.ctxt21 cns pns ns) (MoC.ctxt22 cod pod od) (signal i)
mealy13 (cns,pns,ns) (cod,pod,od) i = MoC.mealy13 (MoC.ctxt21 cns pns ns) (MoC.ctxt23 cod pod od) (signal i)
mealy14 (cns,pns,ns) (cod,pod,od) i = MoC.mealy14 (MoC.ctxt21 cns pns ns) (MoC.ctxt24 cod pod od) (signal i)
mealy21 (cns,pns,ns) (cod,pod,od) i = MoC.mealy21 (MoC.ctxt31 cns pns ns) (MoC.ctxt31 cod pod od) (signal i)
mealy22 (cns,pns,ns) (cod,pod,od) i = MoC.mealy22 (MoC.ctxt31 cns pns ns) (MoC.ctxt32 cod pod od) (signal i)
mealy23 (cns,pns,ns) (cod,pod,od) i = MoC.mealy23 (MoC.ctxt31 cns pns ns) (MoC.ctxt33 cod pod od) (signal i)
mealy24 (cns,pns,ns) (cod,pod,od) i = MoC.mealy24 (MoC.ctxt31 cns pns ns) (MoC.ctxt34 cod pod od) (signal i)
mealy31 (cns,pns,ns) (cod,pod,od) i = MoC.mealy31 (MoC.ctxt41 cns pns ns) (MoC.ctxt41 cod pod od) (signal i)
mealy32 (cns,pns,ns) (cod,pod,od) i = MoC.mealy32 (MoC.ctxt41 cns pns ns) (MoC.ctxt42 cod pod od) (signal i)
mealy33 (cns,pns,ns) (cod,pod,od) i = MoC.mealy33 (MoC.ctxt41 cns pns ns) (MoC.ctxt43 cod pod od) (signal i)
mealy34 (cns,pns,ns) (cod,pod,od) i = MoC.mealy34 (MoC.ctxt41 cns pns ns) (MoC.ctxt44 cod pod od) (signal i)
mealy41 (cns,pns,ns) (cod,pod,od) i = MoC.mealy41 (MoC.ctxt51 cns pns ns) (MoC.ctxt51 cod pod od) (signal i)
mealy42 (cns,pns,ns) (cod,pod,od) i = MoC.mealy42 (MoC.ctxt51 cns pns ns) (MoC.ctxt52 cod pod od) (signal i)
mealy43 (cns,pns,ns) (cod,pod,od) i = MoC.mealy43 (MoC.ctxt51 cns pns ns) (MoC.ctxt53 cod pod od) (signal i)
mealy44 (cns,pns,ns) (cod,pod,od) i = MoC.mealy44 (MoC.ctxt51 cns pns ns) (MoC.ctxt54 cod pod od) (signal i)

