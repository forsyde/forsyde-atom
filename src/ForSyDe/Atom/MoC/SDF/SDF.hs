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
-- The @SDF@ library implements a DSL of atoms operating according to the synchronous
-- dataflow model of computation, along with helpers and associated patterns.
--
-- This module provides a set of helpers for properly instantiating
-- process network patterns as process constructors.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Atom.MoC.SDF.SDF where

import qualified ForSyDe.Atom.MoC as MoC
import           ForSyDe.Atom.MoC.SDF.Core
import           ForSyDe.Atom.Utility.Tuple

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
-- <<fig/moc-sdf-pattern-delay.png>>
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
-- <<fig/moc-sdf-pattern-delayp.png>>
delay' :: Signal a  -- ^ signal containing the initial tokens
       -> Signal a   -- ^ input signal
       -> Signal a   -- ^ output signal

delay' = MoC.delay

------- COMB -------

-- | @actor@ processes map combnational functions on signals and take
-- care of synchronization between input signals. It instantiates the
-- @comb@ atom pattern (see 'ForSyDe.Atom.MoC.comb22').
--
-- Constructors: @actor[1-4][1-4]@.
--
-- >>> let s1 = signal [1..]
-- >>> let s2 = signal [1,1,1,1,1,1,1]
-- >>> let f [a,b,c] [d,e] = [a+d, c+e] 
-- >>> actor21 ((3,2),2,f) s1 s2
-- {2,4,5,7,8,10}
--
-- Incorrect usage (not covered by @doctest@):
--
-- > λ> actor21 ((3,2),3,f) s1 s2
-- > *** Exception: [MoC.SDF] Wrong production
--
-- <<fig/moc-sdf-pattern-comb.png>>
actor22 :: ((Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2]))
          -- ^ function on lists of values, tupled with consumption /
          -- production rates
       -> Signal a1              -- ^ first input signal
       -> Signal a2              -- ^ second input signal
       -> (Signal b1, Signal b2) -- ^ two output signals
actor11 :: (Cons, Prod,
           [a1] -> [b1])
       -> Signal a1
       -> Signal b1
actor21 :: ((Cons,Cons), Prod,
           [a1] -> [a2] -> [b1])
       -> Signal a1 -> Signal a2
       -> Signal b1
actor31 :: ((Cons,Cons,Cons), Prod,
           [a1] -> [a2] -> [a3] -> [b1])
       -> Signal a1 -> Signal a2 -> Signal a3
       -> Signal b1
actor41 :: ((Cons,Cons,Cons,Cons), Prod,
           [a1] -> [a2] -> [a3] -> [a4] -> [b1])
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> Signal b1
actor12 :: (Cons, (Prod,Prod),
           [a1] -> ([b1], [b2]))
       -> Signal a1
       -> (Signal b1, Signal b2)
actor32 :: ((Cons,Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2]))
       -> Signal a1 -> Signal a2 -> Signal a3
       -> (Signal b1, Signal b2)
actor42 :: ((Cons,Cons,Cons,Cons), (Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2]))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> (Signal b1, Signal b2)
actor13 :: (Cons, (Prod,Prod,Prod),
           [a1] -> ([b1], [b2], [b3]))
       -> Signal a1
       -> (Signal b1, Signal b2, Signal b3)
actor23 :: ((Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2], [b3]))
       -> Signal a1 -> Signal a2
       -> (Signal b1, Signal b2, Signal b3)
actor33 :: ((Cons,Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
       -> Signal a1 -> Signal a2 -> Signal a3
       -> (Signal b1, Signal b2, Signal b3)
actor43 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3]))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> (Signal b1, Signal b2, Signal b3)
actor14 :: (Cons, (Prod,Prod,Prod,Prod),
           [a1] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
actor24 :: ((Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> Signal a2
       -> (Signal b1, Signal b2, Signal b3, Signal b4)
actor34 :: ((Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> Signal a2 -> Signal a3
       -> (Signal b1, Signal b2, Signal b3, Signal b4)
actor44 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod),
           [a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3], [b4]))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
       -> (Signal b1, Signal b2, Signal b3, Signal b4)
       
actor11 cpf s1          = MoC.comb11 (scen11 cpf) s1
actor21 cpf s1 s2       = MoC.comb21 (scen21 cpf) s1 s2
actor31 cpf s1 s2 s3    = MoC.comb31 (scen31 cpf) s1 s2 s3
actor41 cpf s1 s2 s3 s4 = MoC.comb41 (scen41 cpf) s1 s2 s3 s4
actor12 cpf s1          = MoC.comb12 (scen12 cpf) s1
actor22 cpf s1 s2       = MoC.comb22 (scen22 cpf) s1 s2
actor32 cpf s1 s2 s3    = MoC.comb32 (scen32 cpf) s1 s2 s3
actor42 cpf s1 s2 s3 s4 = MoC.comb42 (scen42 cpf) s1 s2 s3 s4
actor13 cpf s1          = MoC.comb13 (scen13 cpf) s1
actor23 cpf s1 s2       = MoC.comb23 (scen23 cpf) s1 s2
actor33 cpf s1 s2 s3    = MoC.comb33 (scen33 cpf) s1 s2 s3
actor43 cpf s1 s2 s3 s4 = MoC.comb43 (scen43 cpf) s1 s2 s3 s4
actor14 cpf s1          = MoC.comb14 (scen14 cpf) s1
actor24 cpf s1 s2       = MoC.comb24 (scen24 cpf) s1 s2
actor34 cpf s1 s2 s3    = MoC.comb34 (scen34 cpf) s1 s2 s3
actor44 cpf s1 s2 s3 s4 = MoC.comb44 (scen44 cpf) s1 s2 s3 s4

------- RECONFIG -------

-- | @reconfig@ creates an SDF adaptive process where the first signal
-- carries functions and the other carry the arguments. It
-- instantiates the @reconfig@ atom pattern (see
-- 'ForSyDe.Atom.MoC.reconfig22'). According to our SDF definition,
-- the production and consumption rates need to be fixed, so they are
-- passed as parameters to the constructor, whereas the first signal
-- carries adaptive functions only. For the adaptive signal it only
-- makes sense that the consumption rate is always 1.
--
-- Constructors: @reconfig[1-4][1-4]@.
--
-- >>> let f1 a = [sum a]
-- >>> let f2 a = [maximum a]
-- >>> let sf = signal [f1,f2,f1,f2,f1,f2,f1] 
-- >>> let s1 = signal [1..]
-- >>> reconfig11 (4,1) sf s1
-- {10,8,42,16,74,24,106}
--
-- <<fig/moc-sdf-pattern-reconfig.png>>
reconfig22 :: ((Cons,Cons), (Prod,Prod))
           -> Signal ([a1] -> [a2] -> ([b1], [b2]))
           -- ^ function on lists of values, tupled with consumption /
           -- production rates
           -> Signal a1              -- ^ first input signal
           -> Signal a2              -- ^ second input signal
           -> (Signal b1, Signal b2) -- ^ two output signals
reconfig11 :: (Cons, Prod) 
           -> Signal ([a1] -> [b1])
           -> Signal a1
           -> Signal b1
reconfig21 :: ((Cons,Cons), Prod)
           -> Signal ( [a1] -> [a2] -> [b1])
           -> Signal a1 -> Signal a2
           -> Signal b1
reconfig31 :: ((Cons,Cons,Cons), Prod)
           -> Signal ([a1] -> [a2] -> [a3] -> [b1])
           -> Signal a1 -> Signal a2 -> Signal a3
           -> Signal b1
reconfig41 :: ((Cons,Cons,Cons,Cons), Prod)
           -> Signal ([a1] -> [a2] -> [a3] -> [a4] -> [b1])
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> Signal b1
reconfig12 :: (Cons, (Prod,Prod))
           -> Signal ([a1] -> ([b1], [b2]))
           -> Signal a1
           -> (Signal b1, Signal b2)
reconfig32 :: ((Cons,Cons,Cons), (Prod,Prod))
           -> Signal ([a1] -> [a2] -> [a3] -> ([b1], [b2]))
           -> Signal a1 -> Signal a2 -> Signal a3
           -> (Signal b1, Signal b2)
reconfig42 :: ((Cons,Cons,Cons,Cons), (Prod,Prod))
           -> Signal ([a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2]))
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> (Signal b1, Signal b2)
reconfig13 :: (Cons, (Prod,Prod,Prod))
           -> Signal ([a1] -> ([b1], [b2], [b3]))
           -> Signal a1
           -> (Signal b1, Signal b2, Signal b3)
reconfig23 :: ((Cons,Cons), (Prod,Prod,Prod))
           -> Signal ([a1] -> [a2] -> ([b1], [b2], [b3]))
           -> Signal a1 -> Signal a2
           -> (Signal b1, Signal b2, Signal b3)
reconfig33 :: ((Cons,Cons,Cons), (Prod,Prod,Prod))
           -> Signal ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
           -> Signal a1 -> Signal a2 -> Signal a3
           -> (Signal b1, Signal b2, Signal b3)
reconfig43 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod))
           -> Signal ([a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3]))
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> (Signal b1, Signal b2, Signal b3)
reconfig14 :: (Cons, (Prod,Prod,Prod,Prod))
           -> Signal ([a1] -> ([b1], [b2], [b3], [b4]))
           -> Signal a1
           -> (Signal b1, Signal b2, Signal b3, Signal b4)
reconfig24 :: ((Cons,Cons), (Prod,Prod,Prod,Prod))
           -> Signal ([a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
           -> Signal a1 -> Signal a2
           -> (Signal b1, Signal b2, Signal b3, Signal b4)
reconfig34 :: ((Cons,Cons,Cons), (Prod,Prod,Prod,Prod))
           -> Signal ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
           -> Signal a1 -> Signal a2 -> Signal a3
           -> (Signal b1, Signal b2, Signal b3, Signal b4)
reconfig44 :: ((Cons,Cons,Cons,Cons), (Prod,Prod,Prod,Prod))
           -> Signal ([a1] -> [a2] -> [a3] ->  [a4] -> ([b1], [b2], [b3], [b4]))
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> (Signal b1, Signal b2, Signal b3, Signal b4)
wrap ctxt c p sf = (fmap . fmap) (\f->ctxt (c,p,f)) sf
reconfig11 (c,p) sf = MoC.reconfig11 (wrap scen11 c p sf)
reconfig12 (c,p) sf = MoC.reconfig12 (wrap scen12 c p sf)
reconfig13 (c,p) sf = MoC.reconfig13 (wrap scen13 c p sf)
reconfig14 (c,p) sf = MoC.reconfig14 (wrap scen14 c p sf)
reconfig21 (c,p) sf = MoC.reconfig21 (wrap scen21 c p sf)
reconfig22 (c,p) sf = MoC.reconfig22 (wrap scen22 c p sf)
reconfig23 (c,p) sf = MoC.reconfig23 (wrap scen23 c p sf)
reconfig24 (c,p) sf = MoC.reconfig24 (wrap scen24 c p sf)
reconfig31 (c,p) sf = MoC.reconfig31 (wrap scen31 c p sf)
reconfig32 (c,p) sf = MoC.reconfig32 (wrap scen32 c p sf)
reconfig33 (c,p) sf = MoC.reconfig33 (wrap scen33 c p sf)
reconfig34 (c,p) sf = MoC.reconfig34 (wrap scen34 c p sf)
reconfig41 (c,p) sf = MoC.reconfig41 (wrap scen41 c p sf)
reconfig42 (c,p) sf = MoC.reconfig42 (wrap scen42 c p sf)
reconfig43 (c,p) sf = MoC.reconfig43 (wrap scen43 c p sf)
reconfig44 (c,p) sf = MoC.reconfig44 (wrap scen44 c p sf)

------- CONSTANT -------

-- | A signal generator which repeats the initial tokens
-- indefinitely. It is actually an instantiation of the @stated0X@
-- constructor (check 'ForSyDe.Atom.MoC.stated22').
--
-- Constructors: @constant[1-4]@.
--
-- >>> let (s1, s2) = constant2 ([1,2,3],[2,1])
-- >>> takeS 7 s1
-- {1,2,3,1,2,3,1}
-- >>> takeS 5 s2
-- {2,1,2,1,2}
--
-- <<fig/moc-sdf-pattern-constant.png>>
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
-- Constructors: @generate[1-4]@.
--
-- >>> let f a b = ([sum a, sum a],[sum b, sum b, sum b])
-- >>> let (s1,s2) = generate2 ((2,3),(2,3),f) ([1,1],[2,2,2])
-- >>> takeS 7 s1
-- {1,1,2,2,4,4,8}
-- >>> takeS 8 s2
-- {2,2,2,6,6,6,18,18}
--
-- <<fig/moc-sdf-pattern-generate.png>>
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

generate1 cpf i = MoC.stated01 (scen11 cpf) (signal  i)
generate2 cpf i = MoC.stated02 (scen22 cpf) (signal2 i)
generate3 cpf i = MoC.stated03 (scen33 cpf) (signal3 i)
generate4 cpf i = MoC.stated04 (scen44 cpf) (signal4 i)

------- STATED -------

-- | @stated@ is a state machine without an output decoder. It is an
-- instantiation of the @state@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.stated22').
--
-- Constructors: @stated[1-4][1-4]@.
--
-- >>> let f [a] [b,c] = [a+b+c]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> stated11 ((1,2),1,f) [1] s
-- {1,4,11,22}
--
-- <<fig/moc-sdf-pattern-stated.png>>
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
           
stated11 ns i = MoC.stated11 (scen21 ns) (signal  i)
stated12 ns i = MoC.stated12 (scen32 ns) (signal2 i)
stated13 ns i = MoC.stated13 (scen43 ns) (signal3 i)
stated14 ns i = MoC.stated14 (scen54 ns) (signal4 i)
stated21 ns i = MoC.stated21 (scen31 ns) (signal  i)
stated22 ns i = MoC.stated22 (scen42 ns) (signal2 i)
stated23 ns i = MoC.stated23 (scen53 ns) (signal3 i)
stated24 ns i = MoC.stated24 (scen64 ns) (signal4 i)
stated31 ns i = MoC.stated31 (scen41 ns) (signal  i)
stated32 ns i = MoC.stated32 (scen52 ns) (signal2 i)
stated33 ns i = MoC.stated33 (scen63 ns) (signal3 i)
stated34 ns i = MoC.stated34 (scen74 ns) (signal4 i)
stated41 ns i = MoC.stated41 (scen51 ns) (signal  i)
stated42 ns i = MoC.stated42 (scen62 ns) (signal2 i)
stated43 ns i = MoC.stated43 (scen73 ns) (signal3 i)
stated44 ns i = MoC.stated44 (scen84 ns) (signal4 i)

------- STATE -------

-- | @state@ is a state machine without an output decoder. It is an
-- instantiation of the @stated@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.state22').
--
-- Constructors: @state[1-4][1-4]@.
--
-- >>> let f [a] [b,c] = [a+b+c]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> state11 ((1,2),1,f) [1] s
-- {4,11,22}
--
-- <<fig/moc-sdf-pattern-state.png>>
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
           
state11 ns i = MoC.state11 (scen21 ns) (signal  i)
state12 ns i = MoC.state12 (scen32 ns) (signal2 i)
state13 ns i = MoC.state13 (scen43 ns) (signal3 i)
state14 ns i = MoC.state14 (scen54 ns) (signal4 i)
state21 ns i = MoC.state21 (scen31 ns) (signal  i)
state22 ns i = MoC.state22 (scen42 ns) (signal2 i)
state23 ns i = MoC.state23 (scen53 ns) (signal3 i)
state24 ns i = MoC.state24 (scen64 ns) (signal4 i)
state31 ns i = MoC.state31 (scen41 ns) (signal  i)
state32 ns i = MoC.state32 (scen52 ns) (signal2 i)
state33 ns i = MoC.state33 (scen63 ns) (signal3 i)
state34 ns i = MoC.state34 (scen74 ns) (signal4 i)
state41 ns i = MoC.state41 (scen51 ns) (signal  i)
state42 ns i = MoC.state42 (scen62 ns) (signal2 i)
state43 ns i = MoC.state43 (scen73 ns) (signal3 i)
state44 ns i = MoC.state44 (scen84 ns) (signal4 i)

------- MOORE -------

-- | @moore@ processes model Moore state machines. It is an
-- instantiation of the @moore@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.moore22').
--
-- Constructors: @moore[1-4][1-4]@.
--
-- >>> let ns [a] [b,c] = [a+b+c]
-- >>> let od [a]       = [a+1,a*2]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> moore11 ((1,2),1,ns) (1,2,od) [1] s
-- {2,2,5,8,12,22,23,44}
--
-- <<fig/moc-sdf-pattern-moore.png>>
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

moore11 ns od i = MoC.moore11 (scen21 ns) (scen11 od) (signal i)
moore12 ns od i = MoC.moore12 (scen21 ns) (scen12 od) (signal i)
moore13 ns od i = MoC.moore13 (scen21 ns) (scen13 od) (signal i)
moore14 ns od i = MoC.moore14 (scen21 ns) (scen14 od) (signal i)
moore21 ns od i = MoC.moore21 (scen31 ns) (scen11 od) (signal i)
moore22 ns od i = MoC.moore22 (scen31 ns) (scen12 od) (signal i)
moore23 ns od i = MoC.moore23 (scen31 ns) (scen13 od) (signal i)
moore24 ns od i = MoC.moore24 (scen31 ns) (scen14 od) (signal i)
moore31 ns od i = MoC.moore31 (scen41 ns) (scen11 od) (signal i)
moore32 ns od i = MoC.moore32 (scen41 ns) (scen12 od) (signal i)
moore33 ns od i = MoC.moore33 (scen41 ns) (scen13 od) (signal i)
moore34 ns od i = MoC.moore34 (scen41 ns) (scen14 od) (signal i)
moore41 ns od i = MoC.moore41 (scen51 ns) (scen11 od) (signal i)
moore42 ns od i = MoC.moore42 (scen51 ns) (scen12 od) (signal i)
moore43 ns od i = MoC.moore43 (scen51 ns) (scen13 od) (signal i)
moore44 ns od i = MoC.moore44 (scen51 ns) (scen14 od) (signal i)


------- MEALY -------

-- | @mealy@ processes model Mealy state machines. It is an
-- instantiation of the @mealy@ MoC constructor
-- (see 'ForSyDe.Atom.MoC.mealy22').
--
-- Constructors: @mealy[1-4][1-4]@.
--
-- >>> let ns [a] [b,c] = [a+b+c]
-- >>> let od [a] [b]   = [a+b,a*b]
-- >>> let s = signal [1,2,3,4,5,6,7]  
-- >>> mealy11 ((1,2),1,ns) ((1,1),2,od) [1] s
-- {2,1,6,8,14,33,26,88}
--
-- <<fig/moc-sdf-pattern-mealy.png>>
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

mealy11 ns od i = MoC.mealy11 (scen21 ns) (scen21 od) (signal i)
mealy12 ns od i = MoC.mealy12 (scen21 ns) (scen22 od) (signal i)
mealy13 ns od i = MoC.mealy13 (scen21 ns) (scen23 od) (signal i)
mealy14 ns od i = MoC.mealy14 (scen21 ns) (scen24 od) (signal i)
mealy21 ns od i = MoC.mealy21 (scen31 ns) (scen31 od) (signal i)
mealy22 ns od i = MoC.mealy22 (scen31 ns) (scen32 od) (signal i)
mealy23 ns od i = MoC.mealy23 (scen31 ns) (scen33 od) (signal i)
mealy24 ns od i = MoC.mealy24 (scen31 ns) (scen34 od) (signal i)
mealy31 ns od i = MoC.mealy31 (scen41 ns) (scen41 od) (signal i)
mealy32 ns od i = MoC.mealy32 (scen41 ns) (scen42 od) (signal i)
mealy33 ns od i = MoC.mealy33 (scen41 ns) (scen43 od) (signal i)
mealy34 ns od i = MoC.mealy34 (scen41 ns) (scen44 od) (signal i)
mealy41 ns od i = MoC.mealy41 (scen51 ns) (scen51 od) (signal i)
mealy42 ns od i = MoC.mealy42 (scen51 ns) (scen52 od) (signal i)
mealy43 ns od i = MoC.mealy43 (scen51 ns) (scen53 od) (signal i)
mealy44 ns od i = MoC.mealy44 (scen51 ns) (scen54 od) (signal i)

