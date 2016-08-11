{-# LANGUAGE PostfixOperators, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the core entities of the MoC/synchronization
-- layer: atom interfaces and process constructors as patterns of
-- atoms. It does /NOT/ export any implementation or instantiation of
-- a specific MoC.
--
-- __IMPORTANT!!!__ Most of the multi-parameter higher-order functions
-- provided by the library API are named along the lines of
-- @functionMN@ where @M@ represents the number of __/curried/__
-- inputs (i.e. @a1 -> a2 -> ... -> aM@), while @N@ represents the
-- number of __/tupled/__ outputs (i.e. @(b1,b2,...,bN)@). To avoid
-- repetition we shall only provide documentation for functions with 2
-- inputs and 2 outputs (i.e. @function22@).
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC(

  -- * Atom
  
  MoC(..),

  -- * Process constructors

  -- | As shown in the documentation of "ForSyDe.Atom" process
  -- constructors are implemented as compositions of MoC atoms. Also,
  -- in order to avoid working with signals of tuples and for process
  -- network to reflect the passed functions, we use @unzip@ utilities
  -- (defined in "ForSyDe.Core.Utility").
  --
  -- Due to Haskell's strict type system and the implementation
  -- mechanisms, we need to provide separate constructors @processXY@,
  -- where @process@ is the process constructor type, @X@ is the
  -- number of inputs and @Y@ is the number of outputs. This module
  -- provides constructors with @ X = [0..4]@ and @ Y = [1..4]@. If
  -- needed, the designer is free to implement her own constructor by
  -- following the atom composition rules in the source code.

  delay, (-&>-),
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  comb51, comb52, comb53, comb54,

  state11, state12, state13, state14,
  state21, state22, state23, state24,
  state31, state32, state33, state34,
  state41, state42, state43, state44,

  stated01, stated02, stated03, stated04,
  stated11, stated12, stated13, stated14,
  stated21, stated22, stated23, stated24,
  stated31, stated32, stated33, stated34,
  stated41, stated42, stated43, stated44,
  
  -- ** @moore@

  -- | @moore@ processes model Moore state machines.
  --
  -- <<includes/figs/moore-formula.png>>
  -- <<includes/figs/moore-graph.png>>

  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  -- ** @mealy@

  -- | @mealy@ processes model Mealy state machines.
  --
  -- <<includes/figs/mealy-formula.png>>
  -- <<includes/figs/mealy-graph.png>>

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,  
  ) where

import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Signal
import ForSyDe.Atom.Utility

infixl 5 -$-, -*-
infixl 3 ->-, -&-

-- | This is a type class defining the synchronization layer atoms.
-- Each model of computation exposes its tag system through an unique
-- event constructor as an instance of this class, which defines /T/
-- &#215; /V/.
--
class (Functor e) => MoC e where
  -- | A type defined by each MoC, determining the context in the
  -- presence which functions are being applied.
  type Context e
  
  -- | This atom is mapping a function on extended values in the
  -- presence of a context to a MoC-bound signal (signal of tagged
  -- events), defined as:
  --
  -- <<includes/figs/star-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-$-) :: (Context e, [Value a] -> b) -> Signal (e [Value a]) -> Signal (e b)

  -- | This atom synchronizes two MoC-bound signals, one carrying
  -- functions on extended values in the presence of a context, and
  -- the other containing extended values, during which it applies the
  -- former on the latter. It's definition is:
  --
  -- <<includes/figs/ostar-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-*-) :: (Signal (e (Context e, [Value a] -> b))) -> Signal (e [Value a]) -> Signal (e b)

  -- | When defining the 'Signal' type we enunciated __design rule #2__
  -- where we noticed the importance of initial tokens in feedback
  -- loops. The '->-' atom satisfies this need. Concretely, it prepends
  -- an event at the head of a signal. Its signature used in
  -- mathematical formulas is:
  --
  -- <<includes/figs/pre-atom-formula.png>>
  (->-) :: e a -> Signal (e a) -> Signal (e a)
   
  -- | We introduce the '-&-' atom as means to manipulate the tags in
  -- a signal in a manner which respects monotonicity in order to
  -- respect __design rule #1__ stated in the 'Signal' section. Its
  -- behavior could be described as "shifting the phase of a signal
  -- with a positive constant", thus preserving its characteristic
  -- function intact. Its signature used in mathematical formulas is:
  --
  -- <<includes/figs/phi-atom-formula.png>>
  (-&-) :: e a -> Signal (e a) -> Signal (e a)
  fromEvent :: e a -> a

-- class Event e where
--   event     :: a -> e a
--   fromEvent :: e a -> a


instance (Eq a, MoC e)  => Eq (Signal (e [a])) where
  a == b = flat a == flat b
    where flat = concat . map fromEvent . fromSignal

----------------- DOCUMENTATION -----------------

-- | 
-- #comb22f# /(*) actually/ @[Value a1] -> [Value a2] -> (b1, b2)@
-- /where each argument is individually wrapped with a/
-- /context. Each MoC instance defines its own wrappers./
--
-- @comb@ processes takes care of synchronization between signals and
-- maps combinatorial functions on their event values.
--
-- <<includes/figs/comb-formula.png>> <<includes/figs/comb-graph.png>>
comb22 :: (MoC e)
          => (Context e, [Value a1] -> (Context e, [Value a2] -> (b1, b2)))
          -- ^ (<#comb22f *>)
          -> Signal (e [Value a1])          -- ^ first input signal
          -> Signal (e [Value a2])          -- ^ second input signal
          -> (Signal (e b1), Signal (e b2)) -- ^ two output signals

-- | 
-- #state22f# /(*) actually/ @[Value st1] -> [Value st2] -> [Value a1] -> [Value a2] -> ([Value st1], [Value st2])@
-- /where each argument is individually wrapped with a/
-- /context. Each MoC instance defines its own wrappers./
--
-- @state@ processes model generates the graph shown below. There
-- exists a variant with 0 input signals, in which case the process
-- is a signal generator.
--
-- <<includes/figs/scanl-formula.png>>
-- <<includes/figs/scanl-graph.png>>
state22 :: MoC e
           => (Context e, [Value st1] -> (Context e, [Value st2] -> (Context e, [Value a1] -> (Context e, [Value a2] -> ([Value st1], [Value st2])))))
           -- ^ (<#state22f *>) 
           -> (e [Value st1], e [Value st2])  -- ^ initial states
           -> Signal (e [Value a1])
           -> Signal (e [Value a2])
           -> (Signal (e [Value st1]), Signal (e [Value st2]))

-- | 
-- #stated22f# /(*) actually/ @[Value st1] -> [Value st2] -> [Value a1] -> [Value a2] -> ([Value st1], [Value st2])@
-- /where each argument is individually wrapped with a/
-- /context. Each MoC instance defines its own wrappers./
--
-- @stated@ processes model generates the graph shown below. There
-- exists a variant with 0 input signals, in which case the process
-- is a signal generator.
--
-- <<includes/figs/stated-formula.png>>
-- <<includes/figs/stated-graph.png>>
stated22 :: MoC e
           => (Context e, [Value st1] -> (Context e, [Value st2] -> (Context e, [Value a1] -> (Context e, [Value a2] -> ([Value st1], [Value st2])))))
           -- ^ (<#stated22f *>) 
           -> (e [Value st1], e [Value st2])  -- ^ initial states
           -> Signal (e [Value a1])
           -> Signal (e [Value a2])
           -> (Signal (e [Value st1]), Signal (e [Value st2])) 

--------------- END DOCUMENTATION ---------------


-- | The @delay@ process provides both an initial token and shifts the
-- phase of the signal. In other words, it "delays" a signal with
-- one event.
--
-- <<includes/figs/delay-formula.png>>
-- <<includes/figs/delay-graph.png>>
infixl 3 -&>-
delay i xs = i ->- (i -&- xs)
i -&>- xs = delay i xs          

comb11 f s1                      = (f -$- s1)
comb21 f s1 s2                   = (f -$- s1 -*- s2)
comb31 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3)
comb41 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4)
comb51 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5)
comb61 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6)
comb71 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7)
comb81 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8)

comb12 f s1                      = (f -$- s1 ||<)
comb22 f s1 s2                   = (f -$- s1 -*- s2 ||<)
comb32 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 ||<)
comb42 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 ||<)
comb52 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 ||<)
comb62 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 ||<)
comb72 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 ||<)
comb82 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 ||<)

comb13 f s1                      = (f -$- s1 ||<<)
comb23 f s1 s2                   = (f -$- s1 -*- s2 ||<<)
comb33 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 ||<<)
comb43 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 ||<<)
comb53 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 ||<<)
comb63 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 ||<<)
comb73 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 ||<<)
comb83 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 ||<<)

comb14 f s1                      = (f -$- s1 ||<<<)
comb24 f s1 s2                   = (f -$- s1 -*- s2 ||<<<)
comb34 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 ||<<<)
comb44 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 ||<<<)
comb54 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 ||<<<)
comb64 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 ||<<<)
comb74 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 ||<<<)
comb84 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 ||<<<)

state11 ns i s1          =        comb21 ns st s1 
  where st               = i -&>- comb21 ns st s1 
state21 ns i s1 s2       =        comb31 ns st s1 s2
  where st               = i -&>- comb31 ns st s1 s2
state31 ns i s1 s2 s3    =        comb41 ns st s1 s2 s3
  where st               = i -&>- comb41 ns st s1 s2 s3
state41 ns i s1 s2 s3 s4 =        comb51 ns st s1 s2 s3 s4
  where st               = i -&>- comb51 ns st s1 s2 s3 s4

state12 ns (i1,i2) s1          = let (ns1,ns2) = comb32 ns st1 st2 s1
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
state22 ns (i1,i2) s1 s2       = let (ns1,ns2) = comb42 ns st1 st2 s1 s2
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
state32 ns (i1,i2) s1 s2 s3    = let (ns1,ns2) = comb52 ns st1 st2 s1 s2 s3
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
state42 ns (i1,i2) s1 s2 s3 s4 = let (ns1,ns2) = comb62 ns st1 st2 s1 s2 s3 s4
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)

state13 ns (i1,i2,i3) s1          = let (ns1,ns2,ns3) = comb43 ns st1 st2 st3 s1
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
state23 ns (i1,i2,i3) s1 s2       = let (ns1,ns2,ns3) = comb53 ns st1 st2 st3 s1 s2
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
state33 ns (i1,i2,i3) s1 s2 s3    = let (ns1,ns2,ns3) = comb63 ns st1 st2 st3 s1 s2 s3
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
state43 ns (i1,i2,i3) s1 s2 s3 s4 = let (ns1,ns2,ns3) = comb73 ns st1 st2 st3 s1 s2 s3 s4
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)

state14 ns (i1,i2,i3,i4) s1          = let (ns1,ns2,ns3,ns4) = comb54 ns st1 st2 st3 st4 s1
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
state24 ns (i1,i2,i3,i4) s1 s2       = let (ns1,ns2,ns3,ns4) = comb64 ns st1 st2 st3 st4 s1 s2
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
state34 ns (i1,i2,i3,i4) s1 s2 s3    = let (ns1,ns2,ns3,ns4) = comb74 ns st1 st2 st3 st4 s1 s2 s3
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
state44 ns (i1,i2,i3,i4) s1 s2 s3 s4 = let (ns1,ns2,ns3,ns4) = comb84 ns st1 st2 st3 st4 s1 s2 s3 s4
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)

stated01 ns i             = st 
  where st                = i -&>- comb11 ns st 
stated11 ns i s1          = st
  where st                = i -&>- comb21 ns st s1 
stated21 ns i s1 s2       = st
  where st                = i -&>- comb31 ns st s1 s2
stated31 ns i s1 s2 s3    = st
  where st                = i -&>- comb41 ns st s1 s2 s3
stated41 ns i s1 s2 s3 s4 = st
  where st                = i -&>- comb51 ns st s1 s2 s3 s4

stated02 ns (i1,i2)             = let (ns1,ns2) = comb22 ns st1 st2
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated12 ns (i1,i2) s1          = let (ns1,ns2) = comb32 ns st1 st2 s1
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated22 ns (i1,i2) s1 s2       = let (ns1,ns2) = comb42 ns st1 st2 s1 s2
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated32 ns (i1,i2) s1 s2 s3    = let (ns1,ns2) = comb52 ns st1 st2 s1 s2 s3
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated42 ns (i1,i2) s1 s2 s3 s4 = let (ns1,ns2) = comb62 ns st1 st2 s1 s2 s3 s4
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)

stated03 ns (i1,i2,i3)             = let (ns1,ns2,ns3) = comb33 ns st1 st2 st3
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated13 ns (i1,i2,i3) s1          = let (ns1,ns2,ns3) = comb43 ns st1 st2 st3 s1
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated23 ns (i1,i2,i3) s1 s2       = let (ns1,ns2,ns3) = comb53 ns st1 st2 st3 s1 s2
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated33 ns (i1,i2,i3) s1 s2 s3    = let (ns1,ns2,ns3) = comb63 ns st1 st2 st3 s1 s2 s3
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated43 ns (i1,i2,i3) s1 s2 s3 s4 = let (ns1,ns2,ns3) = comb73 ns st1 st2 st3 s1 s2 s3 s4
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)

stated04 ns (i1,i2,i3,i4)             = let (ns1,ns2,ns3,ns4) = comb44 ns st1 st2 st3 st4
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated14 ns (i1,i2,i3,i4) s1          = let (ns1,ns2,ns3,ns4) = comb54 ns st1 st2 st3 st4 s1
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated24 ns (i1,i2,i3,i4) s1 s2       = let (ns1,ns2,ns3,ns4) = comb64 ns st1 st2 st3 st4 s1 s2
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated34 ns (i1,i2,i3,i4) s1 s2 s3    = let (ns1,ns2,ns3,ns4) = comb74 ns st1 st2 st3 st4 s1 s2 s3
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated44 ns (i1,i2,i3,i4) s1 s2 s3 s4 = let (ns1,ns2,ns3,ns4) = comb84 ns st1 st2 st3 st4 s1 s2 s3 s4
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)

moore11 ns od i s1          =        comb11 od st
  where st                  = i -&>- comb21 ns st s1
moore12 ns od i s1          =        comb12 od st
  where st                  = i -&>- comb21 ns st s1
moore13 ns od i s1          =        comb13 od st
  where st                  = i -&>- comb21 ns st s1
moore14 ns od i s1          =        comb14 od st
  where st                  = i -&>- comb21 ns st s1
moore21 ns od i s1 s2       =        comb11 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore22 ns od i s1 s2       =        comb12 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore23 ns od i s1 s2       =        comb13 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore24 ns od i s1 s2       =        comb14 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore31 ns od i s1 s2 s3    =        comb11 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore32 ns od i s1 s2 s3    =        comb12 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore33 ns od i s1 s2 s3    =        comb13 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore34 ns od i s1 s2 s3    =        comb14 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore41 ns od i s1 s2 s3 s4 =        comb11 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore42 ns od i s1 s2 s3 s4 =        comb12 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore43 ns od i s1 s2 s3 s4 =        comb13 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore44 ns od i s1 s2 s3 s4 =        comb14 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4

mealy11 ns od i s1          =        comb21 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy12 ns od i s1          =        comb22 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy13 ns od i s1          =        comb23 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy14 ns od i s1          =        comb24 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy21 ns od i s1 s2       =        comb31 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy22 ns od i s1 s2       =        comb32 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy23 ns od i s1 s2       =        comb33 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy24 ns od i s1 s2       =        comb34 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy31 ns od i s1 s2 s3    =        comb41 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy32 ns od i s1 s2 s3    =        comb42 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy33 ns od i s1 s2 s3    =        comb43 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy34 ns od i s1 s2 s3    =        comb44 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy41 ns od i s1 s2 s3 s4 =        comb51 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy42 ns od i s1 s2 s3 s4 =        comb52 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy43 ns od i s1 s2 s3 s4 =        comb53 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy44 ns od i s1 s2 s3 s4 =        comb54 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4

