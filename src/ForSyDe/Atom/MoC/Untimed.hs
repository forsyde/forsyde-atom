{-# LANGUAGE PostfixOperators, TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.Untimed
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
----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.Untimed -- (

  -- -- * Atoms
  
  -- MoC(..),

  -- -- * Process constructors

  -- -- | As shown in the documentation of "ForSyDe.Atom" process
  -- -- constructors are implemented as compositions of MoC atoms. Also,
  -- -- in order to avoid working with signals of tuples and for process
  -- -- network to reflect the passed functions, we use @unzip@ utilities
  -- -- (defined in "ForSyDe.Core.Utility").
  -- --
  -- -- Due to Haskell's strict type system and the implementation
  -- -- mechanisms, we need to provide separate constructors @processXY@,
  -- -- where @process@ is the process constructor type, @X@ is the
  -- -- number of inputs and @Y@ is the number of outputs. This module
  -- -- provides constructors with @ X = [0..4]@ and @ Y = [1..4]@. If
  -- -- needed, the designer is free to implement her own constructor by
  -- -- following the atom composition rules in the source code.

  -- delay, (-&>-),
  
  -- comb11, comb12, comb13, comb14,
  -- comb21, comb22, comb23, comb24,
  -- comb31, comb32, comb33, comb34,
  -- comb41, comb42, comb43, comb44,
  -- comb51, comb52, comb53, comb54,

  -- state11, state12, state13, state14,
  -- state21, state22, state23, state24,
  -- state31, state32, state33, state34,
  -- state41, state42, state43, state44,

  -- stated01, stated02, stated03, stated04,
  -- stated11, stated12, stated13, stated14,
  -- stated21, stated22, stated23, stated24,
  -- stated31, stated32, stated33, stated34,
  -- stated41, stated42, stated43, stated44,
  
  -- moore11, moore12, moore13, moore14,
  -- moore21, moore22, moore23, moore24,
  -- moore31, moore32, moore33, moore34,
  -- moore41, moore42, moore43, moore44,

  -- mealy11, mealy12, mealy13, mealy14,
  -- mealy21, mealy22, mealy23, mealy24,
  -- mealy31, mealy32, mealy33, mealy34,
  -- mealy41, mealy42, mealy43, mealy44,  
  -- )
  where

import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.Utility

infixl 5 -.-, -*-
infixl 3 -<-, -&-

-- | This is a type class defining the synchronization layer atoms.
-- Each model of computation exposes its tag system through an unique
-- event constructor as an instance of this class, which defines /T/
-- &#215; /V/.
--
class (Functor e) => Untimed e where
  -- | A type defined by each MoC, determining the context in the
  -- presence which functions are being applied.
  type Rate e

  -- | This atom is mapping a function on extended values in the
  -- presence of a context to a MoC-bound signal (signal of tagged
  -- events), defined as:
  --
  -- <<includes/figs/star-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-.-) :: ([a] -> b) -> (Rate e, Stream (e [a])) -> Stream (e b)

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
  (-*-) :: Stream (e ([a] -> b)) -> (Rate e, Stream (e [a])) -> Stream (e b)

  -- | When defining the 'Stream' type we enunciated __design rule #2__
  -- where we noticed the importance of initial tokens in feedback
  -- loops. The '->-' atom satisfies this need. Concretely, it prepends
  -- an event at the head of a signal. Its signature used in
  -- mathematical formulas is:
  --
  -- <<includes/figs/pre-atom-formula.png>>
  (-<-) :: e [a] -> Stream (e [a]) -> Stream (e [a])
   
  -- | We introduce the '-&-' atom as means to manipulate the tags in
  -- a signal in a manner which respects monotonicity in order to
  -- respect __design rule #1__ stated in the 'Stream' section. Its
  -- behavior could be described as "shifting the phase of a signal
  -- with a positive constant", thus preserving its characteristic
  -- function intact. Its signature used in mathematical formulas is:
  --
  -- <<includes/figs/phi-atom-formula.png>>
  (-&-) :: e [a] -> Stream (e [a]) -> Stream (e [a])

class Untimed e => UtUtil e where
  bind   :: Rate e -> Stream (e [a]) -> (Rate e, Stream (e [a]))
  verify :: Rate e -> Stream (e [a]) -> Stream (e [a])

rates11 c1 p1
  process s1
  = verify p1 $ process
    (bind c1 s1)
rates21 (c1,c2) p1
  process s1 s2
  = verify p1 $ process
    (bind c1 s1) (bind c2 s2)
rates31 (c1,c2,c3) p1
  process s1 s2 s3
  = verify p1 $ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3)
rates41 (c1,c2,c3,c4) p1
  process s1 s2 s3 s4
  = verify p1 $ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
rates51 (c1,c2,c3,c4,c5) p1
  process s1 s2 s3 s4 s5
  = verify p1 $ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5)
rates61 (c1,c2,c3,c4,c5,c6) p1
  process s1 s2 s3 s4 s5 s6
  = verify p1 $ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5)
rates71 (c1,c2,c3,c4,c5,c6,c7) p1
  process s1 s2 s3 s4 s5 s6 s7
  = verify p1 $ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7)
rates81 (c1,c2,c3,c4,c5,c6,c7,c8) p1
  process s1 s2 s3 s4 s5 s6 s7 s8
  = verify p1 $  process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7) (bind c8 s8)


rates12 c1 (p1,p2)
  process s1
  = (verify p1, verify p2) $$ process
    (bind c1 s1)
rates22 (c1,c2) (p1,p2)
  process s1 s2
  = (verify p1, verify p2) $$ process
    (bind c1 s1) (bind c2 s2)
rates32 (c1,c2,c3) (p1,p2)
  process s1 s2 s3
  = (verify p1, verify p2) $$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3)
rates42 (c1,c2,c3,c4) (p1,p2)
  process s1 s2 s3 s4
  = (verify p1, verify p2) $$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
rates52 (c1,c2,c3,c4,c5) (p1,p2)
  process s1 s2 s3 s4 s5
  = (verify p1, verify p2) $$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5)
rates62 (c1,c2,c3,c4,c5,c6) (p1,p2)
  process s1 s2 s3 s4 s5 s6
  = (verify p1, verify p2) $$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5)
rates72 (c1,c2,c3,c4,c5,c6,c7) (p1,p2)
  process s1 s2 s3 s4 s5 s6 s7
  = (verify p1, verify p2) $$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7)
rates82 (c1,c2,c3,c4,c5,c6,c7,c8) (p1,p2)
  process s1 s2 s3 s4 s5 s6 s7 s8
  = (verify p1, verify p2) $$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7) (bind c8 s8)

rates13 c1 (p1,p2,p3)
  process s1
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1)
rates23 (c1,c2) (p1,p2,p3)
  process s1 s2
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1) (bind c2 s2)
rates33 (c1,c2,c3) (p1,p2,p3)
  process s1 s2 s3
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3)
rates43 (c1,c2,c3,c4) (p1,p2,p3)
  process s1 s2 s3 s4
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
rates53 (c1,c2,c3,c4,c5) (p1,p2,p3)
  process s1 s2 s3 s4 s5
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5)
rates63 (c1,c2,c3,c4,c5,c6) (p1,p2,p3)
  process s1 s2 s3 s4 s5 s6
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5)
rates73 (c1,c2,c3,c4,c5,c6,c7) (p1,p2,p3)
  process s1 s2 s3 s4 s5 s6 s7
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7)
rates83 (c1,c2,c3,c4,c5,c6,c7,c8) (p1,p2,p3)
  process s1 s2 s3 s4 s5 s6 s7 s8
  = (verify p1, verify p2, verify p3) $$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7) (bind c8 s8)


rates14 c1 (p1,p2,p3,p4)
  process s1
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1)
rates24 (c1,c2) (p1,p2,p3,p4) process s1 s2
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1) (bind c2 s2)
rates34 (c1,c2,c3) (p1,p2,p3,p4)
  process s1 s2 s3
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3)
rates44 (c1,c2,c3,c4) (p1,p2,p3,p4)
  process s1 s2 s3 s4
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
rates54 (c1,c2,c3,c4,c5) (p1,p2,p3,p4) process s1 s2 s3 s4 s5
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5)
rates64 (c1,c2,c3,c4,c5,c6) (p1,p2,p3,p4)
  process s1 s2 s3 s4 s5 s6
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5)
rates74 (c1,c2,c3,c4,c5,c6,c7) (p1,p2,p3,p4)
  process s1 s2 s3 s4 s5 s6 s7
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7)
rates84 (c1,c2,c3,c4,c5,c6,c7,c8) (p1,p2,p3,p4)
  process s1 s2 s3 s4 s5 s6 s7 s8
  = (verify p1, verify p2, verify p3, verify p4) $$$$ process
    (bind c1 s1) (bind c2 s2) (bind c3 s3) (bind c4 s4)
    (bind c5 s5) (bind c5 s5) (bind c7 s7) (bind c8 s8)


rates11 :: UtUtil e => Rate e -> Rate e
        -> ((Rate e, Stream (e [a])) -> Stream (e [a]))
        -> Stream (e [a]) -> Stream (e [a])
