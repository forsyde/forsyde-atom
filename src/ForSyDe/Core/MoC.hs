{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the atoms and process constructors for the
-- synchronization layer.
-----------------------------------------------------------------------------

module ForSyDe.Core.MoC(
  -- * Atoms
  
  MoC(..),

  -- * Process constructors

  -- | As shown in the documentation of "ForSyDe.Core" process
  -- constructors are implemented as compositions of MoC atoms. In
  -- order to avoid working with signals of tuples @unzip@ and in
  -- order for the process network to reflect the passed functions,
  -- process utilities (defined in "ForSyDe.Core.Utilities") are also
  -- used.
  --
  -- Due to Haskell's strict type system and the implementation
  -- mechanisms, we need to provide separate constructors @nameXY@,
  -- where @name@ is the process constructor type, @X@ is the number
  -- of inputs and @Y@ is the number of outputs. This module provides
  -- constructors with /X &#8804; 4/ and /Y &#8804; 4/. If needed, the
  -- designer is free to implement her own constructor by following
  -- the atom composition rules in the source code.

  -- ** @comb@

  -- | @comb@ processes map combinatorial functions on signals and
  -- take care of synchronization between input signals.
  --
  -- <<includes/figs/comb-formula.png>>
  -- <<includes/figs/comb-graph.png>>

  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  comb51, comb52, comb53, comb54,

  -- ** @delay@

  -- | The @delay@ process provides both an initial token and shifts the
  -- phase of the signal. In other words, it "delays" a signal with
  -- one event.
  --
  -- <<includes/figs/delay-formula.png>>
  -- <<includes/figs/delay-graph.png>>
  delay, (-&>-),

  -- ** @scanl@

  -- | @scanl@ processes model generates the graph shown below. There
  -- exists a variant with 0 input signals, in which case the process
  -- is a signal generator.
  --
  -- <<includes/figs/scanl-formula.png>>
  -- <<includes/figs/scanl-graph.png>>

  -- scanl01, scanl02, scanl03, scanl04,
  scanl11, scanl12, scanl13, scanl14,
  scanl21, scanl22, scanl23, scanl24,
  scanl31, scanl32, scanl33, scanl34,
  scanl41, scanl42, scanl43, scanl44,

  
  -- ** @scanld@

  -- | @scanld@ processes model generates the graph shown below. There
  -- exists a variant with 0 input signals, in which case the process
  -- is a signal generator.
  --
  -- <<includes/figs/scanld-formula.png>>
  -- <<includes/figs/scanld-graph.png>>

  scanld01, scanld02, scanld03, scanld04,
  scanld11, scanld12, scanld13, scanld14,
  scanld21, scanld22, scanld23, scanld24,
  scanld31, scanld32, scanld33, scanld34,
  scanld41, scanld42, scanld43, scanld44,
  
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

import ForSyDe.Core.Signal
import ForSyDe.Core.Utilities
import ForSyDe.Core.ValueExt

infixl 5 -$-, -*-
infixl 3 ->-, -&-

-- | This is a type class holding the synchronization layer atoms.
-- Each model of computation defines its tag system by providing an
-- @Event@ type constructor (that defines /T/ &#215; /V/) which is an
-- instance of this class.
--
-- In order to simplify the mathematical notations, we shall consider
--the following function as the type constructor for a MoC-bound
--signal:
--
-- <<includes/figs/signal-constructor-formula.png>>
--
class MoC e where

  -- | This is a rough equivalent of the @(\<$\>)@ functor operator,
  -- mapping a function on extended values to a MoC-bound signal
  -- (signal of tagged events). Its mathematical definition is:
  --
  -- <<includes/figs/star-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-$-)  :: (Value a -> b) -> Signal (e (Value a)) -> Signal (e b)

  -- | This is a rough equivalent of the @(\<*\>)@ applicative functor
  -- operator, used for synchronizing and applying a signal of
  -- functions (bound to a MoC) with a signal of values (bound to the
  -- same MoC). Its mathematical signature is:
  --
  -- <<includes/figs/ostar-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-*-) :: Signal (e (Value a -> b)) -> Signal (e (Value a)) -> Signal (e b)

  -- | Since ForSyDe signals are modeled similar to
  -- <https://www.cs.ox.ac.uk/files/3378/PRG56.pdf Bird lists>, the
  -- inherent execution model of ForSyDe process networks is the
  -- dataflow network (Kahn process network), as shown by Reekie in
  -- his <http://ptolemy.eecs.berkeley.edu/~johnr/papers/thesis.html process nets>.
  -- Therefore it is of utmost importance to provide an atom which
  -- provides an initial event (token) to avoid execution deadlock in
  -- feedback loops (self-referential processes). Concretely, the
  -- @-\>-@ atom prepends an event at the head of a signal. Its
  -- signature used in mathematical formulas is:
  --
  -- <<includes/figs/pre-atom-formula.png>>
  (->-) :: e a -> Signal (e a) -> Signal (e a)
   
  -- | Another property, this time derived from the tagged signal
  -- model, requires the processes to be /monotonic/
  -- (order-preserving) in order to preserve determinancy. Therefore
  -- we introduce the @(-&-)@ atom as means to manipulate the tags in
  -- a signal in a manner which respects monotonicity. Its behavior
  -- could be described as "shifting the phase of a signal with a
  -- positive constant", thus preserving its characteristic function
  -- intact. Its signature used in mathematical formulas is:
  --
  -- <<includes/figs/phi-atom-formula.png>>
  (-&-) :: e a -> Signal (e a) -> Signal (e a)

           
-----------------------------------------------------------------------------
comb11 f s1             = (f -$- s1)
comb12 f s1             = (f -$- s1 ||<)
comb13 f s1             = (f -$- s1 ||<<)
comb14 f s1             = (f -$- s1 ||<<<)
comb21 f s1 s2          = (f -$- s1 -*- s2)
comb22 f s1 s2          = (f -$- s1 -*- s2 ||<)
comb23 f s1 s2          = (f -$- s1 -*- s2 ||<<)
comb24 f s1 s2          = (f -$- s1 -*- s2 ||<<<)
comb31 f s1 s2 s3       = (f -$- s1 -*- s2 -*- s3)
comb32 f s1 s2 s3       = (f -$- s1 -*- s2 -*- s3 ||<)
comb33 f s1 s2 s3       = (f -$- s1 -*- s2 -*- s3 ||<<)
comb34 f s1 s2 s3       = (f -$- s1 -*- s2 -*- s3 ||<<<)
comb41 f s1 s2 s3 s4    = (f -$- s1 -*- s2 -*- s3 -*- s4)
comb42 f s1 s2 s3 s4    = (f -$- s1 -*- s2 -*- s3 -*- s4 ||<)
comb43 f s1 s2 s3 s4    = (f -$- s1 -*- s2 -*- s3 -*- s4 ||<<)
comb44 f s1 s2 s3 s4    = (f -$- s1 -*- s2 -*- s3 -*- s4 ||<<<)
comb51 f s1 s2 s3 s4 s5 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5)
comb52 f s1 s2 s3 s4 s5 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 ||<)
comb53 f s1 s2 s3 s4 s5 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 ||<<)
comb54 f s1 s2 s3 s4 s5 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 ||<<<)

infixl 3 -&>-
delay i xs = i ->- (i -&- xs)
i -&>- xs  = delay i xs


scanl11 ns i s1          =        comb21 ns st s1 
  where st               = i -&>- comb21 ns st s1 
scanl12 ns i s1          =       (comb21 ns st s1 |||<)
  where st               = i -&>- comb21 ns st s1
scanl13 ns i s1          =       (comb21 ns st s1 |||<<)
  where st               = i -&>- comb21 ns st s1
scanl14 ns i s1          =       (comb21 ns st s1 |||<<<)
  where st               = i -&>- comb21 ns st s1
scanl21 ns i s1 s2       =        comb31 ns st s1 s2
  where st               = i -&>- comb31 ns st s1 s2
scanl22 ns i s1 s2       =       (comb31 ns st s1 s2 |||<)
  where st               = i -&>- comb31 ns st s1 s2
scanl23 ns i s1 s2       =       (comb31 ns st s1 s2 |||<<)
  where st               = i -&>- comb31 ns st s1 s2
scanl24 ns i s1 s2       =       (comb31 ns st s1 s2 |||<<<)
  where st               = i -&>- comb31 ns st s1 s2
scanl31 ns i s1 s2 s3    =        comb41 ns st s1 s2 s3
  where st               = i -&>- comb41 ns st s1 s2 s3
scanl32 ns i s1 s2 s3    =       (comb41 ns st s1 s2 s3 |||<)
  where st               = i -&>- comb41 ns st s1 s2 s3
scanl33 ns i s1 s2 s3    =       (comb41 ns st s1 s2 s3 |||<<)
  where st               = i -&>- comb41 ns st s1 s2 s3
scanl34 ns i s1 s2 s3    =       (comb41 ns st s1 s2 s3 |||<<<)
  where st               = i -&>- comb41 ns st s1 s2 s3
scanl41 ns i s1 s2 s3 s4 =        comb51 ns st s1 s2 s3 s4
  where st               = i -&>- comb51 ns st s1 s2 s3 s4
scanl42 ns i s1 s2 s3 s4 =       (comb51 ns st s1 s2 s3 s4 |||<)
  where st               = i -&>- comb51 ns st s1 s2 s3 s4
scanl43 ns i s1 s2 s3 s4 =       (comb51 ns st s1 s2 s3 s4 |||<<)
  where st               = i -&>- comb51 ns st s1 s2 s3 s4
scanl44 ns i s1 s2 s3 s4 =       (comb51 ns st s1 s2 s3 s4 |||<<<)
  where st               = i -&>- comb51 ns st s1 s2 s3 s4


scanld01 ns i             = st
  where st                = i -&>- comb11 ns st
scanld02 ns i             = (st |||<)
  where st                = i -&>- comb11 ns st
scanld03 ns i             = (st |||<<)
  where st                = i -&>- comb11 ns st
scanld04 ns i             = (st |||<<<)
  where st                = i -&>- comb11 ns st
scanld11 ns i s1          = st
  where st                = i -&>- comb21 ns st s1
scanld12 ns i s1          = (st |||<)
  where st                = i -&>- comb21 ns st s1
scanld13 ns i s1          = (st |||<<)
  where st                = i -&>- comb21 ns st s1
scanld14 ns i s1          = (st |||<<<)
  where st                = i -&>- comb21 ns st s1
scanld21 ns i s1 s2       = st
  where st                = i -&>- comb31 ns st s1 s2
scanld22 ns i s1 s2       = (st |||<)
  where st                = i -&>- comb31 ns st s1 s2
scanld23 ns i s1 s2       = (st |||<<)
  where st                = i -&>- comb31 ns st s1 s2
scanld24 ns i s1 s2       = (st |||<<<)
  where st                = i -&>- comb31 ns st s1 s2
scanld31 ns i s1 s2 s3    = st
  where st                = i -&>- comb41 ns st s1 s2 s3
scanld32 ns i s1 s2 s3    = (st |||<)
  where st                = i -&>- comb41 ns st s1 s2 s3
scanld33 ns i s1 s2 s3    = (st |||<<)
  where st                = i -&>- comb41 ns st s1 s2 s3
scanld34 ns i s1 s2 s3    = (st |||<<<)
  where st                =  i -&>- comb41 ns st s1 s2 s3
scanld41 ns i s1 s2 s3 s4 = st
  where st                =  i -&>- comb51 ns st s1 s2 s3 s4
scanld42 ns i s1 s2 s3 s4 = (st |||<)
  where st                =  i -&>- comb51 ns st s1 s2 s3 s4
scanld43 ns i s1 s2 s3 s4 = (st |||<<)
  where st                =  i -&>- comb51 ns st s1 s2 s3 s4
scanld44 ns i s1 s2 s3 s4 = (st |||<<<)
  where st                =  i -&>- comb51 ns st s1 s2 s3 s4


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

