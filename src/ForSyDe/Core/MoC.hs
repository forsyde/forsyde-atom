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
comb11 f s1          = (f -$- s1)
comb12 f s1          = (f -$- s1 -<)
comb13 f s1          = (f -$- s1 -<<)
comb14 f s1          = (f -$- s1 -<<<)
comb21 f s1 s2       = (f -$- s1 -*- s2)
comb22 f s1 s2       = (f -$- s1 -*- s2 -<)
comb23 f s1 s2       = (f -$- s1 -*- s2 -<<)
comb24 f s1 s2       = (f -$- s1 -*- s2 -<<<)
comb31 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3)
comb32 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<)
comb33 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<<)
comb34 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<<<)
comb41 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4)
comb42 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<)
comb43 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
comb44 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)

infixl 3 -&>-
delay i xs = i ->- (i -&- xs)
i -&>- xs  = delay i xs


moore11 ns od i s1          = comb11 od st
  where st                  = i -&>- comb21 ns st s1
moore12 ns od i s1          = comb12 od st
  where st                  = i -&>- comb21 ns st s1
moore13 ns od i s1          = comb13 od st
  where st                  = i -&>- comb21 ns st s1
moore14 ns od i s1          = comb14 od st
  where st                  = i -&>- comb21 ns st s1
moore21 ns od i s1 s2       = comb11 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore22 ns od i s1 s2       = comb12 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore23 ns od i s1 s2       = comb13 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore24 ns od i s1 s2       = comb14 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore31 ns od i s1 s2 s3    = comb11 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore32 ns od i s1 s2 s3    = comb12 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore33 ns od i s1 s2 s3    = comb13 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore34 ns od i s1 s2 s3    = comb14 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore41 ns od i s1 s2 s3 s4 = comb11 od st
  where st                  = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore42 ns od i s1 s2 s3 s4 = comb12 od st
  where st                  = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore43 ns od i s1 s2 s3 s4 = comb13 od st
  where st                  = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore44 ns od i s1 s2 s3 s4 = comb14 od st
  where st                  = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

mealy11 ns od i s1          = comb21 od st s1
  where st           = i -&>- comb21 ns st s1
mealy12 ns od i s1          = comb22 od st s1
  where st           = i -&>- comb21 ns st s1
mealy13 ns od i s1          = comb23 od st s1
  where st           = i -&>- comb21 ns st s1
mealy14 ns od i s1          = comb24 od st s1
  where st           = i -&>- comb21 ns st s1
mealy21 ns od i s1 s2       = comb31 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy22 ns od i s1 s2       = comb32 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy23 ns od i s1 s2       = comb33 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy24 ns od i s1 s2       = comb34 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy31 ns od i s1 s2 s3    = comb41 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy32 ns od i s1 s2 s3    = comb42 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy33 ns od i s1 s2 s3    = comb43 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy34 ns od i s1 s2 s3    = comb44 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy41 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4)
  where st            = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy42 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<)
  where st            = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy43 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<)
  where st            = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy44 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<<)
  where st            = i -&>- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4


-- | The process constructor 'filter' discards the values who do not fulfill a predicate given by a predicate function and replaces them with as5ent events.
-- filter p s1 = predicate Abst -$- s1 
