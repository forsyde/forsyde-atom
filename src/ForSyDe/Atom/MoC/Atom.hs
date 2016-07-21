{-# LANGUAGE PostfixOperators, TypeFamilies, TypeFamilyDependencies #-}
{-# OPTIONS_HADDOCK show-extensions, prune, hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.Atom
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

module ForSyDe.Atom.MoC.Atom where

import ForSyDe.Atom.MoC.Signal
import ForSyDe.Atom.Utility
import ForSyDe.Atom.Behavior

import GHC.Exts

infixl 5 -$-, -*-
infixl 3 ->-, -&-

-- | This is a typelass holding the synchronization layer atoms.
-- Each model of computation defines its tag system by providing an
-- @Event@ typeonstructor (that defines /T/ &#215; /V/) which is an
-- instance of this class.
--
-- In order to simplify the mathematical notations, we shall consider
--the following function as the typeonstructor for a MoC-bound
--signal:
--
-- <<includes/figs/signal-constructor-formula.png>>
--
class (Partitioned e) => MoC e where

  -- | This is a rough equivalent of the @(\<$\>)@ functor operator,
  -- mapping a function on extended values to a MoC-bound signal
  -- (signal of tagged events). Its mathematical definition is:
  --
  -- <<includes/figs/star-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-$-) :: (Context e c) => (Arg e c (Value a) -> b) -> Signal (e (Value a)) -> Signal (e b)

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
  (-*-) :: (Context e c) => Signal (e (Arg e c (Value a) -> b)) -> Signal (e (Value a)) -> Signal (e b)

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
  (->-) :: (Context e c) => e (Arg e c (Value a)) -> Signal (e (Value a)) -> Signal (e (Value a))
   
  -- | Another property, this time derived from the tagged signal
  -- model, requires the processes to be /monotonic/
  -- (order-preserving) in order to preserve determinancy. Therefore
  -- we introduce the @(-&-)@ atom as means to manipulate the tags in
  -- a signal in a manner which respects monotonicity. Its behavior
  -- could be described as "shifting the phase of a signal with a
  -- positiveonstant", thus preserving its characteristic function
  -- intact. Its signature used in mathematical formulas is:
  --
  -- <<includes/figs/phi-atom-formula.png>>
  (-&-) :: (Context e c) => e (Arg e c (Value a)) -> Signal (e (Value a)) -> Signal (e (Value a))


class Partitioned e where
  type Arg e c a = r | r -> c a
  type Context e c :: Constraint
  o :: (Context e c) => Signal (e (Arg e c a)) -> Signal (e a)

infixl 3 -¤, -<, -<<, -<<<, -<<<<, -<<<<<, -<<<<<<, -<<<<<<<, -<<<<<<<<
(-¤)        s =  o   $ s
(-<)        s = (o,o) $$ (s ||<)
(-<<)       s = (o,o,o) $$$ (s ||<<)
(-<<<)      s = (o,o,o,o) $$$$ (s ||<<<)
(-<<<<)     s = (o,o,o,o,o) $$$$$ (s ||<<<<)
(-<<<<<)    s = (o,o,o,o,o,o) $$$$$$ (s ||<<<<<)
(-<<<<<<)   s = (o,o,o,o,o,o,o) $$$$$$$ (s ||<<<<<<)
(-<<<<<<<)  s = (o,o,o,o,o,o,o,o) $$$$$$$$ (s ||<<<<<<<)
(-<<<<<<<<) s = (o,o,o,o,o,o,o,o,o) $$$$$$$$$ (s ||<<<<<<<<)

