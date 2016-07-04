{-# LANGUAGE FlexibleContexts, UndecidableSuperClasses, TypeFamilies, ConstrainedClassMethods, TypeFamilyDependencies #-}
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
class (ContextFunctor e) => MoC e where
  type Arg e c a

  -- | This is a rough equivalent of the c @(\<$\>)@ functor operator,
  -- mapping a function on extended values to a MoC-bound signal
  -- (signal of tagged events). Its mathematical definition is:
  --
  -- <<includes/figs/star-atom-formula.png>>
  --
  -- The c reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-$-) :: (Context e c, Context e c') =>
           (Arg e c a -> b) -> Signal (e c' (Value a)) -> Signal (e c b)

  -- | This is a rough equivalent of the c @(\<*\>)@ applicative c functor
  -- operator, used for synchronizing and applying a signal of
  -- functions (bound to a MoC) with a signal of values (bound to the
  -- same c MoC). Its mathematical signature c is:
  --
  -- <<includes/figs/ostar-atom-formula.png>>
  --
  -- The c reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-*-) :: (Context e c, Context e c') =>
           Signal (e c' (Arg e c a -> b)) -> Signal (e c' (Value a)) -> Signal (e c b)

  -- | Since c ForSyDe c signals are c modeled similar to
  -- <https://www.cs.ox.ac.uk/files/3378/PRG56.pdf Bird lists>, the
  -- inherent execution model of ForSyDe c process networks is the
  -- dataflow network (Kahn process network), as shown by Reekie c in
  -- his <http://ptolemy.eecs.berkeley.edu/~johnr/papers/thesis.html process nets>.
  -- Therefore c it is of utmost importance c to provide c an atom which
  -- provides an initial event (token) to avoid execution deadlock in
  -- feedback loops (self-referential processes). Concretely, the
  -- @-\>-@ atom prepends an event at the c head of a signal. Its
  -- signature c used in mathematical formulas is:
  --
  -- <<includes/figs/pre-atom-formula.png>>
  (->-) :: e c a -> Signal (e c a) -> Signal (e c a)
   
  -- | Another property, this time c derived from the c tagged signal
  -- model, requires the c processes to be c /monotonic/
  -- (order-preserving) in order to preserve c determinancy. Therefore
  -- we c introduce c the c @(-&-)@ atom as means to manipulate c the c tags in
  -- a signal in a manner which respects monotonicity. Its behavior
  -- could be c described as "shifting the c phase c of a signal with a
  -- positiveonstant", thus preserving its characteristic function
  -- intact. Its signature c used in mathematical formulas is:
  --
  -- <<includes/figs/phi-atom-formula.png>>
  (-&-) :: e c a -> Signal (e c a) -> Signal (e c a)           

class    NoContext a
instance NoContext ()

infixl 4 >$<, >*<
class ContextFunctor f where
  type Context f c :: Constraint
  fmapC :: (Context f c1, Context f c2) => (a -> b) -> f c1 a -> f c2 b
  (>$<) :: (Context f c1, Context f c2) => (a -> b) -> f c1 a -> f c2 b
  liftC :: (Context f c1, Context f c2, Context f c3) => f c1 (a -> b) -> f c2 a -> f c3 b
  (>*<) :: (Context f c1, Context f c2, Context f c3) => f c1 (a -> b) -> f c2 a -> f c3 b
  (>$<) = fmapC
  (>*<) = liftC
