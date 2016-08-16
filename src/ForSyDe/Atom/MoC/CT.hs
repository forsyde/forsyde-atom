{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
-- The @CT@ library implements the atoms holding the sematics for the
-- continuous time computation model. It also provides a set of helpers
-- for properly instantiating process network patterns as process
-- constructors.
--
-- __IMPORTANT!!!__ Most of the multi-parameter higher-order functions
-- provided by the library API are named along the lines of
-- @functionMN@ where @M@ represents the number of __/curried/__
-- inputs (i.e. @a1 -> a2 -> ... -> aM@), while @N@ represents the
-- number of __/tupled/__ outputs (i.e. @(b1,b2,...,bN)@). To avoid
-- repetition we shall only provide documentation for functions with 2
-- inputs and 2 outputs (i.e. @function22@).
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT (

  -- * Continuous time (@CT@) event

  -- | According to <#lee98 [1]>, "[regarding metric time] at a
  -- minimum, /T/ is an Abelian group, in addition to being totally
  -- ordered. A consequence is that /t&#8322;/ - /t&#8321;/ is itself
  -- a tag &#8704; /t&#8321;/, /t&#8322;/ &#8712; /T/. In a slightly
  -- more elaborate model of computation, /T/ has a metric. (...) A
  -- continuous-time system is a metric timed system /Q/ where /T/ is
  -- a continuum (a closed connected set)."
  --
  -- The continuous time (@CT@) MoC defines the closest behavior to
  -- what we could call "physical time", where signals cover the full
  -- span of a simulation as "functions of time" rather than
  -- "values". As such, we can state:
  --
  -- [The CT MoC] is abstracting the execution semantics and describes
  -- a system where computation is performed continuously over a
  -- (possibly infine) span of time.
  --
  -- Below is an illustration of the behavior in time of the input and
  -- the output signals of a CT process:
  --
  -- <<includes/figs/ct-example1.png>>
  --
  -- Implementing the SY tag system is straightforward if we consider
  -- the 'ForSyDe.Atom.Signal.Signal' as an infinite list. In this
  -- case the tags are implicitly defined by the position of events in
  -- a signal: /t&#8320;/ would correspond with the event at the head
  -- of a signal /t&#8321;/ with the following event, etc... Thus the
  -- only explicit parameter passed to a SY event constructor is its
  -- value &#8712; /V&#8337;/. As such, we can state the following
  -- particularities:
  --
  -- 1. tags are implicit by the position in the signal, thus they are
  -- completely ignored in this type constructor.
  --
  -- 1. the type constructor wraps only a value
  --
  -- 1. the full structure used by the MoC atoms is @(SY [Value a])@,
  -- i. e. SY events of partitioned extended values, where the
  -- partition is always 1 since for the SY MoC, /T/ is a total order
  -- (check __design rule #4__ in the documentation of "ForSyDe.Atom")
  --
  -- 1. SY atoms do not require any additional context, apart from a
  -- function on values thus by definition
  --
  -- > type Context SY = ()
  --
  -- From @3.@ and @4.@ we can safely assume that:
  --
  -- <<includes/figs/timed-wrapper-formula.png>>

  CT(..),

  -- * Aliases & utilities

  -- | For convenience we also provide a set of type synonyms and
  -- utilities to ease in the design of systems. The API type
  -- signatures will feature these aliases to hide the cumbersome
  -- construction of atoms and atom patters as seen in
  -- "ForSyDe.Atom.MoC".

  Time, Event, Sig, event2, signal,

  wrap11, wrap21, wrap31, wrap41, wrap51, wrap61, wrap71, wrap81, 
  wrap12, wrap22, wrap32, wrap42, wrap52, wrap62, wrap72, wrap82, 
  wrap13, wrap23, wrap33, wrap43, wrap53, wrap63, wrap73, wrap83, 
  wrap14, wrap24, wrap34, wrap44, wrap54, wrap64, wrap74, wrap84,
  
  -- * @SY@ process constuctors

  -- | These SY-specific process constructors are basically
  -- specific instantiations of the network patterns defined in
  -- "ForSyDe.Atom.MoC", also wrapping functions in a behavioural
  -- model.

  -- ** Default behavior

  -- | These processes manifest a default behavior as defined in
  -- "ForSyDe.MoC.Behavior", when it comes to dealing with special
  -- events.
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  delay,
  
  constant1, constant2, constant3, constant4,

  generate1, generate2, generate3, generate4,

  stated11, stated12, stated13, stated14,
  stated21, stated22, stated23, stated24,
  stated31, stated32, stated33, stated34,
  stated41, stated42, stated43, stated44,

  state11, state12, state13, state14,
  state21, state22, state23, state24,
  state31, state32, state33, state34,
  state41, state42, state43, state44,

  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,
 
  ) where

import ForSyDe.Atom.MoC.CT.Core
import ForSyDe.Atom.MoC.CT.Lib
