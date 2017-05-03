{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SY
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @SY@ library implements the atoms holding the sematics for the
-- synchronous computation model. It also provides a set of helpers
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

module ForSyDe.Atom.MoC.SY (

  -- * Synchronous (@SY@) event

  -- | According to <#lee98 [1]>, "two events are synchronous if they
  -- have the same tag, and two signals are synchronous if all events
  -- in one signal are synchronous to an event in the second signal
  -- and vice-versa. A system is synchronous if every signals in the
  -- system is synchronous to all the other signals in the system."
  --
  -- The synchronous (@SY@) MoC defines no notion of physical time,
  -- its tag system suggesting in fact the precedence among events. To
  -- further demystify its mechanisms, we can formulate the following
  -- proposition:
  --
  -- [The SY MoC] is abstracting the execution semantics of a system
  -- where computation is assumed to perform instantaneously (with
  -- zero delay), at certain synchronization points, when data is
  -- assumed to be available.
  --
  -- Below is a /possible/ behavior in time of the input and the
  -- output signals of a SY process, to illustrate these semantics:
  --
  -- <<includes/figs/sy-example.png>>
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

  SY(..),

  -- * Aliases & utilities

  -- | For convenience we also provide a set of type synonyms and
  -- utilities to ease in the design of systems. The API type
  -- signatures will feature these aliases to hide the cumbersome
  -- construction of atoms and atom patters as seen in
  -- "ForSyDe.Atom.MoC".

  Event, Sig, event, event2, signal,

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
  -- "ForSyDe.Atom.Behavior", when it comes to dealing with special
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

  -- ** Storing behavior
  
  buffer1, buffer2, buffer3, buffer4,
  
  -- ** Predicate behavior

  -- | These processes manipulate the behavior of a signal based on
  -- predicates on their status.

  when, filter, fill,
  hold,

  -- * Interfaces

  toDE, toDE2, toDE3, toDE4,
  zipx, unzipx
                      
  -- * Bibliography

  -- | #lee98# [1] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /Computer-Aided Design of Integrated Circuits and Systems, IEEE Transactions on, 17(12)/, 1217-1229.
  
  -- | #hal91# [2] Halbwachs, N., Caspi, P., Raymond, P., & Pilaud, D. (1991). The synchronous data flow programming language LUSTRE. /Proceedings of the IEEE, 79(9)/, 1305-1320.
  
  ) where

import Prelude hiding (filter)
import ForSyDe.Atom.MoC.SY.Core
import ForSyDe.Atom.MoC.SY.Lib
import ForSyDe.Atom.MoC.SY.Interface
