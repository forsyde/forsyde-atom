{-# OPTIONS_HADDOCK show-extensions, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
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

module ForSyDe.Atom.MoC.DE (

  -- * Discrete event (@DE@)

  -- | According to <#lee98 [1]>, "a discrete-event system is a timed
  -- system /Q/ where for all /s/ &#8712; /Q/, the tag sytem is
  -- order-isomorphic to a subset of the integers. Order-isomorphic
  -- means simply that there exists an order-preserving bijection
  -- between the events in /T/ and a subset of the integers (or the
  -- entire set of integers)."
  --
  -- The discrete event (@DE@) MoC does suggest the notion of physical
  -- time through its tags, also called timestamps. As the definition
  -- above implies, an important property of the DE tag system is that
  -- between any two timestamps /t&#7524;/ and /t&#7525;/ there is a
  -- __/finite/__ number of possible timestamps. Based on this we can
  -- formulate the proposition:
  --
  -- [The DE MoC] is abstracting the execution semantics of a system
  -- where synchronization is /discretized/ and /time-aware/, and it
  -- is performed whenever a new event occurs.
  --
  -- There are many variants of discrete event simulators, each of
  -- them implementing slight variations of the semantics stated in
  -- <#lee98 [1]>. Our execution model could be described as a "cycle
  -- simulator", inspired from HDL simulators. In spite of that, it
  -- does not imply delta-delay semantics, on the contrary, it might
  -- be regarded as a /conservative simulator/ similar to the
  -- Chandy-Misra-Bryant Null message algorirhm, as presented in
  -- <#fuji00 [2]>. Below you can see an example of a DE process in
  -- action:
  --
  -- <<includes/figs/de-example.png>>
  --
  -- Based on the information provided until now, we can state the
  -- following particularities of our DE MoC implementation:
  --
  -- 1. tags are explicit and a DE event will construct a type around
  -- both a tag and a(n extended) value.
  --
  -- 1. respecting the definition in <#lee98 [1]>, tags are positive
  -- integers.
  --
  -- 1. Time 0 is assumed to be the absolute beginning of the
  -- simulation, and all sub-systems must be synchronized to this
  -- absolute 0. A signal might start from a /t&#8320;/ &#8805; 0, but
  -- this is interpreted as /V/(/e&#8320;/) = ? (the first event
  -- carries and unknown value). This initial event will be propagated
  -- as such throughout the system, and will be visible at the first
  -- feedback loop.
  --
  -- 1. events are assumed to persist from their time of arrival until
  -- the next event arrives or, if there is no incoming event, until
  -- infinity. This behavior can be regarded either as a persistent
  -- channel (e.g. wire), or as non-blocking buffered input, with the
  -- buffers of size 1.
  --
  -- 1. a consequence of this behavior is that feedback loops will
  -- generate an infinite number of events (strictly preceding each
  -- other), since a loop updates the value after a certain delay. We
  -- emphasize again the importance of __design rules #2__ and __#1__
  -- (where /T/ is a total order) by adding that in order to both
  -- preserve causality /and/ avoid deadlock a delay must manifest
  -- both a /prepend/ and a /phase shift/ behavior, justifying the
  -- construction of the @delay@ pattern (see
  -- 'ForSyDe.Atom.MoC.delay').
  --
  -- 1. as stated in __design rule #3__, due to the
  -- 'ForSyDe.Atom.Signal' implementation, DE processes are forbidden
  -- to clean up events. This means that a new event is created every
  -- time a new event arrives (irrespective of which input signal is
  -- carrying it, or what value it carries). This means that /all/
  -- values are propagated, justifying our system's conservative
  -- approach (<#fuji00 [2]>).
  --
  -- 1. due to this approach, and to the inherent formalism
  -- (culminating with atoms as independent synchronization entities),
  -- ForSyDe simulators are completely parallelizable, since they are
  -- self-sufficient and do not depend on a global event queue (as
  -- compared to other cycle simulators).
  --
  -- 1. any outside signal needs to be sane (/T/ must be a total
  -- order) before being injected into a ForSyDe process
  -- network. Inside a ForSyDe process network, transformations are
  -- rate-monotonic, thus output signals are guaranteed to be sane. 
  --
  -- 1. the full structure used by the MoC atoms is @(DE [Value a])@,
  -- i. e. DE events of partitioned extended values, where the
  -- partition is always 1 since for the DE MoC, /T/ is a total order
  -- (check __design rule #4__ in the documentation of "ForSyDe.Atom")
  --
  -- 1. DE atoms do not require any additional context, apart from a
  -- function on values thus by definition
  --
  -- > type Context DE = ()
  --
  -- From @8.@ and @9.@ we can safely assume that:
  --
  -- <<includes/figs/timed-wrapper-formula.png>>

  Tag, DE(..),

  -- * Aliases & utilities

  -- | For convenience we also provide a set of type synonyms and
  -- utilities to ease in the design of systems. The API type
  -- signatures will feature these aliases to hide the cumbersome
  -- construction of atoms and atom patters as seen in
  -- "ForSyDe.Atom.MoC".

  Event, Sig, event2, signal,

  wrap11, wrap21, wrap31, wrap41, wrap51, wrap61, wrap71, wrap81, 
  wrap12, wrap22, wrap32, wrap42, wrap52, wrap62, wrap72, wrap82, 
  wrap13, wrap23, wrap33, wrap43, wrap53, wrap63, wrap73, wrap83, 
  wrap14, wrap24, wrap34, wrap44, wrap54, wrap64, wrap74, wrap84,
  
  -- * @DE@ process constuctors

  -- | These DE-specific process constructors are basically
  -- specific instantiations of the network patterns defined in
  -- "ForSyDe.Atom.MoC", also wrapping functions in a behavioural
  -- model.

  -- ** Default behavior

  -- | These processes manifest a default behavior as defined in
  -- "ForSyDe.Atom.Behavior", when it comes to dealing with special
  -- events.
  
  delay,
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  sync2, sync3, sync4,
  
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

  -- ** Interface processes

  toSY, toSY2, toSY3, toSY4,
  toCT, toCT2, toCT3, toCT4,

  zipx, unzipx,
  
  -- ** Hybrid processes

  embedSY11, embedSY12, embedSY13, embedSY14,
  embedSY21, embedSY22, embedSY23, embedSY24,
  embedSY31, embedSY32, embedSY33, embedSY34,
  embedSY41, embedSY42, embedSY43, embedSY44,
    
  -- * Bibliography

  -- | #lee98# [1] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /Computer-Aided Design of Integrated Circuits and Systems, IEEE Transactions on, 17(12)/, 1217-1229.
  
  -- | #fuji00# [2] Fujimoto, R. M. (2000). Parallel and distributed simulation systems (Vol. 300). New York: Wiley.
  
 ) where

import ForSyDe.Atom.MoC.DE.Core
import ForSyDe.Atom.MoC.DE.Lib
import ForSyDe.Atom.MoC.DE.Hybrid
import ForSyDe.Atom.MoC.DE.Interface
