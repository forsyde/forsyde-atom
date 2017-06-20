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
-- The @DE@ library implements the atoms holding the sematics for the
-- discrete event computation model. It also provides a set of helpers
-- for properly instantiating process network patterns as process
-- constructors.
--
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE (

  -- * Discrete event (@DE@)

  -- | According to <ForSyDe-Atom.html#lee98 [Lee98]>, "a
  -- discrete-event system is a timed system /Q/ where for all /s/
  -- &#8712; /Q/, the tag sytem is order-isomorphic to a subset of the
  -- integers. Order-isomorphic means simply that there exists an
  -- order-preserving bijection between the events in /T/ and a subset
  -- of the integers (or the entire set of integers)."
  --
  -- The discrete event (@DE@) MoC does suggest the notion of physical
  -- time through its tags, also called timestamps. As the definition
  -- above implies, an important property of the DE tag system is that
  -- between any two timestamps /t&#7524;/ and /t&#7525;/ there is a
  -- __/finite/__ number of possible timestamps. Based on this we can
  -- formulate the folowing specialized definition:
  --
  -- [The DE MoC] is abstracting the execution semantics of a system
  -- where synchronization is /discretized/ and /time-aware/, and it
  -- is performed whenever a new event occurs.
  --
  -- There are many variants of discrete event simulators, each of
  -- them implementing slight variations of the semantics stated in
  -- <ForSyDe-Atom.html#lee98 [Lee98]>. The execution model covered by
  -- the DE implementation of ForSyDe-atom may be described as a
  -- simplified "cycle simulator" with no delta-delay nor superdense
  -- time. The signals behave as "latched channels" (similar to an HDL
  -- simulator), and processes react instantaneously to any new
  -- event. While the simplicity of the execution engine is a desired
  -- one, more complex behaviors such as zero-time feedback, non-zero
  -- reaction time and communication protocols (e.g. lossy buffers)
  -- may be achieved by composing patterns from the "ForSyDe.Atom.MoC"
  -- and/or "ForSyDe.Atom.ExB" layers. Nevertheless, the DE behaviors
  -- possible within ForSyDe-Atom are included in the class of
  -- /conservative simulators/ as presented in
  -- <ForSyDe-Atom.html#fuji00 [Fujimoto00]>, due to the dataflow
  -- nature of the evaluation mechanisms. Below you can see an example
  -- of a simple DE process, without behavior extensions:
  --
  -- <<docfiles/figs/moc-de-example.png>>
  --
  -- Below are stated a few particularities of our DE MoC
  -- implementation:
  --
  -- 1. According to <ForSyDe-Atom.html#lee98 [Lee98]>, our DE MoC is
  -- a one-sided system, i.e. time starts from an absolute 0. While
  -- negative time cannot be represented, signals can be phase-aligned
  -- with the help of the '-&-' atom. The discrete aspect is is
  -- enforced literally, by representing tags with natural numbers.
  --
  -- 1. tags are explicit and a DE event will construct a type around
  -- both a tag and a value. Tags represent the start time of the
  -- event, the end time being implicit from the start time of the
  -- next event. By doing so, we ensure that our DE MoC is be a
  -- special case of CT (i.e. the time domain is non-disjoint).
  --
  -- 1. according to the previous point, events are assumed to persist
  -- from their time of arrival until the next event arrives or, if
  -- there is no incoming event, until infinity. The default behavior
  -- can be regarded either as a persistent channel (e.g. wire), or as
  -- non-blocking buffered input, with the buffers of size 1.
  --
  -- 1. as a consequence to the previous is that feedback loops will
  -- generate an infinite number of events (strictly preceding each
  -- other), since a loop updates the value after a certain delay, and
  -- any input is assumed to go to infinity. Thus we can now fully
  -- jutify the definition of the 'ForSyDe.Atom.MoC.delay' pattern as
  -- consisting in a /prepend/ (i.e. generating the new value) and a
  -- /phase shift/ (i.e. advancing time with a positive integer). This
  -- is done in order to both preserve causality /and/ avoid deadlock.
  --
  -- 1. due to the reactive and dataflow natures of the execution
  -- system, DE processes /are forbidden/ to clean up events. Doing so
  -- might lead to deadlock wherever any feedback is involved. This
  -- means that a new event is created every time a new event arrives,
  -- regardless of what value it carries. This means that /all/ values
  -- are propagated, justifying our system's conservative approach
  -- <ForSyDe-Atom.html#fuji00 [Fujimoto00]>. Atoms themselves do not
  -- clean signals, but using interfaces that do should be treated
  -- with extreme special care, as it is considered unsafe and
  -- deadlock-prone.
  --
  -- 1. due to the conservative approach, and to the fact that MoC
  -- atoms are independent synchronization entities, ForSyDe
  -- simulators are completely parallelizable, since processes are
  -- self-sufficient and do not depend on a global event queue (as
  -- compared to other cycle simulators).
  --
  -- 1. any signal from outside needs to be sane (/T/ must be a total
  -- order) before being injected into a ForSyDe process
  -- network. Helper functions are equipped with sanity
  -- checkers. Inside a ForSyDe process network, transformations are
  -- rate-monotonic, thus output signals are guaranteed to be sane.
  --
  -- 1. since /T/ is a total orderm there is no need for an
  -- <ForSyDe-Atom-MoC.html#context execution context> and we can
  -- ignore the formatting of functions in "ForSyDe.Atom.MoC", thus we
  -- can safely assume:
  --
  -- <<docfiles/figs/eqs-moc-timed-context.png>>

  Tag, DE(..),

  -- * Aliases & utilities

  -- | A set of type synonyms and utilities are provided for
  -- convenience. The API type signatures will feature these aliases
  -- to hide the cumbersome construction of atoms and patters as seen
  -- in "ForSyDe.Atom.MoC".

  Signal, unit, unit2, unit3, unit4, infinite, signal, checkSignal,
  
  -- * @DE@ process constuctors

  -- | The DE process constructors are basically specific
  -- instantiations of patterns defined in "ForSyDe.Atom.MoC". Some
  -- might also be wrapping functions in an extended behavioural
  -- model.

  -- ** Simple

  -- | These are mainly direct instantiations of patterns defined in
  -- "ForSyDe.Atom.MoC", using DE-specific utilities.
  
  delay, delay',
  
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

  -- -- ** Interface processes

  -- toSY, toSY2, toSY3, toSY4,
  -- toCT, toCT2, toCT3, toCT4,

  -- zipx, unzipx,
  
  -- -- ** Hybrid processes

  -- embedSY11, embedSY12, embedSY13, embedSY14,
  -- embedSY21, embedSY22, embedSY23, embedSY24,
  -- embedSY31, embedSY32, embedSY33, embedSY34,
  -- embedSY41, embedSY42, embedSY43, embedSY44,
      
 ) where

import ForSyDe.Atom.MoC.DE.Core
import ForSyDe.Atom.MoC.DE.Lib
-- import ForSyDe.Atom.MoC.DE.Hybrid
-- import ForSyDe.Atom.MoC.DE.Interface
