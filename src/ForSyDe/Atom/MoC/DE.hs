{-# OPTIONS_HADDOCK show-extensions, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE
-- Copyright   :  (c) George Ungureanu, 2016-2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @DE@ library implements a DSL of atoms that operate according to a (safe
-- instance of) the discrete event model of computation, along with helpers and
-- patterns associated with this MoC.
--
-- This module exports a reduced interpretation of a DE language, where we assume that
-- each event in a DE signal is persistent, i.e. happened at a discrete instant, and
-- is buffered until a new event occurs. This enables to exploit the "dataflow" host
-- of 'ForSyDe.Atom.MoC.Stream's in a conservative and /completely/ deterministic
-- manner. For an experimental DE language where events are not persistent but
-- instantaneous, which goes along the classical interpretation of DE
-- <ForSyDe-Atom.html#cassandras09 [Cassandras09]> please check
-- "ForSyDe.Atom.MoC.DE.React".
--
-- Useful pointers:
--
-- * "ForSyDe.Atom" contains general guidelines for using the API
--
-- * "ForSyDe.Atom.MoC" documents details about the internals of the MoC layer, the
--   atoms and the basic structure of all process constructors as MoC patterns.
--
-- * "ForSyDe.Atom.Utility.Plot" contains useful utilities for plotting DE signals.
--
-- * "ForSyDe.Atom.MoC.DE.React" contains a (possibly non-conservative) DE language
--   where events are treated as non-persistent.
--
-- * the <ForSyDe-Atom.html#naming_conv naming convention> rules on how to interpret
--   the function names based on their number of inputs and outputs.
---------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE (

  -- * Discrete event (@DE@)

  -- | According to the tagged signal model <ForSyDe-Atom.html#lee98 [Lee98]>, "a
  -- discrete-event system is a timed system \(Q\) where for all \(s\in Q\), the tag
  -- sytem is order-isomorphic to a subset of the integers. Order-isomorphic means
  -- that there exists an order-preserving bijection between the events in and a
  -- subset of the integers (or the entire set of integers)."
  --
  -- The discrete event (@DE@) MoC describes a notion of physical time through its
  -- tags, also called timestamps. An important property of the DE tag system is that
  -- between any two timestamps \(t_u\) and \(t_v\) there is a __/finite/__ number of
  -- possible timestamps. Based on these observations we can formulate the folowing
  -- simplified definition:
  --
  -- [The DE MoC] is abstracting the execution semantics of a system where
  --   synchronization is /discrete/ (i.e. happens at discrete instants in time) and
  --   /time-dependent/ (i.e. is based on an implicit algebra of tags).
  --
  -- There are many variants of discrete event simulators in literature. The execution
  -- model covered by the DE implementation in the "ForSyDe.Atom.MoC.DE" module may be
  -- described as a /"strictly causal cycle simulator"/ with no delta-delay nor
  -- superdense time. The signals behave as "persistent channels" (similar to an HDL
  -- simulator), and processes react instantaneously to any new event. The simplicity
  -- of the execution model is ideal for modeling safe DE systems, its monotonic
  -- dataflow internals making it somewhat comparable to the /conservative discrete/
  -- /event simulators/ (see <ForSyDe-Atom.html#fuji00 [Fujimoto00]>). Below you can
  -- see an example of a simple DE process, without any behavior extensions:
  --
  -- <<fig/moc-de-example.png>>
  --
  -- A variant of the DE MoC where events are instantaneous and non-triggering
  -- behavior is implemented in "ForSyDe.Atom.MoC.DE.React". Below are stated a few
  -- particularities of this DE MoC implementation:
  --
  -- 1. in the terms of <ForSyDe-Atom.html#lee98 [Lee98]>, our DE MoC is a one-sided
  --    system, i.e. time starts from an absolute \(0\). While negative time cannot be
  --    represented, signals can be "phase-aligned" with the help of the
  --    'ForSyDe.Atom.MoC.-&-' atom. All signals need to start from timestamp \(0\),
  --    and events need to be positioned with their tags in strict ascending
  --    order. The 'checkSignal' utility enforces these rules. This restriction is
  --    lifted in "ForSyDe.Atom.MoC.DE.React".
  --
  -- 1. tags are explicit and a 'DE' event will construct a type around both a tag and
  --    a value. Tags represent the start time of the event, the end time being
  --    implicit from the start time of the next event. By doing so, we ensure that
  --    the time domain is non-disjoint and continuous. This implies that, at any
  --    instant in time a 'DE' system describes /one specific state/.
  --
  -- 1. events are assumed to persist from their time of arrival until the next event
  --    arrives or, if there is no incoming event, until infinity. Hence, signals can
  --    be interpreted as either persistent channels (e.g. latched wires), or
  --    non-blocking buffers of size 1.
  --
  -- 1. a safe 'delay' consists in a prepend 'ForSyDe.Atom.MoC.-<-' (i.e. generating
  --    the new value) and a phase shift 'ForSyDe.Atom.MoC.-&-' (i.e. advancing time
  --    with a positive integer). This is done in order to both preserve causality and
  --    avoid deadlock.
  --
  -- 1. as a consequence a pure 'DE' feedback loop will generate an infinite number of
  --    events (strictly preceding each other), since it updates the value after a
  --    certain delay, and any input is assumed to go to infinity. Hence /all the/
  --    /stateful processes that involve a feedback loop, e.g. 'state22', 'moore22',/
  --    /'mealy22' are, by design choice, exposing this type of oscillating/
  --    /behavior/. For sateful processes that react "instantaneously", one should
  --    consider defining a hybrid SY/DE process, see 'embedSY22'.
  --
  -- 1. due to the reactive dataflow natures of the host 'ForSyDe.Atom.MoC.Stream's,
  --    DE processes /should not/ to clean up events. Doing so might lead to deadlock
  --    wherever any feedback is involved. This means that a new event is created
  --    every time a new event arrives, regardless of what value it carries. This
  --    means that /all/ values are propagated, justifying our system's conservative
  --    approach <ForSyDe-Atom.html#fuji00 [Fujimoto00]>. 'DE' atoms do not clean
  --    signals, however this restriction is lifted in "ForSyDe.Atom.MoC.DE.React".
  --
  -- 1. due to the pure and conservative approach, ForSyDe DE simulations, although
  --    not very efficient (i.e. suffer from the same drawback as the fist generation
  --    of conservative approaches, namey "lookahead creep", see /unpublished yet/),
  --    are completely parallelizable. This is because processes are self-sufficient
  --    and do not depend on a global event queue (as compared to other cycle
  --    simulators).
  --
  -- 1. any signal from outside needs to be sane ( \(T\) must be a total order) before
  --    being injected into a ForSyDe process network. Helper functions are equipped
  --    with sanity checkers. Inside a ForSyDe process network, transformations are
  --    monotonic, thus output signals are guaranteed to be sane.
  --
  -- 1. since \(T\) is a total order, there is no need for an
  --    <ForSyDe-Atom-MoC.html#context execution context> and we can ignore the
  --    formatting of functions in "ForSyDe.Atom.MoC", thus
  --    \[ \Gamma\vdash\alpha\rightarrow\beta = \alpha\rightarrow\beta \]

   DE(..),

  -- * Aliases & utilities

  -- | These are type synonyms and utilities provided for user
  -- convenience. They mainly concern the construction and usage of
  -- signals.

  SignalBase, Signal, TimeStamp, 
  unit, unit2, unit3, unit4, infinite, until,
  signal, checkSignal, readSignal,
  
  -- * @DE@ process constuctors

    -- | These are specific implementations of the atom patterns defined in
  -- "ForSyDe.Atom.MoC".
  
  -- ** Simple

  -- | These are mainly direct instantiations of patterns defined in
  -- "ForSyDe.Atom.MoC", using DE-specific utilities.
  
  delay, delay', 
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  
  reconfig11, reconfig12, reconfig13, reconfig14,
  reconfig21, reconfig22, reconfig23, reconfig24,
  reconfig31, reconfig32, reconfig33, reconfig34,
  reconfig41, reconfig42, reconfig43, reconfig44,

  sync2, sync3, sync4, pwm,
  
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

  -- ** Hybrid

  -- | Processes that can wrap other MoCs inside a DE execution model.

  embedSY11, embedSY12, embedSY13, embedSY14,
  embedSY21, embedSY22, embedSY23, embedSY24,
  embedSY31, embedSY32, embedSY33, embedSY34,
  embedSY41, embedSY42, embedSY43, embedSY44,

  -- ** Interfaces

  toSY1, toSY2, toSY3, toSY4,
  toCT1, toCT2, toCT3, toCT4,
  hold1, hold2, hold3, hold4,

  zipx, unzipx, unzipx',
        
 ) where

import ForSyDe.Atom.MoC.DE.Core
import ForSyDe.Atom.MoC.DE.Hybrid
import ForSyDe.Atom.MoC.DE.Interface
import ForSyDe.Atom.MoC.DE.Lib
import ForSyDe.Atom.MoC.TimeStamp

import Prelude hiding (until)
