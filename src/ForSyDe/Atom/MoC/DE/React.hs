{-# OPTIONS_HADDOCK show-extensions, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.React
-- Copyright   :  (c) George Ungureanu, 2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an experimental library implementing the atoms and a set of specific
-- patterns for representing the discrete event model of computation
-- <ForSyDe-Atom.html#cassandras09 [Cassandras09]>. One main source of inspiration for
-- this MoC execution model is the /reactor model/
-- <ForSyDe-Atom.html#lohstroh19 [Lohstroh19]>, hence the abbreviation 'RE'.
--
-- #detector# This module exports a more liberal interpretation of DE than
-- "ForSyDe.Atom.MoC.DE", where events are considered non-persistent,
-- i.e. "instantaneous". Unambiguous behaviors for processes can be achieved by virtue
-- of pattern-matching agains the presence/absence of events at different input ports
-- at specific time instants (see API examples). This type of behavior implies a
-- dynamic/adaptive rate of consuming and producing events in streams, hence
-- internally each 'RE' process is a /detector/-/kernel/ pair along the lines of
-- "ForSyDe.Atom.MoC.SDF.SADF", as shown in the picture below. In this representation
-- the detector monitors the tags of each incoming event and for each "earliest event"
-- it sets the execution context \(\Gamma\) dictating the event consumption rate of
-- the kernel, here implemented using 'RE' atoms.
--
-- <<fig/moc-re-principle.png>>
--
-- Useful pointers:
--
-- * "ForSyDe.Atom" contains general guidelines for using the API
--
-- * "ForSyDe.Atom.MoC" documents details about the internals of the MoC layer, the
--   atoms and the basic structure of all process constructors as MoC patterns.
--
-- * "ForSyDe.Atom.MoC.DE" contains a conservative DE language where events are
--   treated as persistent, each new event is triggering a new action seen on all
--   outputs.
--
-- * the <ForSyDe-Atom.html#naming_conv naming convention> rules on how to interpret
--   the function names based on their number of inputs and outputs.
---------------------------------------------------------------------
module ForSyDe.Atom.MoC.DE.React (

  -- | The definitions of the DE MoC are the same as the ones presented in the
  -- "ForSyDe.Atom.MoC.DE" module, however this library implements a non-conservative
  -- interpretation of this MoC, more appropriate to model timed computing
  -- applicatios. In this sense signals are carrying "instant", non-persistent
  -- events. The user is then responsible to define how the process should react in
  -- case new events exist or not at any of the input ports, respectively if the
  -- process reacts at all. This is done by pattern-matching the functions passed as
  -- arguments: at any evaluation instant, a process passes to its function either
  -- singleton lists @[x]@ if new events exist at a particular input port for a
  -- particular timestamp, respectively empty lists @[]@ if no event has triggered the
  -- respective port. This modeling style gives rise to two new very important
  -- notions. For any process \(p(si^m)=so^n\) we say that:
  --
  -- * an input signal \(si_i\) is associated with a /triggering port/ with respect to
  --   an output signal \(so_j\) if \(\forall e_i\in si_i, \exists e_o\in so_j\),
  --   i.e. every event observed on the input port associated with \(si_i\) triggers a
  --   reaction observed in \(so_j\). Using the "ForSyDe.Atom.MoC.DE.React" API we can
  --   model this along the lines of:
  --
  -- > (...,so_j,...) = p f ... si_i ...
  -- >   where f ... [a_i] ... = (...,[b_j],...)
  --
  -- * an input signal \(si_i\) is associated with a /non-triggering/ or /observing/
  --   /port/ with respect to an output signal \(so_j\) if \(\forall e_i\in si_i,
  --   \nexists e_o\in so_j\), i.e. there exist events observed on the input port
  --   associated with \(si_i\) which do not trigger a reaction observable in 
  --   \(so_j\). Using the "ForSyDe.Atom.MoC.DE.React" API we can model this along the
  --   lines of:
  --
  -- > (...,so_j,...) = p f ... si_i ...
  -- >   where f ... [a_i] ... = (...,[],...)
  --
  -- The non-triggering behavior has deep implications and needs to be treated with
  -- care especially when considering feedback loops, as it violates the
  -- "non-cleaning" rule imposed by the host 'ForSyDe.Atom.MoC.Stream'. In the picture
  -- below you see an example behavior of a 'RE' process where both inputs are
  -- triggering, and function \(\Sigma\) @a b = [sum a + sum b]@.
  --
  -- <<fig/moc-re-example.png>>
  --
  -- In this example, if only the first signal is triggering, i.e.
  --
  -- > f [a] b = [a + sum b]
  -- > f _   _ = []
  --
  -- then
  --
  -- <<fig/moc-re-example2.png>>
  --
  -- A conservative variant of the DE MoC is in "ForSyDe.Atom.MoC.DE.React". Below are
  -- stated a few particularities of this DE MoC implementation:
  --
  -- 1. As compared to "ForSyDe.Atom.MoC.DE" this MoC is also one-sided, but time
  --    \(0\) is not necessarily the global start any longer, because there exists the
  --    notion of "absent" event. Tags in a signal still need to be strictly
  --    incremental, but a signal can start at any timestamp, including negative ones.
  --
  -- 1. since events are non-persistent, they are considered to be "lost", or
  --    discarded after they happen, and a process is not allowed to "go back in time"
  --    to retrieve them. Persistent behaviors can be modeled in this MoC with
  --    e.g. buffer patterns, see 'syncAndHold2'.
  --
  -- 1. a safe 'delay' is still considered one which both prepends
  --    'ForSyDe.Atom.MoC.-<-' (i.e. generating a new value) and a phase shifts
  --    'ForSyDe.Atom.MoC.-&-' (i.e. advancing time with a positive integer). However,
  --    since we are not restricted to start from global \(0\) any longer, we can also
  --    use an 'unsafeDelay' variant. This process is named so because it does not
  --    have a prepend behavior, which means that is /never/ allowed to be part of any
  --    feedback datapath, otherwise it causes a simulation deadlock.
  --
  -- 1. also because events are non-persistent, /as a design choice, stateful/
  --    /processes that involve feedback pattern (see 'state22', 'stated22',/
  --    /'moore22', 'mealy22') do not expose oscillating behavior/, but rather an
  --    instantaneous one (i.e. they embed a 'ForSyDe.Atom.MoC.SY.SY' behavior).
  --
  -- 1. since signal cleaning behavior is possible, it might cause unexpected
  --    deadlocks, and should be avoided altogether. However, if this situation cannot
  --    be avoided, the designer should take /special care/ to enforce the following
  --    rule: /all ports involved in a feedback loop's datapath need to be/
  --    /triggering, and pass through at lease one safe 'delay'/.
  --
  -- 1. any signal from outside needs to be sane ( \(T\) must be a total order) before
  --    being injected into a ForSyDe process network. Helper functions are equipped
  --    with sanity checkers. Inside a ForSyDe process network, transformations are
  --    monotonic, thus output signals are guaranteed to be sane.
  --
  -- 1. Since the execution of a process is dictated by the arrival of events, then
  --    the 'RE' atoms require the triggering information as a set of Booleans passed
  --    through the <ForSyDe-Atom-MoC.html#context execution context>: \[
  --    \Gamma\vdash\alpha\rightarrow\beta =
  --    [\mathtt{Bool}]\times([\alpha]\rightarrow[\beta]) \] This context however does
  --    not need to be touched by the user of this API, but rather is taken care of by
  --    the <#detector detector pattern>.

  RE(..),
  
  -- * Aliases & utilities

  -- | These are type synonyms and utilities provided for user
  -- convenience. They mainly concern the construction and usage of
  -- signals.

  Signal, SignalBase, unit, unit2, unit3, unit4, infinite, until,
  signal, checkSignal, readSignal,
  
   -- * @DE@ process constuctors

  -- | These SY process constructors are basically specific
  -- instantiations of the atom patterns defined in
  -- "ForSyDe.Atom.MoC".
  
  -- ** Simple

  -- | These are mainly direct instantiations of patterns defined in
  -- "ForSyDe.Atom.MoC", using DE-specific utilities.
  
  delay, unsafeDelay, delay',
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  
  constant1, constant2, constant3, constant4,

  generate1, generate2, generate3,
  
  state11, state12, state13,
  state21, state22, 
  state31, 

  stated11, stated12, stated13,
  stated21, stated22, 
  stated31, 


  moore11, moore12, moore13,
  moore21, moore22, moore23,
  moore31, moore32, moore33,

  mealy11, mealy12, mealy13,
  mealy21, mealy22, mealy23,
  mealy31, mealy32, mealy33,

   syncAndHold2, syncAndHold3, syncAndHold4,
   syncAndFill2, syncAndFill3, syncAndFill4,

   -- ** Interfaces

   -- ** Lingua Franca constructs

   timer0, timer, timer',

   state1, state2, state3,

   actionD,

  reaction11, reaction12, reaction13, reaction14,
  reaction21, reaction22, reaction23, reaction24,
  reaction31, reaction32, reaction33, reaction34,
  reaction41, reaction42, reaction43, reaction44,
  
  ) where

import ForSyDe.Atom.MoC.DE.React.Core
import ForSyDe.Atom.MoC.DE.React.Lib
import ForSyDe.Atom.MoC.DE.React.LF
import Prelude hiding (until)
