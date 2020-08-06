{-# LANGUAGE PostfixOperators #-}
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

  -- | These are type synonyms and utilities provided for user convenience. They
  -- mainly concern the construction and usage of signals.

  Signal, SignalBase, unit, unit2, unit3, unit4, instant, until,
  signal, checkSignal, readSignal,
  
   -- * Process Constuctors

  -- | These process constructors are re-interpretations of the atom patterns defined
  -- in "ForSyDe.Atom.MoC", using the /reactor/ @comb@ (see 'comb22') presented
  -- <#detector# above>.
  
  -- ** Simple

  delay, delay', unsafeDelay, 
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  
  generate1, generate2, generate3,
  
  state11, state12, state13,
  state21, state22, state23,
  state31, state32, state33,

  stated11, stated12, stated13,
  stated21, stated22, stated23,
  stated31, stated32, stated33,

  moore11, moore12, moore13,
  moore21, moore22, moore23,
  moore31, moore32, moore33,

  mealy11, mealy12, mealy13,
  mealy21, mealy22, mealy23,
  mealy31, mealy32, mealy33,

  syncAndHold2, syncAndHold3, syncAndHold4,
  syncAndFill2, syncAndFill3, syncAndFill4,
  syncAndObs11, syncAndObs12, syncAndObs13,
  syncAndObs21, syncAndObs22, syncAndObs31,

  -- ** Interfaces

  toDE1, toDE2, toDE3, toDE4,
  fromDE1, fromDE2, fromDE3, fromDE4,

  toSYC1, toSYC2, toSYC3, toSYC4,
  fromSYC1, fromSYC2, fromSYC3, fromSYC4,

  embedSY11, embedSY12, embedSY13, embedSY14,
  embedSY21, embedSY22, embedSY23, embedSY24,
  embedSY31, embedSY32, embedSY33, embedSY34,
  embedSY41, embedSY42, embedSY43, embedSY44,
  
  -- * Lingua Franca constructs

  -- | This library also exports functions and helpers to re-create reactor networks
  -- from <https://github.com/icyphy/lingua-franca Lingua Franca> (LF) language
  -- <ForSyDe-Atom.html#lohstroh19 [Lohstroh19]>, as ForSyDe process
  -- networks. Basically each expression in the LF language is given an equivalent
  -- ForSyDe 'RE' construct. Due to fundamental differences between the two languages
  -- (LF being a parsed language and ForSyDe an EDSL hosted on the strictly-typed pure
  -- functional programming language Haskell) the syntax and user experience in
  -- general is quite different between the two languages. The scope of this library
  -- is merely to provide an (executable) behavioral model of LF reactors for study
  -- purposes rather than imitate/replace LF. In particular each construct in the LF
  -- language is associated with a ForSyDe process constructor, and thus @reactor@
  -- definitions in LF are built as specific composite processes in ForSyDe.
  --
  -- According to  the <https://github.com/icyphy/lingua-franca/wiki/Language-Specification Language Reference wiki>
  -- this is how a @reactor@ is defined in LF:
  --
  -- > reactor name (parameters) {
  -- >   state declarations
  -- >   input declarations
  -- >   output declarations
  -- >   timer declarations
  -- >   action declarations
  -- >   reaction declarations
  -- >    ...
  -- > }
  --
  -- Here is how a reactor would be defined as a process network/composite process
  -- using this library, along the lines of:
  --
  -- > reactorName :: ParamTypes -> InputSignalTypes ... -> OutputSignalTypes ...
  -- > reactorName param inputS ... = (outputS, ...)
  -- >   where
  -- >     stateS      = stateP nextStateF initV {inputS|timerS|actionS} ...
  -- >     timerS      = clockP
  -- >     actionS     = actionP {reactionS|other_actionS} ... 
  -- >     reactionS   = reactionP reactF reactInS
  -- >       where syncS = statesXinputsY initV stateS ... {inputS|timerS|actionS} ...
  --
  -- The pseudo-code above suggests the syntax and what kind of constructs a designer
  -- would use, to make a rough association between the ForSyDe-Reactors style and the
  -- LF-Reactors style. As with every functional program, the ForSyDe-Reactors
  -- processes are defined in an "equational" style. The elements in the pseudo-code
  -- denote:
  --
  -- * words between @{...|...}@ represents what different choices of
  -- input/output arguments;
  --
  -- * words with the @S@ suffix suggest that these identifiers are signals; words
  -- with @P@ suffix suggest that these identifiers are processes or process
  -- constructors; words with @F@ suffix suggest that these are functions, and @V@
  -- values respectively;
  --
  -- * @stateP@ are state modifiers, and should be stateful process constructors, see
  -- 'state22', 'stated22' 'moore22', 'mealy22';
  --
  -- * @clockP@ is a clock generator process, see 'timer0', 'timer', 'timer'';
  --
  -- * @actionP@ might be an action "delayer" if the action is to be delayed, or
  -- simply a name binding if it is not to be delayed;
  --
  -- * @reactionP@ is a combinational process constructor (see the 'reaction22'
  -- aliases), which operates on a set of /synchronized/ signals, using the
  -- @statesXinputsY@ utility (see 'states2inputs2'). This utility manipulates the
  -- different signals inside a reactor as to denote which originate from internal
  -- state processes (i.e. values are persistent until the next state modifier action
  -- occurs) and other/input actions (i.e. events are instant, and values are lost
  -- after the event is consumed).
  --
  -- Here is a simple example of how a LF reactor can be translated using this 
  -- library. Consider this LF code:
  --
  -- > reactor reactor1 (period:time) {
  -- >   input x:int;
  -- >   output y:int;
  -- >   timer t(period);
  -- >   state count:int(0);
  -- >   action a:int;
  -- >   reaction(t) -> a {=
  -- >     (self->count)++;
  -- >     schedule(a, MSEC(200), self->count)
  -- >   =}
  -- >   reaction(a,x) -> y {=
  -- >     set(y, a + x)
  -- >   =}
  -- > }
  --
  -- The ForSyDe equivalent model for @reactor1@ would be:
  --
  -- > reactor1 :: TimeStamp -> Signal Int -> Signal Int
  -- > reactor1 period x = y
  -- >   where
  -- >     t       = timer period
  -- >     count   = state11 (\s _ -> s + 1) 0 t
  -- >     a       = actionD (milisec 200) count
  -- >     y       = reaction21 (\i1 i2 -> [sum i1 + sum i2])
  -- >               $ states1inputs1 0 a x
  --
  -- Executing the above reactor with a test input signal in the @ghci@ interpreter
  -- gives you the output:
  --
  -- >>> let s2 = read "{1@0,2@3,3@6,4@10,5@13}" :: Signal Int
  -- >>> simulate 11.2 $ reactor1 1 s2
  -- 0s	1
  -- 0.2s	1
  -- 1.2s	2
  -- 2.2s	3
  -- 3s	5
  -- 3.2s	4
  -- 4.2s	5
  -- 5.2s	6
  -- 6s	9
  -- 6.2s	7
  -- 7.2s	8
  -- 8.2s	9
  -- 9.2s	10
  -- 10s	14
  -- 10.2s	11
  -- 11.2s	12

  
  -- ** Clock Generators
  --
  -- These processes generate signals with periodic events with no data.

  timer0, timer, timer',

  -- ** Delayed Actions

  -- | Non-delayed actions are not provided, because, as seen in the example above,
  -- they are simply (name bindings to) signals. Consequently delayed/scheduled
  -- actions are delayed signals in ForSyDe.

  actionD,

  -- ** Reactions

  -- | Reactions are ForSyDe combinational process constructors, having both
  -- triggering and non-triggering (observing) inputs. Due to the current
  -- implementation of the host 'RE' MoC, the inputs of a reaction need to come from a
  -- @statesXinputsY@ utility, which tells which signals are persistent
  -- (i.e. originating from stateful processes/reactions) or instantaneous (originatig
  -- from non-stateful reactions or inputs).
  --
  -- __Note:__ The equivalent of the @is_present@ construct in LF can be expressed
  -- using pattern-matching on process function arguments (see 'comb22').

  states1inputs1, states2inputs1, states3inputs1,
  states1inputs2, states2inputs2, states1inputs3,

  reaction11, reaction12, reaction13, reaction14,
  reaction21, reaction22, reaction23, reaction24,
  reaction31, reaction32, reaction33, reaction34,
  reaction41, reaction42, reaction43, reaction44,
  
  ) where

import ForSyDe.Atom.MoC.DE.React.Core
import ForSyDe.Atom.MoC.DE.React.Lib
import ForSyDe.Atom.MoC.DE.React.LF
import Prelude hiding (until)
