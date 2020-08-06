{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE PostfixOperators #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT
-- Copyright   :  (c) George Ungureanu, 2016-2018
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @CT@ library implements a DSL of atoms that operate according to a plactical
-- interpretation of the continuous time model of computation, along with helpers and
-- associated patterns.
--
-- This interpretation of the CT MoC is an /extension/ of the 'ForSyDe.Atom.MoC.DE.DE'
-- MoC in the sense that it borrow its algebra of discrete interactions
-- entirely. However, there is one major difference:
--
-- * in 'ForSyDe.Atom.MoC.DE.DE' signals carry values of an \(\alpha\) type, which is
--   conveniently describing persistent values between two (consecutive) discrete
--   events;
--
-- * in 'CT' signals instead carry functions of type \(\tau\rightarrow\alpha\) which
--   evolve in time.
--
-- The type \(\tau\) is an arbitrary (yet appropriate) numerical representation for
-- continuous time (e.g. see the in-built 'ForSyDe.Atom.MoC.Time.Time' alias). By
-- abstracting the time away from signals and keeping it as an (arbitrary) argument, a
-- ForSyDe-Atom CT process network is able to describe and manipulate symbolic (pure)
-- continuous functions, the numerical representation becoming apparent only when the
-- output is forced to be evaluated at discrete points in time, e.g. for the purpose
-- of plotting. Hence this CT MoC DSL is describing "an algebra of discrete
-- interactions between continuous sub-signals". For more information, please consult
-- <ForSyDe-Atom.html#ungureanu18 [Ungureanu18]>.
--
-- Useful pointers:
--
-- * "ForSyDe.Atom" contains general guidelines for using the API
--
-- * "ForSyDe.Atom.MoC" documents details about the internals of the MoC layer, the
--   atoms and the basic structure of all process constructors as MoC patterns.
--
-- * "ForSyDe.Atom.Utility.Plot" contains useful utilities for sampling and plotting
--   CT signals.
--
-- * "ForSyDe.Atom.MoC.DE" is the "host" DSL for the CT MoC, inferring the semantics
--  of discrete interactions.
--
-- * the <ForSyDe-Atom.html#naming_conv naming convention> rules on how to interpret
--   the function names based on their number of inputs and outputs.
----------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT (

  -- * Continuous time (@CT@) event

  -- | According to <ForSyDe-Atom.html#lee98 [Lee98]>, "[regarding metric time] at a
  -- minimum, \(T\) is an Abelian group, in addition to being totally ordered. A
  -- consequence is that \(t_2-t_1\) is itself a tag \(\forall t_1,t_2 \in T\). In a
  -- slightly more elaborate model of computation, \(T\) has a metric. (...) A
  -- continuous-time system is a metric timed system \(Q\) where \(T\) is a continuum
  -- (a closed connected set)."
  --
  -- The continuous time (@CT@) MoC defines the closest behavior to what we could call
  -- "physical time", where signals cover the full span of a simulation as functions
  -- of time rather than values. As such, we can state:
  --
  -- [The CT MoC] is describes a system where computation is performed continuously
  --   over a (possibly infinite) span of time.
  --
  -- For a detailed descrption of the ForSyDe-Atom CT execution model, refer to
  -- <ForSyDe-Atom.html#ungureanu18 [Ungureanu18]>. Below is an illustration of the
  -- behavior in time of the input and the output signals of a CT process:
  --
  -- <<fig/moc-ct-example.png>>
  --
  -- Our 'ForSyDe.MoC.CT.CT' MoC is implemented as a generalized version of
  -- ForSyDe-Atom's 'ForSyDe.MoC.DE.DE', or rather the 'ForSyDe.MoC.DE.DE' MoC is a
  -- special case of 'ForSyDe.MoC.CT.CT' in the sense that:
  --
  -- 1. tags \(t\) represent discrete timestamps when changes in a CT signal happen,
  --    or rather are observed (see below).
  --
  -- 1. "events" are represented as /functions/ over a continuous span of time
  --    \(f(t)\) rather than, like in most simulators based on imperative programs,
  --    series of sampled values. The time domain is abstracted away, and may be
  --    represented with whatever data type the user deems appropriate or convenient.
  --
  -- 1. for practical reasons, the 'CT' event constructor has also a /phase/ component
  --    \(\phi\), which is taken into consideration only when evaluating a signal
  --    function, i.e. \(f(t+\phi)\). This enables the modeling of "phase
  --    dispacements" of delay lines without altering the function itself,
  --    i.e. without increasing the complexity of the un-evaluated functions
  --    (redexes). The phase is reset during event synchronization.
  --
  -- <<fig/misc-ct-model.png>>
  --
  -- Based on the these particularities we can say that the 'CT' MoC is simply a
  -- 'ForSyDe.Atom.DE.DE' machine/observer operating on continuous subdomains, and we
  -- can formulate the following properties:
  --
  -- 1. 'CT' signals represent /discrete/ changes in a continuous function over time
  --    (e.g. analog signal). While the functions carried by events are infinite (have
  --    always happened and will always happen), they are interpreted as being
  --    "active" e.g. during two consecutive discrete times. A CT signal can be
  --    represented by the analog circuit above, where the inputs are continuous
  --    signals, but the switch is discrete. Like in the 'ForSyDe.MoC.DE.DE' MoC, the
  --    absolute time \(0\) represent the time when the system started to be observed.
  --
  -- 1. events carry /functions/ and not /values/. In a lazy evaluation system like
  --    Haskell's, functions are kept symbolic until evaluation. This means that in a
  --    CT system computations are propagated as function AST until a result is
  --    needed, e.g. a signal needs to be plotted for arbitrary time instants. The
  --    cost of higher plot resolution is the cost of evaluating the final
  --    (reduced-form) output expression only, and not more intermediate computations.
  --
  -- 1. since itself the 'ForSyDe.MoC.CT.CT' MoC is simply a 'ForSyDe.MoC.DE.DE'
  --    system operating on continuous subdomains, all atom evaluation properties are
  --    inherited from it: feedback loops need to advance time, atoms are forbidden to
  --    clean signals, and the conservative approach makes it possible
  --    parallelize/distribute the simulation.
  --
  -- 1. since \(T\) is a total order, there is no need for an
  --    <ForSyDe-Atom-MoC.html#context execution context> and we can ignore the
  --    formatting of functions in "ForSyDe.Atom.MoC", thus
  --    \[ \Gamma\vdash\alpha\rightarrow\beta = \alpha\rightarrow\beta \]
 
  TimeStamp, Time, CT(..),

  -- * Aliases & utilities

  -- | A set of type synonyms and utilities are provided for convenience. The API type
  -- signatures will feature these aliases to hide the cumbersome construction of
  -- atoms and patters as seen in "ForSyDe.Atom.MoC".

  SignalBase, Signal, unit, unit2, unit3, unit4, infinite,
  signal, checkSignal, 
  
  -- * @CT@ process constuctors

  -- | The CT process constructors are specific instantiations of patterns defined in
  -- "ForSyDe.Atom.MoC".
  --
  -- In the examples below, we use our 'Signal' type, i.e. a 'SignalBase' with
  -- "ForSyDe.Atom.MoC.TimeStamp" for tag type and "ForSyDe.Atom.MoC.Time" for time
  -- type. Consequently we use functions such as @e'@ @pi'@, @sin'@ and @cos'@ from
  -- their respective collection of utilities. Also, for the sake of documentation the
  -- interactive examples are only dumping the CT signals in data files using the
  -- 'dumpDat' utility defined in "ForSyDe.Atom.Utility.Plot", according to the custom
  -- @cfg@ structure. These files can be further plotted by any tool of choice, or
  -- using the plotting utilities provided in the "ForSyDe.Atom.Utility.Plot" module.
  --
  -- > import ForSyDe.Atom.Utility.Plot
  -- > import ForSyDe.Atom.MoC.Time as Time
  -- > import ForSyDe.Atom.MoC.TimeStamp as TimeStamp
  -- > let pi'  = TimeStamp.pi
  -- > let exp' = Time.exp
  -- > let sin' = Time.sin
  -- > let cos' = Time.cos
  -- > let cfg  = defaultCfg {xmax=10, rate=0.1}

  -- ** Simple

  -- | These are mainly direct instantiations of patterns defined in
  -- "ForSyDe.Atom.MoC", using CT-specific utilities.

  delay, delay',
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  reconfig11, reconfig12, reconfig13, reconfig14,
  reconfig21, reconfig22, reconfig23, reconfig24,
  reconfig31, reconfig32, reconfig33, reconfig34,
  reconfig41, reconfig42, reconfig43, reconfig44,

  constant1, constant2, constant3, constant4,
  infinite1, infinite2, infinite3, infinite4,

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

  -- ** Interfaces

  toDE1, toDE2, toDE3, toDE4,
  sampDE1, sampDE2, sampDE3, sampDE4,
  zipx, unzipx, unzipx'
  
  ) where

import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.MoC.CT.Core
import ForSyDe.Atom.MoC.CT.Lib
import ForSyDe.Atom.MoC.CT.Interface
