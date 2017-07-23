{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE PostfixOperators #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @CT@ library implements the atoms holding the sematics for the
-- continuous time computation model. It also provides a set of
-- helpers for properly instantiating process network patterns as
-- process constructors.
--
-- For working with time or timestamps please check the utilities
-- provided by the "ForSyDe.Atom.MoC.Time" and
-- "ForSyDe.Atom.MoC.TimeStamp" modules.
--
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
----------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT (

  -- * Continuous time (@CT@) event

  -- | According to <ForSyDe-Atom.html#lee98 [Lee98]>, "[regarding
  -- metric time] at a minimum, /T/ is an Abelian group, in addition
  -- to being totally ordered. A consequence is that /t&#8322;/ -
  -- /t&#8321;/ is itself a tag &#8704; /t&#8321;/, /t&#8322;/ &#8712;
  -- /T/. In a slightly more elaborate model of computation, /T/ has a
  -- metric. (...) A continuous-time system is a metric timed system
  -- /Q/ where /T/ is a continuum (a closed connected set)."
  --
  -- The continuous time (@CT@) MoC defines the closest behavior to
  -- what we could call "physical time", where signals cover the full
  -- span of a simulation as "functions of time" rather than
  -- "values". As such, we can state:
  --
  -- [The CT MoC] is abstracting the execution semantics and describes
  -- a system where computation is performed continuously over a
  -- (possibly infinite) span of time.
  --
  -- Below is an illustration of the behavior in time of the input and
  -- the output signals of a CT process:
  --
  -- <<docfiles/figs/moc-ct-example.png>>
  --
  -- Our CT MoC is implemented as an enhanced version of
  -- 'ForSyDe.MoC.DE.DE' with respect to the __CT MoC__ definition, in
  -- the sense that:
  --
  -- 1. tags /t/ are also represented with
  -- 'ForSyDe.Atom.MoC.TimeStamp's, thus we can say that changes in a
  -- CT signal happen at discrete times (see below).
  --
  -- 1. values are represented as functions over a continuous span of
  -- time /f(t)/ rather than just a value or a series of values. The
  -- time domain is represented with rational numbers which, as
  -- compared to floating point numbers, do not suffer from inherent
  -- quantisation, being able to model true continuity, i.e. between
  -- any two arbitrary points in time there is an infinite amount of
  -- intermediate moments.
  --
  -- 1. The event constructor has also a /phase/ component /&#966;/,
  -- which is taken into consideration only when evaluating the event
  -- function, i.e. /f (t + &#966;)/. This enables the modeling of
  -- "phase dispacements" of delay lines without altering the function
  -- itself (and thus increasing the complexity of the un-evaluated
  -- function graph). The phase needs to be reset during event
  -- synchronization.
  --
  -- These seemingly minor changes have deep implications in the
  -- expressiveness of a FoSyDe CT system and how we interpret
  -- it. Capturing the particularities of this MoC, we can formulate
  -- the following properties:
  --
  -- <<docfiles/figs/misc-ct-model.png>>
  --
  -- 1. 'CT' signals, due to their formation as streams of tagged
  -- events, represent /discrete/ changes in a continuous function
  -- over time (e.g. analog signal). While the functions carried by
  -- events are infinite (have always happened and will always
  -- happen), being carried by events in a tag system suggests that
  -- changes occur at discrete times. A CT signal can be represented
  -- by the analog circuit above, where the inputs are continuous
  -- signals, but the switch is discrete. Like in the
  -- 'ForSyDe.MoC.DE.DE' MoC, the absolute time 0 represent the time
  -- when the system started to be observed.
  --
  -- 1. the previous property is also proven by the fact that the
  -- evaluation engine of ForSyDe-Atom is inherently discrete,
  -- i.e. evaluation is performed whenever a new event occurs, in a
  -- dataflow manner. Allowing infinitely small distances between tags
  -- would hinder the advancement of simulation time.
  --
  -- 1. events carry /functions/ and not /values/. In a lazy
  -- evaluation system like Haskell's, functions are kept symbolic
  -- until evaluation. This means that in a CT system computations are
  -- propagated as function graphs until a result is needed, e.g. a
  -- signal is plotted for arbitrary positions in time. This way
  -- intermediate quantization errors are eliminated, and the cost of
  -- higher plot resolution is the cost of evaluating the final
  -- results only.
  --
  -- 1. needless to say, for each /t/ &#8712; /T/, a signal is able to
  -- return (e.g. plot) the exact value /v/ for that particular /t/.
  --
  -- 1. since itself the 'ForSyDe.MoC.CT.CT' MoC is just an enhanced
  -- 'ForSyDe.MoC.DE.DE' system, all atom evaluation properties are
  -- inherited from it: feedback loops need to advance time, atoms are
  -- forbidden to clean signals, and the conservative approach makes
  -- it ideal for parallel/distributed simulation.
  --
  -- 1. since /T/ is a total order, there is no need for an
  -- <ForSyDe-Atom-MoC.html#context execution context> and we can
  -- ignore the formatting of functions in "ForSyDe.Atom.MoC", thus we
  -- can safely assume:
  --
  -- <<docfiles/figs/eqs-moc-timed-context.png>>

  TimeStamp, Time, CT(..),

  -- * Aliases & utilities

  -- | A set of type synonyms and utilities are provided for
  -- convenience. The API type signatures will feature these aliases
  -- to hide the cumbersome construction of atoms and patters as seen
  -- in "ForSyDe.Atom.MoC".

  Signal, unit, unit2, unit3, unit4, infinite,
  signal, checkSignal, 

  plot, plot2, plot3, plot4,
  plotFloat, plotFloat2, plotFloat3, plotFloat4,
  
  -- * @CT@ process constuctors

  -- | The CT process constructors are basically specific
  -- instantiations of patterns defined in "ForSyDe.Atom.MoC". Some
  -- might also be wrapping functions in an extended behavioural
  -- model.
  --
  -- In the examples below we have imported and instantiated the
  -- functions such as @e'@ @pi'@, @sin'@ and @cos'@ from the
  -- collection of utilities in "ForSyDe.Atom.MoC.Time" and
  -- "ForSyDe.Atom.MoC.TimeStamp" :
  --
  -- > import ForSyDe.Atom.MoC.Time as Time
  -- > import ForSyDe.Atom.MoC.TimeStamp as TimeStamp
  -- > let pi'  = TimeStamp.pi
  -- > let exp' = Time.exp
  -- > let sin' = Time.sin
  -- > let cos' = Time.cos

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

  toDE, toDE2, toDE3, toDE4,
  zipx, unzipx, unzipx'
  
  ) where

import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.MoC.CT.Core
import ForSyDe.Atom.MoC.CT.Lib
import ForSyDe.Atom.MoC.CT.Interface
