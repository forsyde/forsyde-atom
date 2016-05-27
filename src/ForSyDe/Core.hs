{-# OPTIONS_HADDOCK not-home, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2016;
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The formal foundation upon which ForSyDe defines the execution
-- semantics of heterogeneous cyber-physical systems is the tagged
-- signal model <#lee98 [1]>.  This is a denotational framework
-- introduced by Lee and Sangiovanni-Vincentelli as a common meta
-- model for describing properties of concurrent systems in general
-- terms as sets of possible behaviors. Systems are regarded as
-- /compositions/ of /processes/ acting on /signals/ which are
-- collections of events. Signals are characterized by a /tag system/
-- which determines causality between events, and could model time,
-- precedence relationships, synchronization points, and other key
-- properties. Based on how tag systems are defined, one can identify
-- several /Models of Computations (MoCs)/ as classes of behaviors
-- dictating the semantics of execution and concurrency in a network
-- of processes.
--
-----------------------------------------------------------------------------
module ForSyDe.Core (

  -- * Basic notions

  -- ** Signals

  -- | In ForSyDe a signal is represented as a (partially or totally)
  -- /ordered/ sequence of events that enables processes to
  -- communicate and synchronize.
  --
  -- [signals] are sets of events composed of tags /T/ and values /V/,
  -- where signal /s/ contains a set of events /e_j/. In other words,
  -- we can state that through its tag system, a signal is /bound/ to
  -- a MoC.
  --
  -- #sig-definition#
  -- <<includes/figs/tagged-signal-model.png>>

  Signal(..),

  -- | Additional functions for the 'Signal' data type are provided
  -- for covenience in library development. For an extended API
  -- documentation consult "ForSyDe.Core.Signal"

  
  -- | #processes#

  -- ** Processes
  
  -- | As described in <#lee98 [1]>, processes are either "set of
  -- possible behaviors" of signals or "relations" between multiple
  -- signals. One can describe complex systems by composing processes,
  -- which in this case is interpreted as the "intersection of the
  -- behaviors of each of the processes being involved".  ForSyDe
  -- inherits this definition with respect to a functional view:
  --
  -- [@process@ /p/] is a functional mapping over (the history of)
  -- signals. Instantiated using only process constructors.
  -- [@process constructor@ /pc/] is a higher order function embedding MoC
  -- semantics and/or a specific composition, but lacking
  -- functionality. 
  --
  -- #proc-definition#
  -- <<includes/figs/process_definition.png>>
  --
  -- Since processes are functions, process composition is equivalent
  -- to function composition. This means that composing two processes
  -- @p1@ and @p2@ grants the process @p2 . p1@
  --
  -- > p1      :: Signal alpha -> Signal beta
  -- > p2      :: Signal beta  -> Signal gamma
  -- > p2 . p1 :: Signal alpha -> Signal gamma
  --
  --  This implies that there is a signal @Signal beta @ that
  --  "streams" the result from @p1@ to @p2@, as suggested in the
  --  drawing:
  --
  -- <<includes/figs/ser-composition-formula.png>>
  --
  -- [@process networks@] in ForSyDe originate from Reekie's process
  -- nets <#reekie95 [2]> and describe ForSyDe systems in terms of
  -- (tuple of) compositions of processes. As seen in the type
  -- signature (function from signals to signals), a process network
  -- is a process itself. The composition above @p2 . p1 @ can also be
  -- regarded as a process network.
  --
  -- <<includes/figs/process-network-formula.png>>

  -- ** Extended values

  -- | Dealing with cyber-physical systems, ForSyDe needs to model
  -- special behaviour such as the absence of events, or
  -- non-deterministic values. This is expressed in the current
  -- library by extending the set of values /V/
  -- (see <#sig-definition signal definition>) with special tokens
  -- carrying behavioural semantics, by wrapping arbitrary types into
  -- a new data type. Currently @forsyde-atom@ supports the following
  -- special value extensions:
  --
  -- [@absent event@ &#8869;] determines the absence of an event at
  --time (tag) /t/
  -- [@undefined value@ ?] suggests a non-determinate value, similar
  -- to the "anything" (@x@ value) in VHDL
  --
  -- <<includes/figs/extended-values.png>>
  
  Value(..),

  -- ** Models of Computation (MoCs)
  
  -- | As mentioned in the introduction, /MoCs/ are classes of behaviors
  -- dictating the semantics of execution and concurrency in a network
  -- of processes. Based on the definitions of their tag systems
  -- ForSyDe identifies MoCs as: /timed/ where /T/ (see
  -- <#sig-definition signal definition>) is a totally ordered set and
  -- /t/ express the notion of time, also being called timestamps
  -- (e.g. continuous time, discrete event, synchronous, etc.); or
  -- /untimed/, where /T/ is a partially ordered set and /t/ express
  -- the notion of precedence (e.g. dataflow, etc.).
  --
  -- As concerning MoCs, ForSyDe implements the execution semantics
  -- /through process constructors/, abstracting the timing model and
  -- inferring a schedule of the process network. This means that all
  -- processing entities in ForSyDe embed operating semantics dictated
  -- by a certain MoC and are side-effect-free. This ensures the
  -- functional correctness of a system even from early design stages.
  --
  -- The mechanisms of implementing MoCs are briefly explained as part
  -- of the <#moc-layer the 3-layered process model>, whereas the
  -- actual MoCs implemented and their semantics are described in
  -- their respective library.


  -- * The layered process model

  -- | The @forsyde-atom@ project is characterized by two main features:
  --
  -- 1. it tries to separate the concerns of execution and
  -- synchronization in cyber-physical systems
  -- 1. it provides primitive (indivizible) operators called /atoms/
  -- as building blocks for independently developing complex aspects
  -- of a system's execution through means of composition or
  -- generalization.
  --
  -- [@atom@] the elementary (primitive, indivizible) constructor in a
  -- tagged signal model which embeds timing/behavioural semantics.
  --
  -- The separation of concerns led to the so-called /layered process model/
  -- which is reflected in the library implementation by the
  -- development of separate independent modules for each aspect of
  -- the execution. These layers have the following properties:
  --
  -- 1. are implemented as higher-order functions, where functions of
  -- layer /l/ takes functions of layer /l-1/ as arguments.
  --
  -- 1. each layer operates on a different part of an event and
  -- abstracts a different execution aspect.
  --
  -- 1. the lowest layer /l=1/ contains arbitrary functions on values
  -- /V/
  --
  -- 1. layers /l>1/ are instantiated using library-provided
  -- /constructors/ which are in fact specific compositions of
  -- /atoms/.
  --
  -- 1. constructors have meaningful semantics and /known/
  -- implementations on target platforms. In this sense they are both
  -- analyzable and synthesizable.
  --
  -- 1. complex behaviors can be obtain by means of arbitrary
  -- compositions of the provided constructors.
  --
  -- A depiction of the layers of a process can be seen in the picture
  -- below:
  --
  -- #3layer#
  -- <<includes/figs/3-layered-process.png>>

  -- ** The synchronization layer

  -- | The synchronization layer abstracts timing semantics as defined
  -- by a chosen MoC. It provides /process constructors/ as means of
  -- instantiating processes (see <#proc-definition the process definition>).
  -- This layer provides:
  --
  -- * 4 atoms as infix operators. To show their generality, these
  -- atoms are implemented as part of a type class 'MoC'. Since we
  -- have stated that each MoC is determined by its tag system, we
  -- define tag systems for the supported MoCs in form of event
  -- constructors (i.e. /T/ &#215; /V/) which are instances of the
  -- 'MoC' class. Therefore each MoC overloads the 4 atom operators to
  -- implement their specific timing semantics.
  --
  -- * a library of meaningful atom compositions as process
  -- constructors, extensively documented in the "ForSyDe.Core.MoC"
  -- module.

  -- *** Atoms
  
  MoC(..),

  -- *** Constructors

  -- | As mentioned, process constructors are simply meaningful
  -- compositions of synchronization atoms. since in the
  -- <#3layer 3-layered process model> this is represented as the
  -- outer layer, we can consider that by passing (applied)
  -- behavior-layer functions to them we obtain processes just like in
  -- the <#proc-definition process definition>.
  --
  -- Recall that by composing /n/ processes, we obtain a process
  -- network, where each composition infers a signal between two
  -- processes. This mechanism also works in the sense that by (fully)
  -- applying a constructor obtained from the composition of two other
  -- (atom) constructors, we obtain a process network equivalent to
  -- the composition of the respective primitive processes, like in
  -- the example below:
  --
  -- <<includes/figs/pn-example-constructor.png>>
  --
  -- Now if we visualize process networks as graphs, where processes
  -- are nodes and signals are edges, a meaningful process composition
  -- could be regarded as graph patterns. Therefore it is safe to
  -- associate process constructors as patterns in process networks.
  --
  -- [@process network patterns@] is another view of process
  -- constructors if we regard process networks as graphs and
  -- meaningful compositions of atoms as graph patterns.
  --
  -- For an extensive list with the provided process constructors,
  -- please consult the module "ForSyDe.MoCLib"

  -- *** The "unzip" utility
  --
  -- | Recall that in our <#proc-definition definition for processes>,
  -- the return type may be an /n/-arry cartesian product. A caveat of
  -- founding the ForSyDe framework on a functional language is that,
  -- although we can express cartesian products for input arguments
  -- using the curried notation (which is very powerful in combination
  -- with an applicative style), we cannot do so for return types. For
  -- the latter we must rely on tuples.
  --
  -- Working with tuples of data wrapped as @Event@ which is also
  -- wrapped in a 'Signal' structure becomes extremely
  -- cumbersome. Take for example the case of a process constructed
  -- with /pc/ in equation (1) below. Using only the provided atoms to
  -- implement /pc/ would give us a process which returns only one
  -- signal of a tuple and not, as we would like, a tuple of signals
  -- of events.
  --
  -- <<includes/figs/unzip-example.png>>
  --
  -- Therefore, by implementing the data types associated with signals
  -- and events as instances of 'Functor', we were able to provide a
  -- (set of) /unzip/ process(es) defined as in equation (2) above, as
  -- part of the "ForSyDe.Core.Utilities" module.  Mind that we call
  -- /unzip/ a utility process and not an atom, since it has no
  -- sinchronization nor behavior semantic. It just conveniently
  -- "lifts" the wrapped tuples in order to create "collections" of
  -- events and signals, and it is imposed by the mechanisms of the
  -- type system in the host language.

  
  -- ** Behavior layer

  -- | As seen in <#3layer layered process model>, the behavior layer
  -- abstracts the semantics implied by the extended values. In other
  -- words, the constructors of this layer dictate what action to be
  -- performed in case of different event types. This layer provides:
  -- 
  -- * a set of behavior atoms. Since we defined only one data type
  -- for extended values (the 'Value' type) they have been implemented
  -- as normal functions instead of class methods.
  --
  -- * a library of function wrappers as specific behavior atom
  -- compositions. These wrappers are meant to be passed to the the
  -- synchronziation layer constructors as arguments when implementing
  -- process constructors.

  -- *** Atoms
  
  (>$), (>*), (>!), (>?), (>%),

  -- *** Behavior wrappers
  
  -- | Wrappers are the behavior layer entities passed as arguments
  -- to the synchronization layer. They are implemented as specific
  -- compositions of behavior atoms.
  --
  -- For extended documentation on the available behavior wrappers
  -- please consult the module "ForSyDe.WrapLib"


  -- ** Function layer

  -- | As seen in the <#3layer layered process model> the function
  -- layer abstracts nothing. It exposes the actual transformations on
  -- the event values as specified by a system designer. Needless to
  -- say, they are taken as arguments (and wrapped) by the behavior
  -- layer wrappers.

  -- * Utilities

  -- | The 'ForSyDe.Core' module also provides a set of utility
  -- functions, mainly for aiding the designer to avoid working with
  -- event tuples which might polute the design. 
  --
  -- For a list of all the provided utilities, please consult the
  -- following module:

  module ForSyDe.Core.Utilities,
         
  -- * Bibliography

  -- | #lee98# [1] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /Computer-Aided Design of Integrated Circuits and Systems, IEEE Transactions on, 17(12)/, 1217-1229. 

  -- | #reekie95# [2] Reekie, H. J. (1995). Realtime signal processing: Dataflow, visual, and functional programming. 
       
) where

import ForSyDe.Core.Signal
import ForSyDe.Core.Utilities
import ForSyDe.Core.ValueExt
import ForSyDe.Core.MoC

