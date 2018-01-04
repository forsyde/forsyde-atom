{-# OPTIONS_HADDOCK not-home, prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2016;
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The formal foundation upon which ForSyDe <#sander04 [Sander04]>
-- defines its semantics is the /tagged signal model/
-- <ForSyDe-Atom.html#lee98 [Lee98]>.  This is a denotational
-- framework introduced by Lee and Sangiovanni-Vincentelli as a common
-- meta model for describing properties of concurrent systems in
-- general terms as sets of possible behaviors. Systems are regarded
-- as /compositions/ of /processes/ acting on /signals/ which are sets
-- of /tagged events/. Signals are characterized by a /tag system/
-- which determines causality between events, and could model time,
-- precedence relationships, synchronization points, and other key
-- properties. Based on how tag systems are defined, one can identify
-- several /Models of Computations (MoCs)/ as classes of behaviors
-- dictating the semantics of execution and concurrency in a network
-- of processes.
--
-- These concepts are the supporting pillars of ForSyDe's philosophy,
-- and state the purpose of the @forsyde-atom@ library: it is supposed
-- to be a modelling framework used as a proof-of-concept for the
-- atom-based approach to cyber-physical systems
-- <ForSyDe-Atom.html#ungureanu17 [Ungureanu17]>. This approach
-- extends the tagged signal model by systematically deconstructing
-- processes to their basic semantics and recreating them using a
-- minimal language of primitive building blocks called /atoms/. It
-- also tries to expand the scope of this model by exploiting more
-- aspects than just timing, by adding primitives for parallelism,
-- behavior, etc.
--
-- The API documentation is structured as follows: this page provides
-- an overview of the general notions and concepts, gently introducing
-- the separate modules and the motivation behind them. Each major
-- module corresponds to a separate /layer/
-- <ForSyDe-Atom.html#ungureanu17 [Ungureanu17]> implemented as a type
-- class. The documentation pages for each layer and for each of their
-- instances contains in-depth knowledge and examples, and can be
-- accessed from the contents page or by following the links
-- suggested. For more complex examples and tutorials follow the links
-- in the <https://github.com/forsyde/forsyde-atom project web page>.
--
--  #naming_conv# __IMPORTANT!!!__ All multi-parameter patterns and
-- utilities provided by the library API as higher-order functions are
-- named along the lines of @functionMN@ where @M@ represents the
-- number of __/curried/__ inputs (i.e. @a1 -> a2 -> ... -> aM@),
-- while @N@ represents the number of __/tupled/__ outputs
-- (i.e. @(b1,b2,...,bN)@). To avoid repetition we only provide
-- documentation for functions with 2 inputs and 2 outputs
-- (i.e. @function22@), while the available ones are mentioned as a
-- regex (i.e. @function[1-4][1-4]@). In case the provided functions
-- do not suffice, feel free to implement your own patterns following
-- the examples in the source code.
-----------------------------------------------------------------------------
module ForSyDe.Atom (
  
  -- * The layered process model

  -- | The @forsyde-atom@ project is led by three main policies:
  --
  -- 1. in order to cope with the complexity of cyber-physical systems
  -- (CPS) it tries to separate the concerns such as computation,
  -- timing, synchronization, parallelism, structure, behavior, etc.
  --
  -- 1. in order to have a small, ideally minimal grammar to reason
  -- about systems correctness, it aims to provide primitive
  -- (indivisible) operators called /atoms/ as building blocks for
  -- independently developing complex aspects of a system's execution
  -- through means of composition or generalization.
  --
  -- 1. in order to express complex behaviors with a minimal grammar,
  -- it decouples structure (composition) from meaning (semantics),
  -- the only semantics carriers being atoms. Thus complex behaviors
  -- can be described in terms of /patterns of atoms/. Using ad-hoc
  -- polymorphism, atoms can be overloaded with different semantics
  -- triggered by the data type they input, whereas their composition
  -- is always the same.
  --
  -- [@atom@] the elementary (primitive, indivisible) constructor
  -- which embeds a set of semantics relevant for their respective
  -- layer (e.g. timing, behavioural, structural, etc.)
  --
  -- [@atom patterns@] meaningful compositions of atoms. They are
  -- provided as constructors which need to be properly instantiated
  -- in order to be used. We also use the term "pattern" to
  -- differentiate atom compositions as constructors from atoms as
  -- constructors.
  --
  -- The first policy, i.e. the separation of concerns led to the
  -- so-called /layered process model/ which is reflected in the
  -- library by providing separate major modules associated with each
  -- layer. Layers as such are independent collections of entities for
  -- modeling different aspects of CPS. These aspects interact through
  -- means of higher-order functions, wrapping each other in as
  -- structured fashion in a way which can be visualized as below.
  --
  -- #layered-model#
  -- <<fig/misc-layered-model.png>>
  --
  -- Layers are implemented as type classes which imply:
  --
  -- * __atoms__ as function signatures belonging to the type class;
  --
  -- * __patterns__ which are compositions atoms, provided as constructors;
  --
  -- * __data types__ for all the classes of behaviors concerning the
  -- aspect described by the layer in question. These types
  -- instantiate the above type class and overload the atoms with
  -- semantics in accordance to the behavior described. For example,
  -- the 'ForSyDe.Atom.MoC.MoC' layer is currently instantiated by
  -- types describing the 'ForSyDe.Atom.Moc.CT.CT',
  -- 'ForSyDe.Atom.Moc.DE.DE', 'ForSyDe.Atom.Moc.SY.SY' and
  -- 'ForSyDe.Atom.Moc.SDF.SDF' MoCs.
  --
  -- In order to model interleaving aspects of CPS, layers interact
  -- with each other through means of higher order functions. As such,
  -- each layer describes some atoms as higher-order functions which
  -- take entities belonging to another layer as arguments.
  -- Intrinsically, the data types belonging to a layer may be wrapping
  -- types of other layers, as depicted in the <#layered-model figure>
  -- above. For a short comprehensive overview on layers, please refer
  -- to <ForSyDe-Atom.html#ungureanu17 [Ungureanu17]>.
  --
  -- By convention, the first (innermost) layer is always the
  -- /function layer/ which describes arbitrary functions on data and
  -- expresses the system's functional aspects. In the following
  -- paragraphs we will give an overview of the \"outer\" layers
  -- currently implemented in @forsyde-atom@, which in comparison,
  -- express the extra-functional aspects of a system (timing,
  -- behavior, synchronization, and so on).

  -- * The Extended Behavior (ExB) Layer

  -- | As seen in <#layered-model layered process model>, the extended
  -- behavior layer expands the set of possible behaviors implied by a
  -- layer (typically the function layer), by defining a set of
  -- symbols with /known/ semantics, and adding it to (i.e. wrapping)
  -- the pool of possible values or states.
  --
  -- While semantically the 'ExB' layer extends the value pool in
  -- order to express special events (e.g. error messages or even the
  -- complete absence of events), it practically provides an
  -- independent environment to model events with a default/known
  -- response, independently of the data path. These responses are
  -- particularly captured by atoms, thus enforcing the high-level
  -- separation of concerns between e.g. control and data paths.
  --
  -- This layer provides:
  -- 
  -- * a set of extended behavior atoms defining the interfaces for
  -- the resolution and response functions, as part of the 'ExB' type
  -- class /(see below)/.
  --
  -- * a library of function wrappers as specific atom patterns
  -- (/Check the "ForSyDe.Atom.ExB" module for extensive/
  -- /documentation/).
  --
  -- * a set of data types defining classes of behaviors and
  -- instantiating the 'ExB' type class (/check the links in the/
  -- /<#section.i:ExB instances> section for extensive documentation/).
    
  ExB(..),
  
  -- * The Model of Computation (MoC) Layer

  -- | This layer represents a major part of the @forsyde-atom@
  -- library and is concerned in modeling the timing aspects of
  -- CPS. While its foundations have been layered in the classical
  -- ForSyDe <#sander04 [Sander04]>, it is mainly inspired from
  -- <#lee98 [Lee98]> an it tries to follow the tagged signal model as
  -- closely as it is permitted by the host language, and with the
  -- adaptations require by the atom approach.
  --
  -- Although a short introduction of the tagged signal model has been
  -- written in the introduction of this documentation, we feel
  -- obliged to provide a primer in the classical ForSyDe theory in
  -- order to understand how everything fits together.

  -- ** Signals
  
  -- | <#lee98 [Lee98]> defines signals as (ordered) sets of events
  -- where each event is composed of a tag /T/ and a value
  -- /V/. Similarly, in ForSyDe a signal is defined as a (partially or
  -- totally) /ordered sequence/ of events that enables processes to
  -- communicate and synchronize. Sequencing might infer an implicit
  -- order of events, but more importantly it determines an order of
  -- evaluation, which is a key piece of a simulation engine.
  --
  -- <<fig/misc-tagged-signal.png>>
  --
  -- In ForSyDe-Atom, sequencing is achieved using the 'Stream' data
  -- type, inspired from <#reekie95 [Reekie95]>. In ForSyDe-Atom,
  -- signals are streams that carry /events/, where each type of event
  -- is identified by a type constructor which defines its tag
  -- system. In other words, we can state that through its tag system,
  -- a signal is /bound/ to a MoC.

  Stream (..),

  -- | For extended documentation and a list of all utilities associated
  -- with the 'Stream' type you can consult:

  module ForSyDe.Atom.MoC.Stream,
  
  -- ** Processes
  
  -- | As described in <#lee98 [Lee98]>, processes are either "set of
  -- possible behaviors" of signals or "relations" between multiple
  -- signals. One can describe complex systems by composing processes,
  -- which in this case is interpreted as the "intersection of the
  -- behaviors of each of the processes being involved".
  --
  -- [monotonicity] In order to ensure causal order and determinancy,
  -- processes need to be /monotonic/ <#lee98 [Lee98]>. A signal's
  -- tags (if explicit) /must be/ a partial or total order and all tag
  -- alterations must be monotonic.
  --
  -- ForSyDe inherits this definition with respect to a functional
  -- view, thus a __process__ /p/ is a functional mapping over (the
  -- history of) signals. A process can /only/ be instantiated using a
  -- __process constructor__ /pc/, which is a higher order function
  -- embedding MoC semantics and/or a specific composition, but
  -- lacking functionality.
  --
  -- #proc-definition#
  -- <<fig/misc-process.png>>
  --
  -- Since processes are functions, process composition is equivalent
  -- to function composition. This means that composing two processes
  -- @p1@ and @p2@ grants the process @p2 . p1@
  --
  -- > p1      :: Signal α -> Signal β
  -- > p2      :: Signal β -> Signal γ
  -- > p2 . p1 :: Signal α -> Signal γ
  --
  --  This implies that there is a signal @Signal beta @ that
  --  "streams" the result from @p1@ to @p2@, as suggested in the
  --  drawing:
  --
  -- <<fig/misc-ser-composition.png>>
  --
  -- __Process networks__ describe ForSyDe systems in terms of
  -- compositions of processes and originate from Reekie's process
  -- nets <#reekie95 [Reekie95]>. A process network is a process itself,
  -- i.e. function from signal(s) to signal(s). The composition above
  -- @p2 . p1 @ can also be regarded as a process network.
  --
  -- In ForSyDe-Atom atoms can be regarded as process constructors as
  -- their instantiations are functions on signals of events.
  -- Instantiations of atom patterns are the exact equivalent of
  -- process networks, which themselves are also processes, depending
  -- on the level of abstraction you are working with (hierarchical
  -- blocks vs. flat structures).
  --
  -- To understand the versatility of composition and partial
  -- application in building process constructors, consider the
  -- example above where composition of two processes infers a signal
  -- between them. This mechanism also works when composing
  -- constructors (un-instantiated atoms), which yields another
  -- constructor. By instantiating (fully applying) the new
  -- constructor we obtain a process network equivalent to the
  -- composition of the respective primitive processes obtained by
  -- instantiating (fully applying) the component atoms, like in the
  -- example below:
  --
  -- <<fig/misc-process-constructor.png>>
  --
  -- Now if we visualize process networks as graphs, where processes
  -- are nodes and signals are edges, a meaningful process composition
  -- could be regarded as graph patterns. Therefore it is safe to
  -- associate process constructors as patterns in process networks.

  -- ** Models of Computation
  
  -- | As mentioned in the introduction, /MoCs/ are classes of behaviors
  -- dictating the semantics of execution and concurrency in a network
  -- of processes. Based on the definitions of their tag systems
  -- ForSyDe identifies MoCs as:
  --
  -- 1. /timed/ where /T/ is a totally ordered set and /t/ express the
  -- notion of physical time (e.g. continuous time
  -- 'ForSyDe.Atom.MoC.CT.CT', discrete event
  -- 'ForSyDe.Atom.MoC.DE.DE') or precedence (e.g. synchronous
  -- 'ForSyDe.Atom.MoC.SY.SY');
  --
  -- 1. /untimed/, where /T/ is a partially ordered set and /t/ is
  -- expressed in terms of constraints on the tags in signals
  -- (e.g. dataflow, synchronous data flow
  -- 'ForSyDe.Atom.MoC.SDF.SDF').
  --
  -- As concerning MoCs, ForSyDe implements the execution semantics
  -- /through process constructors/, abstracting the timing model and
  -- inferring a schedule of the process network. In ForSyDe-Atom all
  -- atoms embed operating semantics dictated by a certain MoC and are
  -- side-effect-free. This ensures the functional correctness of a
  -- system even from early design stages.

  -- ** Representing Time
  
  -- | For explicit time representation, ForSyDe-atom provides two
  -- distinct data types.

  Time(..), TimeStamp(..),

  -- ** MoC Layer Overview

  -- | This layer consists of:
  --
  -- * 4 atoms as infix operators, implemented as methods of the type
  -- class 'MoC'. Since each MoC is determined by its tag system, we
  -- expose this 
  -- which are instances of this class. Thus an event's type will
  -- trigger an atom to behave in accordance to its associated MoC.
  --
  -- * a library of meaningful atom patterns as process constructors.
  -- (/Check the "ForSyDe.Atom.MoC" module for extensive/
  -- /documentation/).
  --
  -- * a set of data types defining tag systems through the structure
  -- of events (i.e. /T/ &#215; /V/). They are instances of the 'MoC'
  -- type class and define the rules of execution that will trigger an
  -- atom to behave in accordance to its associated MoC. For each
  -- supported MoC, @forsyde-atom@ provides a module which defines the
  -- signal (event) type, but also a set of utilities and process
  -- constructors as specific instantiations of atom patterns.
  -- (/Check the links in the <#section.i:MoC instances> section for/
  -- /extensive documentation/).
  
  MoC(..),  

  -- * The Skeleton Layer

  -- | The skeleton layer describes recursive and regular composition
  -- of processes which expose inherent potential for parallelism. As
  -- such, it wraps lower layer entities (i.e. processes, signals),
  -- into regular structures called /categorical types/. Most of the
  -- ground work for this layer is based on the categorical type
  -- theory <#bird97 [Bird97]>, which enable the description of
  -- algorithmic skeletons as high-level constructs encapsulating
  -- parallelism and communication with an associated cost complexity.
  --
  -- This layer provides:
  --
  -- * 3 atoms as infix operators which, as demonstrated in <#bird97 [Bird97]>
  -- and <#skillicorn05 [Skillicorn05]>, are enough to describe /all/
  -- algorithmic skeletons.
  --
  -- * a library of generic skeletons as specific atom patterns.
  -- (/Check the "ForSyDe.Atom.Skeleton" module for extensive/
  -- /documentation/).
  --
  -- * a set of different categorical types which implement these
  -- atoms, as instances of the 'Skeleton' type class. These types
  -- provide additional skeletons patterns of atoms which takes as
  -- arguments their own type constructors.
  -- (/Check the links in the <#section.i:Skeleton instances> section for/
  -- /extensive documentation/).
  
  Skeleton(..),

  -- * Utilities

  -- | The 'ForSyDe.Atom' module exports a set of utility functions,
  -- mainly for aiding the designer to avoid working with zipped
  -- tuples which might pollute the design. Utilities are function
  -- without any semantical value (thus not considered atoms). They
  -- operate on and might alter the /structure/ of some datum, but it
  -- does not affect its state.
  --
  -- For a list of all the provided utilities, please consult the
  -- following module:

  module ForSyDe.Atom.Utility,

  -- | Among the most useful utilities we mentions the @unzip@
  -- function. Recall that in all our definitions for patterns, they
  -- were expressed in the most general form as functions from /n/-ary
  -- Cartesian products to /m/-ary Cartesian products. While partial
  -- application provides a versatile mechanism that can translate
  -- n-ary inputs into curried arguments (which is very powerful in
  -- combination with an applicative style), we cannot do so for
  -- return types. For the latter we must rely on tuples. But working
  -- with tuples of data wrapped in several layers of structures
  -- becomes extremely cumbersome. Take for example the case of a
  -- process constructed with /pc/ in equation (1) below. Using only
  -- the provided atoms to implement /pc/ would give us a process
  -- which returns only one signal of a tuple and not, as we would
  -- like, a tuple of signals of events.
  --
  -- <<fig/misc-unzip.png>>
  --
  -- Therefore, by implementing all data types associated with signals
  -- and events as instances of 'Functor', we were able to provide a
  -- (set of) /unzip/ utility functions defined as in equation (2)
  -- above, in the "ForSyDe.Atom.Utility" module.  Mind that we call
  -- /unzip/ a utility and not an atom, since it has no
  -- synchronization nor behavior semantic. It just conveniently
  -- "lifts" the wrapped tuples in order to create "collections" of
  -- events and signals, and it is imposed by the mechanisms of the
  -- type system in the host language.

  (||<),
  
  -- * Bibliography

  -- | #bird87# <https://www.cs.ox.ac.uk/files/3378/PRG56.pdf [Bird87]> Bird, R. S. (1987). An introduction to the theory of lists. In /Logic of programming and calculi of discrete design/ (pp. 5-42). Springer Berlin Heidelberg.

  -- | #bird97# <http://dl.acm.org/citation.cfm?id=248932 [Bird97]> Bird, R. S. & de Moor, O. (1997). Algebra of Programming. Prentice-Hall, Inc., Upper Saddle River, NJ, USA. 

  -- | #fuji00# [Fujimoto00] Fujimoto, R. M. (2000). Parallel and distributed simulation systems (Vol. 300). New York: Wiley.
  
  -- | #halbwachs91# <http://ieeexplore.ieee.org/document/97300/ [Halbwachs91]> Halbwachs, N., Caspi, P., Raymond, P., & Pilaud, D. (1991). The synchronous data flow programming language LUSTRE. /Proceedings of the IEEE, 79(9)/, 1305-1320.

  -- | #gorlatch03# <http://link.springer.com/chapter/10.1007/978-1-4471-0097-3_1#page-1 [Gorlatch03]> Fischer, J., Gorlatch, S., & Bischof, H. (2003). Foundations of data-parallel skeletons. In /Patterns and skeletons for parallel and distributed computing/ (pp. 1-27). Springer London.
  
  -- | #kahn76# [Kahn76] Kahn, G., & MacQueen, D. (1976). Coroutines and networks of parallel processes.

  -- | #lee98# [Lee98] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /IEEE Transactions on Computer-Aided Design of Integrated Circuits and Systems, 17(12)/, 1217-1229. 

  -- | #reekie95# <http://ptolemy.eecs.berkeley.edu/~johnr/papers/pdf/thesis.pdf [Reekie95]> Reekie, H. J. (1995). Realtime signal processing: Dataflow, visual, and functional programming.
  
  -- | #sander04# <http://people.kth.se/~ingo/Papers/TCAD2004_SystemModeling.pdf [Sander04]> Sander, I., & Jantsch, A. (2004). System modeling and transformational design refinement in ForSyDe [formal system design]. /IEEE Transactions on Computer-Aided Design of Integrated Circuits and Systems, 23(1)/, 17-32.
    
  -- | #skillicorn05# <https://books.google.se/books?hl=ro&lr=&id=rQwsL5xsZigC&oi=fnd&pg=PP1&dq=skillicorn+foundation+parallel+programming&ots=UJMBr0uO2Q&sig=ncyXxE0gFNkUZwVOYyFb_ezWlGY&redir_esc=y#v=onepage&q=skillicorn%20foundation%20parallel%20programming&f=false [Skillicorn05]> Skillicorn, D. B. (2005). Foundations of parallel programming (No. 6). Cambridge University Press.

  -- | #ungureanu17# <http://ieeexplore.ieee.org/document/7927270/ [Ungureanu17]> Ungureanu, G., & Sander, I., /A layered formal framework for modeling of cyber-physical systems/, in 2017 Design, Automation & Test in Europe Conference & Exhibition (DATE), 2017, pp. 1715–1720.
  
) where

import ForSyDe.Atom.ExB
import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Skeleton
import ForSyDe.Atom.Utility

