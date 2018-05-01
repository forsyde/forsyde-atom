{-# OPTIONS_HADDOCK not-home, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom
-- Copyright   :  (c) George Ungureanu, 2015-2018
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The "spiritual parent" of this modeling framework dates as back as
-- <#sander04 [Sander04]> which presented ForSyDe, an EDSL for
-- describing heterogeneous systems as /networks of processes/
-- communicating through /signals/. In ForSyDe, processes alone
-- capture the timing semantics of execution and synchronization
-- according to a certain /model of computation (MoC)/. The shallow
-- implementation of ForSyDe,
-- @<https://forsyde.github.io/forsyde-shallow/ forsyde-shallow>@
-- provides libraries of higher-order functions for instantiating
-- processes called /process constructors/ for several MoCs. The
-- formal foundation upon which ForSyDe defines its semantics is the
-- /tagged signal model/ <ForSyDe-Atom.html#lee98 [Lee98]>, which is
-- a denotational framework introduced by Lee and
-- Sangiovanni-Vincentelli as a common meta model for describing
-- properties of concurrent systems in terms of their behavior.
--
-- <<fig/misc-process-network.png>>
--
-- The @<https://forsyde.github.io/forsyde-atom/ forsyde-atom>@
-- project started as a proof-of-concept for the atom-based approach
-- to cyber-physical systems (CPS) introduced in
-- <ForSyDe-Atom.html#ungureanu17 [Ungureanu17]>. This approach
-- extends the ideas of the tagged signal model by systematically
-- deconstructing processes to their basic semantics and recreating
-- them using a minimal language of primitive building blocks called
-- /atoms/. It also expands the scope of this model by exploiting more
-- aspects of cyber-physical systems than just timing, but also adding
-- primitives for parallelism, behavior extensions, etc, each in its
-- own interacting environment called /layer/. You can read more about
-- layers in the <#g:1 corresponding section> further on.
--
-- The API documentation is structured as follows: this page provides
-- an overview of the general notions and concepts, introducing the
-- separate modules and the motivation behind them. Each major module
-- corresponds to a separate layer described by a type class, and
-- contains a number of sub-modules, mainly providing instances and
-- helpers for their respective layer. The documentation for each
-- module contains in-depth knowledge and examples. For more complex
-- examples and tutorials follow the links in the
-- <https://forsyde.github.io/forsyde-atom/ project web page>.
--
--  #naming_conv# __IMPORTANT!!!__ All multi-argument functions and
-- utilities provided by the @<https://forsyde.github.io/forsyde-atom/ forsyde-atom>@
-- API are named along the lines of @functionMN@ where @M@ represents
-- the number of __/curried/__ inputs (i.e. @a1 -> a2 -> ... -> aM@),
-- while @N@ represents the number of __/tupled/__ outputs
-- (i.e. @(b1,b2,...,bN)@). To avoid repetition we only provide
-- documentation for functions with 2 inputs and 2 outputs
-- (i.e. @function22@). In case the provided functions do not suffice,
-- feel free to implement your own patterns following the examples in
-- the source code.
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
  --   which embeds a set of semantics relevant for their respective
  --   layer (e.g. timing, behavioural, structural, etc.)
  --
  -- [@atom patterns@] meaningful compositions of atoms. They are
  --   provided as constructors which need to be properly instantiated
  --   in order to be used. We also use the term "pattern" to
  --   differentiate atom compositions as constructors from atoms as
  --   constructors.
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
  -- Layers are implemented as type classes which define:
  --
  -- * __atoms__ as function signatures belonging to the type class;
  --
  -- * __patterns__ which are compositions atoms, provided as
  --   (functional block) constructors, e.g. process constructors in
  --   the MoC layer;
  --
  -- * __data types__ for all the classes of behaviors concerning the
  --   aspect described by the layer in question. These types
  --   instantiate their respective type class and overload the atoms
  --   with semantics in accordance to the behavior described. For
  --   example, the 'ForSyDe.Atom.MoC.MoC' layer is currently
  --   instantiated by types describing the 'ForSyDe.Atom.Moc.CT.CT',
  --   'ForSyDe.Atom.Moc.DE.DE', 'ForSyDe.Atom.Moc.SY.SY' and
  --   'ForSyDe.Atom.Moc.SDF.SDF' MoCs.
  --
  -- In order to model interleaving aspects of CPS, layers interact
  -- with each other through means of higher order functions. As such,
  -- each layer describes some atoms as higher-order functions which
  -- take entities belonging to another layer as arguments.
  -- Intrinsically, the data types belonging to a layer may be
  -- wrapping types of other layers, as depicted in <#layered-model the figure above>.
  -- For a short comprehensive overview on layers, please refer to
  -- <ForSyDe-Atom.html#ungureanu17 [Ungureanu17]>.
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
  -- symbols with /known/ semantics, and adding it to the pool of
  -- possible values or states.
  --
  -- While semantically the 'ExB' layer extends the value pool in
  -- order to express special events (e.g. protocol, error messages or
  -- even the complete absence of events), it practically provides an
  -- independent environment to model events with a default/known
  -- response, independently of the data path. These responses are
  -- particularly captured by ExB atoms, thus enforcing the high-level
  -- separation of concerns between e.g. control and data paths.
  --
  -- This layer provides:
  -- 
  -- * a set of extended behavior atoms defining the interfaces for
  -- the resolution and response functions, as part of the 'ExB' type
  -- class /(see below)/.
  --
  -- * a library of function wrappers as specific atom patterns
  --   (/check the "ForSyDe.Atom.ExB" module/).
  --
  -- * a set of data types defining classes of behaviors and
  --   instantiating the 'ExB' type class (/check the/
  --   /<#i:ExB instances>/).
    
  ExB(..),
  
  -- * The Model of Computation (MoC) Layer

  -- | This layer represents a major part of the @forsyde-atom@
  -- library and is concerned in modeling the timing aspects of
  -- CPS. While its foundations have been laid in the classical
  -- ForSyDe <#sander04 [Sander04]>, it mainly tries to follow the
  -- tagged signal model <#lee98 [Lee98]> as closely as it is
  -- permitted by the host language, and with the adaptations required
  -- by the atom approach.
  --
  -- Continuing the short description in <#package-header the
  -- introduction>, the following sections offer a short primer in
  -- ForSyDe-Atom and how it connects to the classical ForSyDe theory,
  -- culminating with an overview of the MoC layer as provided by
  -- @forsyde-atom@.

  -- ** Signals
  
  -- | <#lee98 [Lee98]> defines signals as ordered sets of events
  -- where each event is composed of a tag /T/ and a value /V/, where
  -- /T/ defines a total or partial order. Similarly, in ForSyDe a
  -- signal is defined as a /sequence/ of events that enables
  -- processes to communicate and synchronize. Sequencing might infer
  -- an implicit total order of events, but more importantly it
  -- determines an order of evaluation, which is a key piece of the
  -- simulation engine.
  --
  -- <<fig/misc-tagged-signal.png>>
  --
  -- In ForSyDe, sequencing is achieved using a 'Stream' data type,
  -- inspired from <#reekie95 [Reekie95]>. In ForSyDe-Atom, signals
  -- are streams that carry /events/, where each type of event is
  -- identified by a type constructor. Hence the pseudo-Haskell
  -- definition for a signal would look like below, where @e@ is the
  -- type of an event which encodes a tag system through its type
  -- constructor. Since, according to <#lee98 [Lee98]>, MoCs are
  -- defined by tag systems, we can state that any specific instance
  -- of a signal is describing (i.e. is bound to) a MoC.
  --
  -- > type Signal a = exists e . MoC e => Stream (e a) 

  Stream (..),

  -- | For extended documentation and a list of all utilities associated
  -- with the 'Stream' type you can consult:

  module ForSyDe.Atom.MoC.Stream,
  
  -- ** Processes
  
  -- | As described in <#lee98 [Lee98]>, processes are either "set of
  -- possible behaviors" of signals or "relations" between multiple
  -- signals. One can describe complex systems by composing processes,
  -- which in this case is interpreted as the "intersection of the
  -- behaviors of each of the processes being involved". According to
  -- that framework, we can describe ForSyDe-Atom processes as having
  -- the following properties:
  --
  -- [functionality] Processes are functional if provided they are
  --   stimulated with the /same/ input signals, they react with the
  --   /same/ output signals every time. This condition is fulfilled
  --   by Haskell's purity. On the other hand in the current modeling
  --   framework it is not possible to model non-functional
  --   processes.
  --
  -- [determinancy] A functional process is determinate, which means
  --   that they describe either /one/ behavior, or unambiguously no
  --   behavior.
  --
  -- [strict causality] Refers to the fact that the distance between
  --   input signals of a process is strictly preserved at the
  --   output. This means that ForSyDe-Atom process networks are
  --   unable to describe and simulate delta-causal or non-causal
  --   systems (i.e. zero-delay feedback loops) in the MoC
  --   layer. These problems are solved in other layers (see
  --   <ForSyDe-Atom.html#ungureanu18 [Ungureanu18]>)
  --
  -- [monotonicity] In order to ensure causal order and determinancy,
  --   processes need to be /monotonic/. A signal's tags (if explicit)
  --   /must be/ a partial or total order and all tag alterations must
  --   be monotonic.
  --
  -- ForSyDe inherits this definition with respect to a functional
  -- view, thus a __process__ /p/ is a functional mapping over (the
  -- history of) signals. A process can /only/ be instantiated using a
  -- __process constructor__ /pc/, which is a higher order function
  -- embedding MoC semantics and/or a specific composition, but
  -- lacking functionality. An intuitive way of viewing process
  -- constructors in general is as "parameterized templates" for
  -- processes. Stretching this analogy we take the liberty of
  -- referring to processes as "instances of process constructors".
  --
  -- #proc-definition#
  -- <<fig/misc-process.png>>
  --
  -- <<fig/misc-process-template.png>>
  --
  -- In the definitions/figure above, \(v_i \in V\) may also denote
  -- the set of functions, not only values, fact which is taken for
  -- granted in Haskell. Actually the whole power of ForSyDe as a
  -- Haskell EDSL is that the designer can send functions as normal
  -- values in programs. 
  --
  -- Since processes are functions, process composition is equivalent
  -- to function composition. This means that composing two processes
  -- @p1@ and @p2@ grants the process @p2 . p1@
  --
  -- > p1      :: Signal α -> Signal β
  -- > p2      :: Signal β -> Signal γ
  -- > p2 . p1 :: Signal α -> Signal γ
  --
  --  This implies that there is a signal @Signal β@ that "streams"
  --  the result from @p1@ to @p2@, as suggested in the drawing:
  --
  -- <<fig/misc-ser-composition.png>>
  --
  -- __Process networks__ describe ForSyDe systems in terms of
  -- compositions of processes and originate from Reekie's process
  -- nets <#reekie95 [Reekie95]>. A process network is a process itself,
  -- i.e. function from signal(s) to signal(s). The composition above
  -- @p2 . p1 @ can also be regarded as a process network.
  --
  -- In ForSyDe-Atom, MoC atoms are themselves process constructors,
  -- and follow the process constructor definition: they (may) take
  -- values as arguments and return processes. Based on that we can
  -- create the following equivalence table:
  --
  -- <<fig/misc-atom-forsyde-equiv.png>>
  --
  -- These equivalences fit well with intuition when considering how
  -- partial application and composition mechanisms are acting. The
  -- whole idea of atoms is basically applying Backus' principle of
  -- combining forms in order to obtain abstractions, which is one of
  -- the basic principles of the functional programming paradigm. To
  -- further even more this idea, one could visualize process networks
  -- as graphs, where processes are nodes and signals are edges. A
  -- subgraph with a meaningful shape could be considered a /pattern/
  -- (hence the name atom patterns), and one could find isomorphisms
  -- between different patterns.
  --
  -- To get a flavor of how (partial) composition of higher order
  -- function works in ForSyDe-Atom, consider the example below where
  -- both /pn/ and /p'/ denote the same thing and instantiate the
  -- pattern in the right hand side, visualized as a graph. The
  -- circled operators denote the '-.-' and '-*-' atoms presented in
  -- the next section.
  --
  -- <<fig/misc-process-constructor.png>>
  --
  
  -- ** Models of Computation
  
  -- | MoCs are classes of behaviors dictating the semantics of
  -- execution and concurrency in a network of processes and,
  -- according to <#lee98 [Lee98]>, are determined by the tag
  -- systems. Based on how their tag systems are defined ForSyDe
  -- identifies MoCs as:
  --
  -- 1. /timed/ where /T/ is a totally ordered set and, depending on
  --   the abstraction level, /t/ might express from the notion of
  --   physical time (e.g. continuous time 'ForSyDe.Atom.MoC.CT.CT',
  --   discrete event 'ForSyDe.Atom.MoC.DE.DE') to the notion of
  --   precedence and causality (e.g. synchronous
  --   'ForSyDe.Atom.MoC.SY.SY');
  --
  -- 1. /untimed/, where /T/ is a partially ordered set and /t/ is
  --   expressed in terms of constraints on the tags in signals
  --   (e.g. dataflow, synchronous data flow
  --   'ForSyDe.Atom.MoC.SDF.SDF').
  --
  -- As concerning MoCs, ForSyDe implements the execution semantics
  -- through process constructors, abstracting the timing model and
  -- inferring a schedule of the process network. Furthermore,
  -- processes are side-effect-free, ensuring the functional
  -- correctness of a system even from early design stages. In
  -- ForSyDe-Atom all instances of MoC atoms embed operating semantics
  -- dictated by a certain MoC.
  
  -- ** Representing Time
  
  -- | Some MoCs require explicit time representation, for which
  -- @forsyde-atom@ provides two distinct data types.

  Time(..), TimeStamp(..),

  -- ** MoC Layer Overview
  
  -- | Now, after the crash course in ForSyDe, we can finally present
  -- what the MoC layer consists of. Similarly to all other layers, the
  -- MoC layer defines:
  --
  -- * 4 atoms provided as infix operators and implemented as type
  --   class methods for the class 'MoC'. Since each atom's semantics
  --   is overloaded based on the input type, and each input type
  --   defines an own tag system, we enforce Lee's idea <#lee98
  --   [Lee98]> that tag systems determine the MoC (semantics). (/See/
  --   /below/)
  --
  -- * a library of meaningful atom patterns as process constructors.
  --   (/Check the "ForSyDe.Atom.MoC" module/).
  --
  -- * a set of data types representing events defining tag systems
  --   through their structure (i.e. /T/ &#215; /V/). These types are
  --   instances of the 'MoC' type class and are defining the rules of
  --   execution which will trigger an atom to behave in accordance to
  --   an associated MoC. For each supported MoC, @forsyde-atom@
  --   provides a module which defines the signal type, but also a
  --   library of utilities and helpers for specific instantiations of
  --   atom patterns. (/Check the <#i:MoC instances>/).
  
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
  --   and <#skillicorn05 [Skillicorn05]>, are enough to describe
  --   /all/ algorithmic skeletons.
  --
  -- * a library of generic skeletons as specific atom patterns.
  --   (/Check the "ForSyDe.Atom.Skeleton" module/).
  --
  -- * a set of different categorical types which implement these
  --   atoms, as instances of the 'Skeleton' type class. These types
  --   provide additional skeletons patterns of atoms which takes as
  --   arguments their own type constructors.  (/Check the/
  --   /<#i:Skeleton instances>/).
  
  Skeleton(..),

  -- * Utilities

  -- | The "ForSyDe.Atom" module export a set of utility functions,
  -- mainly for helping to write more concise code or to perform
  -- regular chores like plotting, or nicer printouts.
  --
  -- The "ForSyDe.Atom.Utility.Plot" module contains utilities for
  -- dumping and visualizing the data in ForSyDe-Atom models in
  -- different formats, e.g. <https://forsyde.github.io/forsyde-latex/ LaTeX>
  -- or <www.gnuplot.info gnuplot>.

  module ForSyDe.Atom.Utility.Plot,
  
  -- | The "ForSyDe.Atom.Utility.Tuple" library contains functions
  -- that help in avoiding working with explicitly zipping and
  -- unzipping structures of /n/-tuples. These utilities work mainly
  -- on the structures of data without altering the information, thus
  -- they are not considered atoms and are not associated with any
  -- layer.

  module ForSyDe.Atom.Utility.Tuple,
  
  -- | Among the most useful of these utilities we mentions the
  -- @unzip@ function. When consulting the ForSyDe-Atom papers or
  -- further checking the modules' API documentation, you will notice
  -- that in all our formal definitions, patterns are expressed in the
  -- most general form as functions from /n/-ary Cartesian products to
  -- /m/-ary Cartesian products. While partial application provides a
  -- versatile mechanism which translates n-ary inputs into curried
  -- arguments (which is a powerful mechanism in combination with an
  -- applicative style of programming), for the return types we must
  -- rely on tuples. But working with tuples of data wrapped in
  -- several layers of structures becomes extremely cumbersome. Take
  -- for example the case of a process constructed with /pc/ in
  -- equation (1) below. Using only the atoms defined by
  -- 'ForSyDe.Atom.MoC.MoC' to implement /pc/ we would be able to
  -- design a process which returns only one signal carrying tuples
  -- and not, as we would like, a tuple of signals.
  --
  -- <<fig/misc-unzip.png>>
  --
  -- Therefore, by implementing all data types associated with signals
  -- and events as instances of 'Functor', we were able to provide a
  -- (set of) /unzip/ utility functions defined like in equation (2)
  -- above.  Mind that we call /unzip/ a utility and not an atom,
  -- since it has no behavioral semantic. It just conveniently
  -- un-wraps tuples and lifts them into layers above.

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
  
  -- | #ungureanu18# <https://ieeexplore.ieee.org/document/8342019/ [Ungureanu18]> Ungureanu, G., de Medeiros, J. E. G. & Sander, I., /Bridging discrete and continuous time models with Atoms/, in 2018 Design, Automation & Test in Europe Conference & Exhibition (DATE), 2018, pp. 277-280
  
) where

import ForSyDe.Atom.ExB
import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Skeleton
import ForSyDe.Atom.Utility.Tuple
import ForSyDe.Atom.Utility.Plot
