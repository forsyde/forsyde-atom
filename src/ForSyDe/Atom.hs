{-# OPTIONS_HADDOCK not-home, prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom
-- Copyright   :  (c) George Ungureanu, 2015-2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- "ForSyDe.Atom" is a formal framework for modeling and simulating heterogeneous
-- embedded and cyber-physical systems (CPS) at a high level of abstraction, whose
-- "spiritual parent" is the <https://forsyde.github.io/forsyde-shallow/ ForSyDe>
-- modeling framework <#sander04 [Sander04]>. In ForSyDe, heterogeneous systems are
-- modeled as /networks of processes/ communicating through /signals/. In ForSyDe,
-- processes alone capture the timing semantics of execution and synchronization
-- according to a certain /model of computation (MoC)/. The shallow implementation of
-- ForSyDe, @<https://forsyde.github.io/forsyde-shallow/ forsyde-shallow>@ provides
-- libraries of higher-order functions for instantiating processes called
-- /process constructors/ for several MoCs. 
--
-- The @<https://forsyde.github.io/forsyde-atom/ forsyde-atom>@ project started as a
-- proof-of-concept for the atom-based approach to CPS introduced in
-- <ForSyDe-Atom.html#ungureanu17 [Ungureanu17]>. This approach extends the ideas of
-- the tagged signal model by systematically deconstructing processes to their basic
-- semantics and recreating them using a minimal language of primitive building blocks
-- called /atoms/. It also expands the scope of this model by exploiting more aspects
-- of cyber-physical systems than just timing, but also adding primitives for
-- parallelism, behavior extensions, probabilistic distribution, etc., each in its own
-- interacting environment called /layer/.
-----------------------------------------------------------------------------
module ForSyDe.Atom (

  -- * Prerequisites
  
  -- | The API documentation is meant to be a comprehensive reference guide for an
  -- interested user, with enough details, pictures and examples to ease the
  -- understanding of each API element. It is not meant to be an introduction to the
  -- ForSyDe-Atom methodology nor a modeling manual. For proper introductory material
  -- we recommend consulting the following:
  --
  -- * <#ungureanu20a [Ungureanu20a]> is a peer-reviewed comprehensive introduction to
  --   the methodology and the scientific reasoning behind this modeling framework. It
  --   defines the main modeling concepts proposed by ForSyDe-Atom: atom, pattern and
  --   layer.
  -- 
  -- * <#ungureanu20b [Ungureanu20b]> is a collection of step-by-step tutorials and
  --   simple case studies.
  -- 
  -- * the <https://forsyde.github.io/docs/ Documentation> page from the ForSyDe web
  --   site contains up-to-date pointers to further useful material.

  -- * Using the API

  -- ** Loading Modules

  -- | "ForSyDe.Atom" is a collection of libraries, each representing a /layer/, which
  -- is an own DSL for describing one modeling aspect in a CPS. Each module under
  -- "ForSyDe.Atom" , e.g. @ForSyDe.Atom.MoC@ represents a layer, defines the main
  -- /atoms/ and the generic /patterns/ for the respective layer. Each module under a
  -- layer, e.g. @ForSyDe.Atom.MoC.SY@ describes a sub-domain of that layer and
  -- defines the actual semantics of atoms (i.e. overloads atom functions), as well as
  -- other specific patterns and utilities. Due to deliberate name clashes and to
  -- improve readability, each module needs to be imported with a (maybe qualified)
  -- alias:
  --
  -- > import ForSyDe.Atom.MoC    as MoC
  -- > import ForSyDe.Atom.MoC.SY as SY
  -- > 
  -- > -- MoC.comb11 /= SY.comb11
  -- 
  -- By default, the current module, "ForSyDe.Atom", only re-exports a couple of
  -- generic utilities for working with tuples and for plotting signals.

  module ForSyDe.Atom.Utility.Tuple,
  module ForSyDe.Atom.Utility.Plot,

  -- ** Available Layers

  -- | Following are the layers provided by the current version of ForSyDe-Atom. Click
  -- on any of the links for more documentation:
  --
  -- * "ForSyDe.Atom.MoC", a DSL for capturing the semantics of computation and
  --   concurrency according to a model of computation.
  --
  -- * "ForSyDe.Atom.Skel" a DSL for describing structured parallelism.
  --
  -- * "ForSyDe.Atom.Probability", a DSL for describing numerical values as probability
  -- * distributions, e.g. Gaussian.
  --
  -- * "ForSyDe.Atom.ExB", a DSL for extending the pool of values with logic symbols
  --   with well-kown semantics (e.g. absent values).
  
  -- ** Naming Convention
  
  -- | #naming_conv# __IMPORTANT!!!__ All multi-argument functions and utilities
  -- provided by the @<https://forsyde.github.io/forsyde-atom/ forsyde-atom>@ API are
  -- named along the lines of @functionMN@ where @M@ represents the number of
  -- __/curried/__ inputs (i.e. @a1 -> a2 -> ... -> aM@), while @N@ represents the
  -- number of __/tupled/__ outputs (i.e. @(b1,b2,...,bN)@). For brevity, we only
  -- write documentation for functions with 2 inputs and 2 outputs
  -- (i.e. @function22@), while all the other available ones are mentioned as a regex
  -- (i.e. @function[1-4][1-4]@). In case the provided functions are not sufficient,
  -- feel free to implement your own patterns following the examples in the source
  -- code.

  -- * Bibliography

  -- | Here are gathered pointers to documents referenced throughout the API documentation.

  -- | #bonna19# <https://dl.acm.org/doi/10.1145/3342997?cid=99659130620 [Bonna19]> Bonna, R., Loubach, D. S., Ungureanu, G., & Sander, I. (2019). Modeling and simulation of dynamic applications using scenario-aware dataflow. ACM Transactions on Design Automation of Electronic Systems (TODAES), 24(5), 1-29.

  -- | #buck93# [Buck93] Buck, J. T., & Lee, E. A. (1993). Scheduling dynamic dataflow graphs with bounded memory using the token flow model. In /1993 IEEE international conference on acoustics, speech, and signal processing/ (Vol. 1, pp. 429-432). IEEE.
  
  -- | #cassandras09# [Cassandras09] Cassandras, C. G., & Lafortune, S. (2009). Introduction to discrete event systems. Springer Science & Business Media.
  
  -- | #fuji00# [Fujimoto00] Fujimoto, R. M. (2000). Parallel and distributed simulation systems (Vol. 300). New York: Wiley.
  
  -- | #halbwachs91# <http://ieeexplore.ieee.org/document/97300/ [Halbwachs91]> Halbwachs, N., Caspi, P., Raymond, P., & Pilaud, D. (1991). The synchronous data flow programming language LUSTRE. /Proceedings of the IEEE, 79(9)/, 1305-1320.

  -- | #gorlatch03 e# <http://link.springer.com/chapter/10.1007/978-1-4471-0097-3_1#page-1 [Gorlatch03]> Fischer, J., Gorlatch, S., & Bischof, H. (2003). Foundations of data-parallel skeletons. In /Patterns and skeletons for parallel and distributed computing/ (pp. 1-27). Springer London.
  
  -- | #kahn76# [Kahn76] Kahn, G., & MacQueen, D. (1976). Coroutines and networks of parallel processes.

  -- | #lee87# [Lee87] Lee, E. A., & Messerschmitt, D. G. (1987). Synchronous data flow. /Proceedings of the IEEE/, 75(9), 1235-1245.
  
  -- | #lee98# <https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=736561&casa_token=GfbDXDN-wS4AAAAA:uE2y6ZVAGUv8vVwoe9BXTyzdDRFEAWq-c45nZfQurcqTRwU6a1k7rbWHGodSMQDNAOOQqSd7&tag=1 [Lee98]> Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /IEEE Transactions on Computer-Aided Design of Integrated Circuits and Systems, 17(12)/, 1217-1229.

  -- | #lohstroh19# <https://people.eecs.berkeley.edu/~marten/pdf/Lohstroh_etAl_CyPhy19.pdf [Lohstroh19]> Lohstroh, M., Romeo, Í. Í., Goens, A., Derler, P., Castrillon, J., Lee, E. A., & Sangiovanni-Vincentelli, A. (2019). Reactors: A deterministic model for composable reactive systems. In /Cyber Physical Systems. Model-Based Design/ (pp. 59-85). Springer, Cham.

  -- | #reekie95# <http://ptolemy.eecs.berkeley.edu/~johnr/papers/pdf/thesis.pdf [Reekie95]> Reekie, H. J. (1995). Realtime signal processing: Dataflow, visual, and functional programming.
  
  -- | #sander04# <http://people.kth.se/~ingo/Papers/TCAD2004_SystemModeling.pdf [Sander04]> Sander, I., & Jantsch, A. (2004). System modeling and transformational design refinement in ForSyDe [Formal System Design]. /IEEE Transactions on Computer-Aided Design of Integrated Circuits and Systems, 23(1)/, 17-32.
    
  -- | #skillicorn05# <https://books.google.se/books?hl=ro&lr=&id=rQwsL5xsZigC&oi=fnd&pg=PP1&dq=skillicorn+foundation+parallel+programming&ots=UJMBr0uO2Q&sig=ncyXxE0gFNkUZwVOYyFb_ezWlGY&redir_esc=y#v=onepage&q=skillicorn%20foundation%20parallel%20programming&f=false [Skillicorn05]> Skillicorn, D. B. (2005). Foundations of parallel programming (No. 6). Cambridge University Press.

  -- | #stuijk11# [Stuijk11] Stuijk, S., Geilen, M., Theelen, B., & Basten, T. (2011, July). Scenario-aware dataflow: Modeling, analysis and implementation of dynamic applications. /In 2011 International Conference on Embedded Computer Systems: Architectures, Modeling and Simulation/ (pp. 404-411). IEEE.
  
  -- | #ungureanu17# <http://ieeexplore.ieee.org/document/7927270/ [Ungureanu17]> Ungureanu, G., & Sander, I., A layered formal framework for modeling of cyber-physical systems, in /2017 Design, Automation & Test in Europe Conference & Exhibition (DATE)/, 2017, pp. 1715–1720.
  
  -- | #ungureanu18# <https://ieeexplore.ieee.org/document/8342019/ [Ungureanu18]> Ungureanu, G., de Medeiros, J. E. G. & Sander, I., /Bridging discrete and continuous time models with Atoms/, in 2018 Design, Automation & Test in Europe Conference & Exhibition (DATE), 2018, pp. 277-280

  -- | #ungureanu20a# [Ungureanu20a] Ungureanu, G., et. al. , ForSyDe-Atom: Taming complexity in cyber-physical system design with layers, /under review/.

  -- | #ungureanu20b# [Ungureanu20b] Ungureanu, G., ForSyDe-Atom: User manual, version 2, 2020

  
) where

import ForSyDe.Atom.ExB
import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Skel
import ForSyDe.Atom.Utility.Tuple
import ForSyDe.Atom.Utility.Plot
