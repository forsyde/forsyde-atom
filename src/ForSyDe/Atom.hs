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
  -- * "ForSyDe.Atom.Skeleton" a DSL for describing structured parallelism.
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

  -- | #ungureanu20a# [Ungureanu20a] Ungureanu, G., et. al. , /ForSyDe-Atom: Taming complexity in cyber-physical system design with layers/, under review.

  -- | #ungureanu20b# [Ungureanu20b] Ungureanu, G., /ForSyDe-Atom: User manual/, version 2, 2020

  
) where

import ForSyDe.Atom.ExB
import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.Time
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Skeleton
import ForSyDe.Atom.Utility.Tuple
import ForSyDe.Atom.Utility.Plot
