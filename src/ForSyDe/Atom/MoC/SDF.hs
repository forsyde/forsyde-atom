{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SDF
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--
-- The @SDF@ library implements the atoms holding the sematics for the
-- synchronous data flow computation model. It also provides a set of
-- helpers for properly instantiating process network patterns as
-- process constructors.
--
-- __IMPORTANT!!!__ Most of the multi-parameter higher-order functions
-- provided by the library API are named along the lines of
-- @functionMN@ where @M@ represents the number of __/curried/__
-- inputs (i.e. @a1 -> a2 -> ... -> aM@), while @N@ represents the
-- number of __/tupled/__ outputs (i.e. @(b1,b2,...,bN)@). To avoid
-- repetition we shall only provide documentation for functions with 2
-- inputs and 2 outputs (i.e. @function22@).
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.SDF (
  
  -- * Synchronous data flow (@SDF@) event

  -- | The synchronous data flow (@SDF@) MoC is the first untimed MoC
 -- implemented by the @forsyde-atom@ framework. On untimed MoCs,
 -- <#lee98 [1]>, states that: "when tags are partially ordered rather
 -- than totally ordered, we say that the system is untimed. Untimed
 -- systems cannot have the same notion of causality as timed systems
 -- [see 'ForSyDe.Atom.MoC.SY.SY']. The equivalent intuition is
 -- provided by the monotonicity [and continuity]
 -- condition[s]". Regarding SDF, it states that "is a special case of
 -- Kahn process networks <#kahn76 [2]>. A dataflow process is a Kahn
 -- process that is also sequential, where the events on the self-loop
 -- signal denote the firings of the dataflow actor. The firing rules
 -- of a dataflow actor are partial ordering constraints between these
 -- events and events on the inputs."
  --
  -- Based on the above insights, let us define the @forsyde-atom@
  -- interpretation of SDF:
  --
  -- [The SDF MoC] is abstracting the execution semantics of a system
  -- where computation is performed according to some firing rules
  -- where the production and the consumption is fixed. 
  --
  -- Below is a /possible/ behavior in time of the input and the
  -- output signals of a SDF process, to illustrate these semantics:
  --
  -- <<includes/figs/sy-example.png>>
  --
  -- Implementing the SDF tag system implied a series of engineering
  -- decisions which lead to formulating __design rule #4__ (see
  -- "ForSyDe.Atom") and embedding it into the atom construction (see
  -- "ForSyDe.Atom.MoC"). Namely, we use an implicitly tagged
  -- 'ForSyDe.Atom.Signal' which, by its nature is associated with a
  -- total order, group its events into /partitions/ to mimic the
  -- partial ordering. This partitioning mirrors the firing rules with
  -- which (atom) processes were endowned with. We can list the
  -- following particularities that come with this implementation:
  --
  -- 1. tags are implicit and inferred through a combination of the
  -- position in the signal and the position in the partition.
  --
  -- 1. the type constructor wraps around a list of values,
  -- symbolizing the events sharing a particular tag.
  --
  -- 1. while a correct format for our above interpretation would be
  -- @[SDF (Value a)]@ (i.e. a partition of events of extended
  -- values), the type polymorphism mechanics imposed the formal @SDF
  -- [Value a]@ (i.e. event constructors around partitions of extended
  -- values). Nevertheless, we can easily prove semantic equivalence
  -- between the two types, by ensuring that both constructors are
  -- functors and traversable.
  --
  -- 1. SDF atoms /do/ require a context (see below list): the
  -- consumption /c/ and production /p/ rates, as parameters which
  -- determine the atom's firing rules.
  --
  -- 1. A ForSyDe SDF atom will fire only when there are enough events
  -- to trigger its inputs. Once a firing occurs, it will take care of
  -- partitioning the input or output signals.
  --
  -- 1. As implemented in the current version of @forsyde-atom@, only
  -- /c/ is a primary parameter required by the atoms. /p/ is only a
  -- secondary parameter and it is required by the function wrapper to
  -- perform a /run-time/ sanity check on the produced partition
  -- (which should be correctly defined by the user-provided
  -- function).
  --
  -- > type Context SDF = Int
  --
  SDF(..),
  
  -- * Aliases & utilities

  -- | For convenience we also provide a set of type synonyms and
  -- utilities to ease in the design of systems. The API type
  -- signatures will feature these aliases to hide the cumbersome
  -- construction of atoms and atom patters as seen in
  -- "ForSyDe.Atom.MoC".

  Partition, Sig, part2, signal,

  wrap11, wrap21, wrap31, wrap41, wrap51, wrap61, wrap71, wrap81, 
  wrap12, wrap22, wrap32, wrap42, wrap52, wrap62, wrap72, wrap82, 
  wrap13, wrap23, wrap33, wrap43, wrap53, wrap63, wrap73, wrap83, 
  wrap14, wrap24, wrap34, wrap44, wrap54, wrap64, wrap74, wrap84,
  
  -- * @SDF@ process constuctors

  -- | These SDF-specific process constructors are basically
  -- specific instantiations of the network patterns defined in
  -- "ForSyDe.Atom.MoC", also wrapping functions in a behavioural
  -- model.

  -- ** Default behavior

  -- | These processes manifest a default behavior as defined in
  -- "ForSyDe.MoC.Behavior", when it comes to dealing with special
  -- events.
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  delay,
  
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


  -- * Bibliography

  -- | #lee98# [1] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /Computer-Aided Design of Integrated Circuits and Systems, IEEE Transactions on, 17(12)/, 1217-1229.
  
  -- | #kahn76# [2] Kahn, G., & MacQueen, D. (1976). Coroutines and networks of parallel processes.
  
  ) where

import Prelude hiding (filter)
import ForSyDe.Atom.MoC.SDF.Core
import ForSyDe.Atom.MoC.SDF.Lib
