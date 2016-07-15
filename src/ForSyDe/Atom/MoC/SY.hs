-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SY
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @SY@ library implements the atoms holding the sematics for the
-- synchronous computation model. It also provides a set of helpers
-- for properly instantiating process network patterns as process
-- constructors.
--
-- 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.SY (

  -- * Synchronous (@SY@) event

  -- | According to <#lee98 [1]>, "two events are synchronous if they
  -- have the same tag, and two signals are synchronous if all events
  -- in one signal are synchronous to an event in the second signal
  -- and vice-versa. A system is synchronous if every signals in the
  -- system is synchronous to all the other signals in the system."
  --
  -- The synchronous (@SY@) MoC defines no notion of physical time,
  -- its tag system suggesting in fact the precedence among events. To
  -- further demystify its mechanisms, we can formulate the following
  -- proposition:
  --
  -- [The SY MoC] is abstracting the execution semantics of a system
  -- where computation is assumed to perform instantaneously (with
  -- zero delay), at certain synchronization points, when data is
  -- assumed to be available.
  --
  -- Below is a /possible/ behavior in time of the input and the
  -- output signals of a SY process, to illustrate these semantics:
  --
  -- <<includes/figs/sy-example.png>>
  --
  -- Implementing the SY tag system is straightforward if we consider
  -- signals as infinite lists. In this case the tags are implicitly
  -- defined by the position of events in a signal. In this case
  -- /t&#8320;/ would correspond with the event at the head of a
  -- signal /t&#8321;/ with the second event, and so on. Thus we do
  -- not need to explicitate tags, the only explicit argument passed
  -- to a SY event constructor being its value &#8712;
  -- /V&#8337;/. Thus we have the definition of the SY event
  -- implementing (among others) the 'MoC' class:
  
  SY(..),

  -- | For convenience we also provide a type alias for a SY signal
  -- which stands for "signal of synchronous events where the values
  -- are extended", and a couple of utilities:

  Sig, event, signal, 

  -- * @SY@ process constuctors

  -- | These SY-specific process constructors are basically
  -- specific instantiations of the network patterns defined in
  -- "ForSyDe.Core.MoC" also wrapping functions in a behavioural
  -- model as defined in "ForSyDe.Core.ValueExt".
  
  -- ** @comb@

  -- | @comb@ processes map combinatorial functions on signals and
  -- take care of synchronization between input signals. It
  -- instantiates the @combXY@ process constructor
  -- (eg. 'ForSyDe.Core.MoC.comb11').
  --
  -- <<includes/figs/sy-comb-graph.png>>

  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  -- ** @delay@

  -- | The @delay@ process "delays" a signal with one event. It is an
  -- instantiation of the 'ForSyDe.Core.MoC.delay' constructor.
  --
  -- <<includes/figs/sy-delay-graph.png>>
  delay,


  -- ** @constant@

  -- | A signal generator which keeps a value constant. It
  -- is actually an instantiation of the @scanld0X@ constructor
  -- (eg. 'ForSyDe.Core.MoC.scanld01').
  --
  -- <<includes/figs/sy-constant-graph.png>>
  
  constant1, constant2, constant3, constant4,
  
  -- ** @generate@

  -- | A signal generator based on a function and a kernel value. It
  -- is actually an instantiation of the @scanld0X@ constructor
  -- (eg. 'ForSyDe.Core.MoC.scanld01').
  --
  -- <<includes/figs/sy-generate-graph.png>>
  
  generate1, generate2, generate3, generate4,
  
  -- ** @scanld@

  -- | @scanld@ is a state machine without an output decoder. It is an
  -- instantiation of the @scanXY@ MoC constructor
  -- (eg. 'ForSyDe.Core.MoC.scanld11').
  --
  -- <<includes/figs/sy-scanld-graph.png>>

  scanld11, scanld12, scanld13, scanld14,
  scanld21, scanld22, scanld23, scanld24,
  scanld31, scanld32, scanld33, scanld34,
  scanld41, scanld42, scanld43, scanld44,

  -- ** @scanl@

  -- | @scanl@ is a state machine without an output decoder. It is an
  -- instantiation of the @scanXY@ MoC constructor
  -- (eg. 'ForSyDe.Core.MoC.scanl11').
  --
  -- <<includes/figs/sy-scanl-graph.png>>

  scanl11, scanl12, scanl13, scanl14,
  scanl21, scanl22, scanl23, scanl24,
  scanl31, scanl32, scanl33, scanl34,
  scanl41, scanl42, scanl43, scanl44,

  
  -- ** @moore@

  -- | @moore@ processes model Moore state machines. It is an
  -- instantiation of the @mooreXY@ MoC constructor
  -- (eg. 'ForSyDe.Core.MoC.moore11').
  --
  -- <<includes/figs/sy-moore-graph.png>>

  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  -- ** @mealy@

  -- | @mealy@ processes model Mealy state machines. It is an
  -- instantiation of the @mealyXY@ MoC constructor
  -- (eg. 'ForSyDe.Core.MoC.mealy11').
  --
  -- <<includes/figs/sy-mealy-graph.png>>

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,

  -- ** @buffer@

  -- | @buffer@ processes roughly implement a memory model which
  -- stores all input present and known values.
  --
  -- <<includes/figs/sy-buffer-graph.png>>

  -- buffer1, buffer2, buffer3, buffer4,
  
  -- -- ** Predicate processes

  -- -- | These processes manipulate the behavior of a signal based on
  -- -- predicates on their status.

  -- when, filter, fill,
  -- hold,
         
  -- * Bibliography

  -- | #lee98# [1] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /Computer-Aided Design of Integrated Circuits and Systems, IEEE Transactions on, 17(12)/, 1217-1229.
  
  -- | #hal91# [2] Halbwachs, N., Caspi, P., Raymond, P., & Pilaud, D. (1991). The synchronous data flow programming language LUSTRE. /Proceedings of the IEEE, 79(9)/, 1305-1320.
  
  ) where

import Prelude hiding (filter)
import ForSyDe.Atom.MoC.SY.Core
import ForSyDe.Atom.MoC.SY.Lib
