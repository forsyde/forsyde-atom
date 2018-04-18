{-# OPTIONS_HADDOCK prune #-}
---------------------------------------------------------------------
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
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
---------------------------------------------------------------------

module ForSyDe.Atom.MoC.SY (

  -- * Synchronous (@SY@) event

  -- | According to <ForSyDe-Atom.html#lee98 [Lee98]>, "two events
  -- are synchronous if they have the same tag, and two signals are
  -- synchronous if all events in one signal are synchronous to an
  -- event in the second signal and vice-versa. A system is
  -- synchronous if every signals in the system is synchronous to all
  -- the other signals in the system."
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
  -- <<docfiles/figs/moc-sy-example.png>>
  --
  -- Implementing the SY tag system is straightforward if we consider
  -- the synchronous 'Signal' as an infinite list. In this case the
  -- tags are implicitly defined by the position of events in a
  -- signal: /t&#8320;/ would correspond with the event at the head of
  -- a signal /t&#8321;/ with the following event, etc... The only
  -- explicit parameter passed to a SY event constructor is the value
  -- it carries &#8712; /V&#8337;/. As such, we can state the
  -- following particularities:
  --
  -- 1. tags are implicit from the position in the
  -- 'ForSyDe.Atom.MoC.Stream.Stream', thus they are completely
  -- ignored in the type constructor.
  --
  -- 1. the type constructor wraps only a value
  --
  -- 1. being a /timed MoC/, the order between events is total
  -- <ForSyDe-Atom.html#lee98 [Lee98]>.
  -- 
  -- 1. from the previous statement we can conclude that there is no
  -- need for an <ForSyDe-Atom-MoC.html#context execution context>
  -- and we can ignore the formatting of functions in
  -- "ForSyDe.Atom.MoC", thus we can safely assume:
  --
  -- <<docfiles/figs/eqs-moc-timed-context.png>>
  SY(..),

  -- * Aliases & utilities

  -- | A set of type synonyms and utilities are provided for
  -- convenience. The API type signatures will feature these aliases
  -- to hide the cumbersome construction of atoms and patters as seen
  -- in "ForSyDe.Atom.MoC".

  Signal, unit, unit2, unit3, unit4, signal,
  
  -- * @SY@ process constuctors

  -- | These SY process constructors are basically specific
  -- instantiations of the patterns of atoms defined in
  -- "ForSyDe.Atom.MoC". Some are also wrapping functions in an
  -- extended behavioural model.

  -- ** Simple

  delay,

  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  
  reconfig11, reconfig12, reconfig13, reconfig14,
  reconfig21, reconfig22, reconfig23, reconfig24,
  reconfig31, reconfig32, reconfig33, reconfig34,
  reconfig41, reconfig42, reconfig43, reconfig44,
  
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

  -- -- ** Storing behavior
  
  -- buffer1, buffer2, buffer3, buffer4,
  
  -- ** Predicate behavior

  -- | These processes manipulate the behavior of a signal based on
  -- predicates on their status.

  when, when', is, whenPresent, filter, filter', fill, hold,
  reactAbst1, reactAbst2, reactAbst3, reactAbst4,

  -- * Interfaces

  toDE, toDE2, toDE3, toDE4,
  toSDF, toSDF2, toSDF3, toSDF4,
  zipx, unzipx, unzipx'  
  
  ) where

import Prelude hiding (filter)
import ForSyDe.Atom.MoC.SY.Core
import ForSyDe.Atom.MoC.SY.Lib
import ForSyDe.Atom.MoC.SY.Interface
