{-# OPTIONS_HADDOCK prune #-}
---------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SY
-- Copyright   :  (c) George Ungureanu, 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @SY@ library implements the atoms that operate according to the synchronous
-- reactive model of computation. This module also provides a set of helpers for
-- instantiating the MoC layer patterns described in the "ForSyDe.Atom.MoC" module as
-- meaningful synchronous process constructors.
--
-- This module exports a reduced SY language, where all processes are assumed to
-- operate on the same clock rate. For an extended SY language of implicit multi-clock
-- rate systems, check "ForSyDe.Atom.MoC.SY.Clocked". Alternatively, this layer can be
-- extended by explicitly wrapping a "ForSyDe.Atom.ExB"-like layer below it.
--
-- Useful pointers:
--
-- * "ForSyDe.Atom" contains general guidelines for using the API
--
-- * "ForSyDe.Atom.MoC" documents details about the internals of the MoC layer, the
--   atoms and the basic structure of all process constructors as MoC patterns.
--
-- * "ForSyDe.Atom.MoC.SY.Clocked" contains an extended SY language of multi-rate
--   clocked systems.
--
-- * the <ForSyDe-Atom.html#naming_conv naming convention> rules on how to interpret
--   the function names based on their number of inputs and outputs.
---------------------------------------------------------------------

module ForSyDe.Atom.MoC.SY (

  -- * Synchronous (@SY@) event

  -- | Acording to the tagged signal model <ForSyDe-Atom.html#lee98 [Lee98]> "two
  -- events are synchronous if they have the same tag, and two signals are synchronous
  -- if all events in one signal are synchronous to an event in the second signal and
  -- vice-versa. A system is synchronous if every signals in the system is synchronous
  -- to all the other signals in the system."  The synchronous (@SY@) MoC defines no
  -- notion of physical time, its tag system suggesting in fact the precedence among
  -- events. In simpler words:
  --
  -- [The SY MoC] is abstracting the execution semantics of a system where computation
  --   is assumed to perform instantaneously (with zero delay), at certain
  --   synchronization instants, when data is assumed to be available.
  --
  -- Below is a /possible/ behavior in time of the input and the output signals of a
  -- SY process, to illustrate these semantics:
  --
  -- <<fig/moc-sy-example.png>>
  --
  -- The SY tag system is derived directly from the 'ForSyDe.Atom.MoC.Stream' host.
  -- Hence a SY 'Signal' is isomorphic to an infinite list, where tags are implicitly
  -- defined by the position of events in a signal: \(t_0\) corresponds with the event
  -- at the head of a signal, \(t_1\) with the following event, etc... The only
  -- explicit parameter passed to a SY event constructor is the value it carries \(\in
  -- V\). As such, we can state the following particularities:
  --
  -- 1. tags are implicit from the position in the 'ForSyDe.Atom.MoC.Stream.Stream',
  --    thus they are ignored in the type constructor.
  --
  -- 1. the type constructor wraps only a value
  --
  -- 1. being a /timed MoC/, the order between events is interpreted as total.
  -- 
  -- 1. there is no need for an <ForSyDe-Atom-MoC.html#context execution context> and
  --    we can ignore the formatting of functions in "ForSyDe.Atom.MoC", thus we can
  --    safely assume that \[ \Gamma\vdash\alpha\rightarrow\beta =
  --    \alpha\rightarrow\beta \]
  SY(..),

  -- * Aliases & utilities

  -- | These are type synonyms and utilities provided for user convenience. They
  -- mainly concern the construction and usage of signals.

  Signal, unit, unit2, unit3, unit4, signal, fromSignal, readSignal,
  
  -- * @SY@ process constuctors

  -- | These are specific implementations of the atom patterns defined in
  -- "ForSyDe.Atom.MoC".
  
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

  -- ** Hybrid

  -- | Processes that can wrap other MoCs inside a DE execution model.

  interleave2, interleave3, interleave4,
  
  -- ** Interfaces

  toDE, toDE2, toDE3, toDE4,
  toSDF1, toSDF2, toSDF3, toSDF4,
  toSDF1', toSDF2', toSDF3', toSDF4',
  zipx, unzipx, unzipx'  
  ) where

import Prelude hiding (filter)
import ForSyDe.Atom.MoC.SY.Core
import ForSyDe.Atom.MoC.SY.Lib
import ForSyDe.Atom.MoC.SY.Interface
import ForSyDe.Atom.MoC.SY.Hybrid
