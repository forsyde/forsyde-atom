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

  -- * Signals

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
  -- <<includes/figs/tagged-signal-model.pdf.png>>

  Signal(..),

  -- | Additional functions for the 'Signal' data type are provided
  -- for covenience in library development. For an extended API
  -- documentation consult "ForSyDe.Core.Signal"

  
  -- | #processes#

  -- * Processes
  
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
  -- <<includes/figs/process_definition.pdf.png>>
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
  -- <<includes/figs/ser-composition-formula.pdf.png>>
  --
  -- [@process networks@] in ForSyDe originate from Reekie's process
  -- nets <#reekie95 [2]> and describe ForSyDe systems in terms of
  -- (tuple of) compositions of processes. As seen in the type
  -- signature (function from signals to signals), a process network
  -- is a process itself. The composition above @p2 . p1 @ can also be
  -- regarded as a process network.
  --
  -- <<includes/figs/process-network-formula.pdf.png>>

  -- * Extended values

  -- | Dealing with cyber-physical systems, ForSyDe needs to model
  -- special behaviour such as the absence of events, or
  -- non-deterministic values. This is expressed in the current
  -- library by extending the set of values /V/
  -- (see <#sig-definition signal definition>) with special tokens
  -- carrying behavioural semantics, by wrapping arbitrary types into
  -- a new data type. Currently @forsyde-atom@ supports the following
  -- special value extensions:
  --
  -- [@absent event@ &#8869;] determines the absence of an event at time (tag)
  -- /t/
  -- [@undefined value@ ?] determines a non-determinate value,
  -- similar to the "anything" (@x@ value) in VHDL
  --
  -- <<includes/figs/extended-values.pdf.png>>

  Value(..),

  
  
  module ForSyDe.Core.ValueExt,
         
  
  -- * Vector type
       module ForSyDe.Core.Vector,
       module ForSyDe.Core.Utilities,

  -- * Bibliography

  -- | #lee98# [1] Lee, E. A., & Sangiovanni-Vincentelli, A. (1998). A framework for comparing models of computation. /Computer-Aided Design of Integrated Circuits and Systems, IEEE Transactions on, 17(12)/, 1217-1229. 

  -- | #reekie95# [2] Reekie, H. J. (1995). Realtime signal processing: Dataflow, visual, and functional programming. 

       
) where

import ForSyDe.Core.Signal
import ForSyDe.Core.Vector
import ForSyDe.Core.Utilities
import ForSyDe.Core.ValueExt
