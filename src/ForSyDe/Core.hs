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
-- semantics of heterogeneous cyber-physical systems is the
-- <http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=736561 tagged signal model>.
-- This is a denotational framework introduced by Lee and
-- Sangiovanni-Vincentelli as a common meta model for describing
-- properties of concurrent systems in general terms as sets of possible
-- behaviors. Systems are regarded as /compositions/ of /processes/ acting
-- on /signals/ which are collections of events. Signals are
-- characterized by a /tag system/ which determines causality between
-- events, and could model time, precedence relationships,
-- synchronization points, and other key properties. Based on how tag
-- systems are defined, one can identify several /Models of Computations (MoCs)/
-- as classes of behaviors dictating the semantics of execution
-- and concurrency in a network of processes.
--
-----------------------------------------------------------------------------
module ForSyDe.Core (

  -- * Signals
  
  -- | Signals are described as sets of events composed of tags /T/
  -- and values /V/, where signal /s/ contains a set of events
  -- /e_j/. In other words, we can state that through its tag system,
  -- a signal is /bound/ to a MoC.
  -- 
  -- <<includes/figs/tagged-signal-model.pdf.png>>

  Signal(..),

  -- | Additional functions for the 'Signal' data type are provided
  -- for covenience in library development. For an extended API
  -- documentation consult "ForSyDe.Core.Signal"
  
  -- | #processes#
  
  -- * Processes
  
  -- * Vector type
       module ForSyDe.Core.Vector,
       module ForSyDe.Core.Utilities,
       module ForSyDe.Core.ValueExt,
) where

import ForSyDe.Core.Signal
import ForSyDe.Core.Vector
import ForSyDe.Core.Utilities
import ForSyDe.Core.ValueExt
