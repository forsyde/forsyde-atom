-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the core entities of the MoC/synchronization
-- layer: atom interfaces and process constructors as patterns of
-- atoms. It does /NOT/ export any implementation or instantiation of
-- a specific MoC.
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC(


  -- * Atom
  
  MoC(..),

  -- * Process constructors

  -- | As shown in the documentation of "ForSyDe.Core" process
  -- constructors are implemented as compositions of MoC atoms. In
  -- order to avoid working with signals of tuples @unzip@ and in
  -- order for the process network to reflect the passed functions,
  -- process utilities (defined in "ForSyDe.Core.Utility") are also
  -- used.
  --
  -- Due to Haskell's strict type system and the implementation
  -- mechanisms, we need to provide separate constructors @nameXY@,
  -- where @name@ is the process constructor type, @X@ is the number
  -- of inputs and @Y@ is the number of outputs. This module provides
  -- constructors with /X &#8804; 4/ and /Y &#8804; 4/. If needed, the
  -- designer is free to implement her own constructor by following
  -- the atom composition rules in the source code.

  -- ** @comb@

  -- | @comb@ processes map combinatorial functions on signals and
  -- take care of synchronization between input signals.
  --
  -- <<includes/figs/comb-formula.png>>
  -- <<includes/figs/comb-graph.png>>

  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  comb51, comb52, comb53, comb54,

  -- ** @delay@

  -- | The @delay@ process provides both an initial token and shifts the
  -- phase of the signal. In other words, it "delays" a signal with
  -- one event.
  --
  -- <<includes/figs/delay-formula.png>>
  -- <<includes/figs/delay-graph.png>>
  delay, (-&>-),

  -- ** @scanl@

  -- | @scanl@ processes model generates the graph shown below. There
  -- exists a variant with 0 input signals, in which case the process
  -- is a signal generator.
  --
  -- <<includes/figs/scanl-formula.png>>
  -- <<includes/figs/scanl-graph.png>>

  --scanl01, scanl02, scanl03, scanl04,
  scanl11, scanl12, scanl13, scanl14,
  scanl21, scanl22, scanl23, scanl24,
  scanl31, scanl32, scanl33, scanl34,
  scanl41, scanl42, scanl43, scanl44,

  
  -- ** @scanld@

  -- | @scanld@ processes model generates the graph shown below. There
  -- exists a variant with 0 input signals, in which case the process
  -- is a signal generator.
  --
  -- <<includes/figs/scanld-formula.png>>
  -- <<includes/figs/scanld-graph.png>>

  scanld01, scanld02, scanld03, scanld04,
  scanld11, scanld12, scanld13, scanld14,
  scanld21, scanld22, scanld23, scanld24,
  scanld31, scanld32, scanld33, scanld34,
  scanld41, scanld42, scanld43, scanld44,
  
  -- ** @moore@

  -- | @moore@ processes model Moore state machines.
  --
  -- <<includes/figs/moore-formula.png>>
  -- <<includes/figs/moore-graph.png>>

  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  -- ** @mealy@

  -- | @mealy@ processes model Mealy state machines.
  --
  -- <<includes/figs/mealy-formula.png>>
  -- <<includes/figs/mealy-graph.png>>

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,  
  ) where

import ForSyDe.Atom.MoC.Atom
import ForSyDe.Atom.MoC.Cons

