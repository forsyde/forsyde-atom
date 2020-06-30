{-# OPTIONS_HADDOCK show-extensions, prune #-}
module ForSyDe.Atom.MoC.DE.React (
  RE(..),
  
  -- * Aliases & utilities

  -- | These are type synonyms and utilities provided for user
  -- convenience. They mainly concern the construction and usage of
  -- signals.

  Signal, SignalBase, unit, unit2, unit3, unit4, infinite, until,
  signal, checkSignal, readSignal,
  
   -- * @DE@ process constuctors

  -- | These SY process constructors are basically specific
  -- instantiations of the atom patterns defined in
  -- "ForSyDe.Atom.MoC".
  
  -- ** Simple

  -- | These are mainly direct instantiations of patterns defined in
  -- "ForSyDe.Atom.MoC", using DE-specific utilities.
  
  delay, unsafeDelay, delay',
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  
  constant1, constant2, constant3, constant4,

  generate1, generate2, generate3,
  
  state11, state12, state13,
  state21, state22, 
  state31, 

  stated11, stated12, stated13,
  stated21, stated22, 
  stated31, 


  moore11, moore12, moore13,
  moore21, moore22, moore23,
  moore31, moore32, moore33,

  mealy11, mealy12, mealy13,
  mealy21, mealy22, mealy23,
  mealy31, mealy32, mealy33,

   syncAndHold2, syncAndHold3, syncAndHold4,
   syncAndFill2, syncAndFill3, syncAndFill4,

   -- ** Interfaces

   -- ** Lingua Franca constructs

   timer0, timer, timer',

   state1, state2, state3,

   actionD,

  reaction11, reaction12, reaction13, reaction14,
  reaction21, reaction22, reaction23, reaction24,
  reaction31, reaction32, reaction33, reaction34,
  reaction41, reaction42, reaction43, reaction44,
  
  ) where

import ForSyDe.Atom.MoC.DE.React.Core
import ForSyDe.Atom.MoC.DE.React.Lib
import ForSyDe.Atom.MoC.DE.React.Interface
import ForSyDe.Atom.MoC.DE.React.LF
import Prelude hiding (until)
