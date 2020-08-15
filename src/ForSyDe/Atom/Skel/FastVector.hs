----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skel.Vector
-- Copyright   :  (c) George Ungureanu, KTH/EECS/ESY 2019-2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This library is is an un-official alternative to 'ForSyDe.Atom.Skel.Vector'
-- meant for simulations of large data which is likely to become too
-- cumbersome. "Fast" 'Vector' functions do not use atoms, but rather use 'Prelude'
-- functions on a wrapped @newtype@ using a native Haskell type (in this case
-- lists). The API tries to copy the exported functions of
-- "ForSyDe.Atom.Skel.Vector" and its submodule so that switching betwen libraries
-- can be made seamlessly just by @Vector@ with @FastVector@ in the library import.
--
-- Useful links:
--
-- * "ForSyDe.Atom" contains general guidelines for using the API
--
-- * "ForSyDe.Atom.Skel.Vector" documents the API which this module is trying to
--   replicate.
--
-- * "ForSyDe.Atom.Skel.FastVector.Matrix" contains a collection of patterns for
--   working with 2-dimensional 'Vector's.
--
-- * "ForSyDe.Atom.Skel.FastVector.Cube" contains a collection of patterns for
--   working with 3-dimensional 'Vector's.
--
-- * "ForSyDe.Atom.Skel.FastVector.DSP" contains a collection of patterns commonly
--   used in signal processing designs.
--
-- * the <ForSyDe-Atom.html#naming_conv naming convention> rules on how to interpret
--   the function names based on their number of inputs and outputs.
----------------------------------------------------------------------


module ForSyDe.Atom.Skel.FastVector (
  module ForSyDe.Atom.Skel.FastVector.Lib,
  module ForSyDe.Atom.Skel.FastVector.Interface
  ) where

import ForSyDe.Atom.Skel.FastVector.Lib hiding (unsafeApply, unsafeLift, evensF, oddsF)
import ForSyDe.Atom.Skel.FastVector.Interface
