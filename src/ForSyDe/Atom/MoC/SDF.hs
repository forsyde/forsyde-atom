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
-- The @SY@ library implements the atoms holding the sematics for the
-- synchronous computation model. It also provides a set of helpers
-- for properly instantiating process network patterns as process
-- constructors.
--
-- 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.SDF (
  module ForSyDe.Atom.MoC.SDF.Core,
  module ForSyDe.Atom.MoC.SDF.Lib
  ) where

import Prelude hiding (filter)
import ForSyDe.Atom.MoC.SDF.Core
import ForSyDe.Atom.MoC.SDF.Lib
