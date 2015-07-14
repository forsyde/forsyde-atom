-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Signal' datatype and
-- functions operating on it.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Core (
       -- * Signal type class
       module ForSyDe.Shallow.Core.Signal,
       -- * Absent extended type
       AbstExt (Abst, Prst), fromAbstExt, abstExt, psi, 
       isAbsent, isPresent, abstExtFunc,
       module ForSyDe.Shallow.Core.Vector,
) where

import ForSyDe.Shallow.Core.Signal
import ForSyDe.Shallow.Core.AbsentExt
import ForSyDe.Shallow.Core.Vector



