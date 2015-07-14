-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core
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
module ForSyDe.Core (
       -- * Signal type class
       module ForSyDe.Core.Signal,
       -- * Absent extended type
       AbstExt (Abst, Prst), fromAbstExt, abstExt, psi, 
       isAbsent, isPresent, abstExtFunc,
       module ForSyDe.Core.Vector,
) where

import ForSyDe.Core.Signal
import ForSyDe.Core.AbsentExt
import ForSyDe.Core.Vector



