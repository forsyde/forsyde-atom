-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronuous library defines process constructors, processes and a signal conduit
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process. 
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY (
         -- * Synchronous signal
         module ForSyDe.MoC.SY.Signal,
         -- * Process constructors
         module ForSyDe.MoC.SY.Process
       ) where

import ForSyDe.MoC.SY.Signal hiding (Stream (..), (§-), (-§-), (§§-), (-§§-), (§§!-), (-§§!-), anyS)
import ForSyDe.MoC.SY.Process

