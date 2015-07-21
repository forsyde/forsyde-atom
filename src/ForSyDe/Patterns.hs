-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Patterns
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015;
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable 
-- 
-- The process network patterns are based on algorithmic skeletons. They
-- exploit the homomorphic nature of vectors to create structured networks
-- fit for parallel execution. The 'Vector' data type is extended so that
-- is equipped with means of creating constructors capable of applying 
-- processes to vectors in a similar manner in which processes apply functions
-- to signals.
-- 
-- There are two categories of process network constructors:
--
-- * transition patterns alter only the structure of the vector containers
--   but do not touch the data transported by their signals
-- 
-- * applicative patterns alter both structure AND the data transported by
--   the signals, by applying processes. 
-----------------------------------------------------------------------------

module ForSyDe.Patterns(  
  -- * Applicative process network constructors
  module ForSyDe.Patterns.Applicative,
  -- * Transition process network constructors 
  module ForSyDe.Patterns.Transition,
) where

import ForSyDe.Patterns.Transition
import ForSyDe.Patterns.Applicative








