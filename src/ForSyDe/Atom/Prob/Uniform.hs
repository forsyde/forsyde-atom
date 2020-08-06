----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Prob.Uniform
-- Copyright   :  (c) George Ungureanu, 2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines recipes for uniformly distributed random variables. Wraps
-- utilities imported from "System.Random".
----------------------------------------------------------------------
module ForSyDe.Atom.Prob.Uniform (
  -- * Recipes
  uniform, uniformR, uniformD,
  
  -- * Layer

  -- | The "ForSyDe.Atom.Prob" module is re-exported for convenience, so it does not
  -- need to be imported explicitly.

  module ForSyDe.Atom.Prob
  ) where

import System.Random
import ForSyDe.Atom.Prob

-- $setup
-- >>> import qualified ForSyDe.Atom.Utility.Plot as Pl
-- >>> prepare = Pl.prepare
-- >>> dumpDat = Pl.dumpDat
-- >>> defaultCfg = Pl.defaultCfg

-- | Recipe for uniformly distributed over all possible values of that type.
--
-- >>> gen <- getStdGen 
-- >>> let x = uniform :: Dist Float
-- >>> let hx = histogram (-0.5) 1.5 0.1 $ samplesn gen 10000 x
-- >>> dumpDat $ prepare defaultCfg hx
-- Dumped hist1 in ./fig
-- ["./fig/hist1.dat"]
--
-- <<fig/prob-unif.png>>
uniform :: Random a => Dist a
uniform = Dist randoms

-- | Recipe for uniformly distributed over a provided range.
--
-- >>> gen <- getStdGen 
-- >>> let x = uniformR 0 20 :: Dist Int
-- >>> let hx = histogram (-1) 21 1 $ samplesn gen 10000 x
-- >>> dumpDat $ prepare defaultCfg hx
-- Dumped hist1 in ./fig
-- ["./fig/hist1.dat"]
--
-- <<fig/prob-unifr.png>>
uniformR :: Random a
         => a      -- ^ lower bound
         -> a      -- ^ upper bound
         -> Dist a
uniformR rl rh = Dist $ randomRs (rl,rh)

-- | Recipe for uniformly distributed over a range determined by standard deviation.
--
-- >>> gen <- getStdGen 
-- >>> let x = uniformD 5 0.5 :: Dist Float
-- >>> let hx = histogram 4 6 0.1 $ samplesn gen 10000 x
-- >>> dumpDat $ prepare defaultCfg hx
-- Dumped hist1 in ./fig
-- ["./fig/hist1.dat"]
--
-- <<fig/prob-unifd.png>>
uniformD :: (Random a, Num a)
         => a       -- ^ deviation
         -> a       -- ^ mean
         -> Dist a
uniformD dev a = Dist $ randomRs (a - dev , a + dev)
