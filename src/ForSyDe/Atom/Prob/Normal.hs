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
-- This module defines recipes for normally distributed random variables,
-- i.e. Gaussian distribution. Wraps utilities imported from "Data.Random.Normal".
----------------------------------------------------------------------
module ForSyDe.Atom.Prob.Normal (
  -- * Recipes
  normal, normal',
  normal1, samp1,
  
  -- * Layer

  -- | The "ForSyDe.Atom.Prob" module is re-exported for convenience, so it does not
  -- need to be imported explicitly.

  module ForSyDe.Atom.Prob
  ) where

import System.Random
import qualified Data.Random.Normal as N
import ForSyDe.Atom.Prob


import ForSyDe.Atom.Utility.Plot

-- $setup
-- >>> import qualified ForSyDe.Atom.Utility.Plot as Pl
-- >>> prepare = Pl.prepare
-- >>> prepareL = Pl.prepareL
-- >>> dumpDat = Pl.dumpDat
-- >>> defaultCfg = Pl.defaultCfg

-- | Recipe for normally distributed variables using the default parameters, i.e. mean = 0 and standard deviation = 1.
--
-- >>> gen <- getStdGen 
-- >>> let x = normal' :: Dist Float
-- >>> let hx = histogram (-1) 1 0.2 $ samplesn gen 10000 x
-- >>> dumpDat $ prepare defaultCfg hx
-- Dumped hist1 in ./fig
-- ["./fig/hist1.dat"]
--
-- <<fig/prob-gausp.png>>
normal' :: (Random a, Floating a) => Dist a
normal' = Dist N.normals

-- | Recipe for normally distributed variables.
--
-- >>> gen <- getStdGen 
-- >>> x1  = normal 0.2 2 :: Dist Float
-- >>> x2  = normal 0.5 1 :: Dist Float
-- >>> x3  = normal 0.6 2 :: Dist Float
-- >>> let hx = map (histogram 0 3 0.2 . samplesn gen 10000) [x1,x2,x3]
-- >>> dumpDat $ prepareL defaultCfg hx
-- Dumped hist1, hist2, hist3 in ./fig
-- ["./fig/hist1.dat","./fig/hist2.dat","./fig/hist3.dat"]
--
-- <<fig/prob-gaus.png>>
normal :: (Random a, Floating a)
       => a      -- ^ standard deviation
       -> a      -- ^ mean
       -> Dist a
normal dev m = Dist $ N.normals' (m,dev)


normal1 dev m = fst . N.normal' (m,dev)
samp1 g r = r g
