{-# LANGUAGE GADTs, FlexibleContexts, PostfixOperators #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Prob
-- Copyright   :  (c) George Ungureanu, 2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the Probability layer, and is concerned in modeling aspects of
-- uncertainty in values. For a brief presentation of the theoretical background of
-- this layer, the atom approach, but also an incentive to use this layer in CPS
-- design, please consult <ForSyDe-Atom.html#ungureanu20a [Ungureanu20a]>.
--
-- The idea of this layer is wrapping certain values in a 'Dist' type which represents
-- a probabilistic distribution of values, and lift any function operatin on values in
-- the Probability layer. As a practical implementation, the 'Dist' type contains a
-- recipe to obtain a distributed value \(\in\alpha\) from a random experiment
-- \(\in\mathbf{1}\), i.e. a function \(\mathbf{1}\rightarrow\alpha\), using numerical
-- methods. As such any layered system involves lazy propagation (i.e. functional
-- compositions) of recipes which get evaluated once we need to plot/trace the "final"
-- behavior. In this respect it is very similar to the "ForSyDe.Atom.MoC.CT" DSL.
--
-- Currently the Probability layer exports the following types of distributions, each
-- defined in its own submodule:
--
-- * "ForSyDe.Atom.Prob.Uniform" defines the uniform (i.e. random, square)
--   distribution.
--
-- * "ForSyDe.Atom.Prob.Normal" defines the normal (i.e. Gaussian) distribution.
--
-- Useful links:
--
-- * "ForSyDe.Atom" contains general guidelines for using the API
--
-- * the <ForSyDe-Atom.html#naming_conv naming convention> rules on how to interpret
--   the function names based on their number of inputs and outputs.
----------------------------------------------------------------------
module ForSyDe.Atom.Prob (
  -- * Distribution type
  Dist(..),
  
  -- * Atoms

  -- | Since the layer's type is unique, atoms are presented as regular functions, not
  -- as type class methods.
  (%.), (%*), samples,
  
  -- * Patterns
  sample, samplesn,
  
  trans11, trans21, trans31, trans41, trans51, trans61, trans71, trans81,
  trans12, trans22, trans32, trans42, trans52, trans62, trans72, trans82,
  trans13, trans23, trans33, trans43, trans53, trans63, trans73, trans83,
  trans14, trans24, trans34, trans44, trans54, trans64, trans74, trans84,
  -- * Utilities
  Histogram(..), histogram, getStdGen, mkStdGens
  ) where

import ForSyDe.Atom.Utility.Tuple
import System.Random

-- | Unlike all the other layers, all the subdomains of the Probability layer share
-- the same enabling type 'Dist', the only thing differing being the numerical recipe
-- to obtain a distributed value.
--
-- To make full advantage of Haskell's lazyness and its native libraries for random
-- number generation (e.g. "System.Random") we represent recipes in the most generic
-- form as functions \(\mathbf{1}\rightarrow[\alpha]\) from a seed (here 'StdGen') to
-- an /infinite list/ containing all possible values in a random sequence. Any
-- practical simulation/tracing thus needs to limit this list to a finite number of
-- experiments.
data Dist a where
  Dist :: {recipe :: StdGen -> [a]} -> Dist a

-- | Ensures functional composition of recipes
instance Functor Dist where
  fmap f (Dist g) = Dist (map f . g)

-------------- ATOMS -------------

-- | The @map@ atom. It lifts an abitrary function \(f\) from a layer below and
-- creates a random variable \(\mathbf{y}=f(\mathbf{x})\) by mapping every point from
-- the original random variable according to \(f\).
(%.) :: (a -> b) -> Dist a -> Dist b
f %. (Dist g)          = Dist (map f . g)

-- | The applicative atom. Transforms one random variable distribution to another by
-- mapping samples.
(%*) :: Dist (a -> b) -> Dist a -> Dist b
(Dist f) %* (Dist g) = Dist (\a -> f a <*> g a)

-- | Returns all possible values of a random variable as an infinite list of values.
samples :: StdGen -> Dist a -> [a]
samples g (Dist r) = r g

------------ PATTERNS ------------

-- | Samples a random variable by running /one/ experiment trial defined by a certain distribution.
sample gen = head . tail . samples gen

-- | Samples a random variable by running /n/ experiment trials defined by a certain distribution. This is equivalent to running a Monte Calro experiment on /n/ samples.
samplesn gen n = take n . samples gen


trans11 p v1                      = (p %. v1)
trans21 p v1 v2                   = (p %. v1 %* v2)
trans31 p v1 v2 v3                = (p %. v1 %* v2 %* v3)
trans41 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4)
trans51 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5)
trans61 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6)
trans71 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7)
trans81 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 %* v8)
trans12 p v1                      = (p %. v1 |<)
-- | This pattern transforms a (set of) random distribution(s) into another (set) by
-- composing their recipes with an arbitrary function. In other words it lifts an
-- arbitrary function to the Probability layer.
--
-- Constructors: @trans[1-8][1-4]@
trans22 p v1 v2                   = (p %. v1 %* v2 |<)
trans32 p v1 v2 v3                = (p %. v1 %* v2 %* v3 |<)
trans42 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4 |<)
trans52 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5 |<)
trans62 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 |<)
trans72 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 |<)
trans82 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v5 %* v8 |<)
trans13 p v1                      = (p %. v1 |<<)
trans23 p v1 v2                   = (p %. v1 %* v2 |<<)
trans33 p v1 v2 v3                = (p %. v1 %* v2 %* v3 |<<)
trans43 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4 |<<)
trans53 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5 |<<)
trans63 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 |<<)
trans73 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 |<<)
trans83 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v5 %* v8 |<<)
trans14 p v1                      = (p %. v1 |<<<)
trans24 p v1 v2                   = (p %. v1 %* v2 |<<<)
trans34 p v1 v2 v3                = (p %. v1 %* v2 %* v3 |<<<)
trans44 p v1 v2 v3 v4             = (p %. v1 %* v2 %* v3 %* v4 |<<<)
trans54 p v1 v2 v3 v4 v5          = (p %. v1 %* v2 %* v3 %* v4 %* v5 |<<<)
trans64 p v1 v2 v3 v4 v5 v6       = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 |<<<)
trans74 p v1 v2 v3 v4 v5 v6 v7    = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 |<<<)
trans84 p v1 v2 v3 v4 v5 v6 v7 v8 = (p %. v1 %* v2 %* v3 %* v4 %* v5 %* v6 %* v7 %* v8 |<<<)

------------ UTILITIES ------------

-- | Returns a list of generators, to be used in various purposes
mkStdGens :: StdGen -> [StdGen]
mkStdGens = map mkStdGen . randoms

-- | Histogram representation as a zipped list of bins, each bin paired with its
-- center value.
newtype Histogram = Hist { getBins :: [(Rational,Int)] }

instance Show Histogram where
  showsPrec p = showParen (p>1) . showBins . getBins
    where
      showBins (x:xs)  = showChar '{' . showBin x . showBins' xs
      showBins []      = showChar '{' . showChar '}'
      showBins' (x:xs) = showChar ' ' . showBin x . showBins' xs
      showBins' []     = showChar '}'
      showBin (a,b)    = showChar '(' . shows (fromRational a :: Float) .
                         showChar ',' . shows b . showChar ')'

-- | Returns the histogram of (a list of) experiments.
--
-- >>> let xs = [1..10] ++ [4..7]
-- >>> histogram 1 10 2 xs
-- {(1.0,1) (3.0,2) (5.0,4) (7.0,4) (9.0,2)}
histogram :: (Ord a, Num a, Real a)
          => a         -- ^ value of leftmost bin (minimum covered) 
          -> a         -- ^ value of rightmost bin (maximum covered)
          -> Rational  -- ^ step
          -> [a]       -- ^ set of experiments
          -> Histogram -- ^ histogram
histogram l r step xs =  Hist $ zipWith (\(x,_) h->(x+halfstep,h)) range bins
  where
    halfstep = step / 2
    bins     = count <$> range <*> pure xs
    range    = let l' = realToFrac l
                   r' = realToFrac r
                   ix = map realToFrac [(0::Int)..]
               in takeWhile (\(x,_) -> x < r')
                  [(l' + k*step - halfstep , l' + k*step + halfstep ) | k <- ix]
    count (x,y) = length.filter (\a -> x <= realToFrac a && realToFrac a < y)
