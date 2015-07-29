{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Process
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF.Process  where

import ForSyDe.Core
import ForSyDe.MoC.SDF.Signal

-- | The `combSY` take a combinatorial function as argument and returns a process with one input signals and one output signal.
combSDF :: Int -> Int -> ([a] -> [b]) -- ^ combinatorial function
       -> SignalSDF a -- ^ input signal
       -> SignalSDF b -- ^ output signal
combSDF c p f xs = let (x,xs') = splitAt' nx xs 
                   in if length x == c then f x <+> 

(⨀) :: ([a] -> b) -> (Int,Signal a) -> Signal b
f ⨀ (nx,xs) = let (x,xs') = splitAt' nx xs 
              in if P.length x == nx then f x :+ f ⨀ (nx,xs') else Null


mapSDF :: Int -> Int -> ([a] -> [b]) -> Signal a -> Signal b
mapSDF _ _ _ NullS   = NullS
mapSDF c p f xs     
    | c <= 0         = error "mapSDF: Number of consumed tokens must be positive integer" 
    | not $ sufficient_tokens c xs 
                     = NullS
    | otherwise      = if length produced_tokens == p then
                          signal produced_tokens +-+ mapSDF c p f (dropS c xs) 
                       else   
                          error "mapSDF: Function does not produce correct number of tokens" 
                       where consumed_tokens = fromSignal $ takeS c xs
                             produced_tokens = f consumed_tokens
