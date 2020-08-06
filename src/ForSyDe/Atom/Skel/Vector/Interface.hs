{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skel.Vector.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The library for the 'Vector' type. Contains the main skeletons.
----------------------------------------------------------------------
module ForSyDe.Atom.Skel.Vector.Interface where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream as S
import ForSyDe.Atom.Skel
import ForSyDe.Atom.Skel.Vector.Core
import ForSyDe.Atom.Skel.Vector.Lib as V (recuri, tail, fanout, fanoutn, reduce1)
  

import Prelude hiding (tail, map, length)
import Control.Applicative

mapS :: MoC e => (a -> b) -> Stream (e a) -> Stream (e b)
mapS = fmap . fmap

-- | 'zipx' is a template skeleton for \"zipping"\ a vector of
-- signals. It synchronizes all signals (of the same MoC) in a vector
-- and outputs one signal with vectors of the synced values. For each
-- signal in the input vector it requires a function which
-- /translates/ a partition of events (see "ForSyDe.Atom.MoC") into
-- sub-vectors.
--
-- There exist helper instances of the 'zipx' skeleton interface for
-- all supported MoCs.
--
-- <<fig/eqs-skel-vector-zipx.png>>
-- <<fig/skel-vector-comm-zipx.png>>
zipx :: MoC e
     => Vector ((Vector a -> Vector a -> Vector a)
                -> Fun e (Vector a) (Fun e (Vector a)
                                      (Ret e (Vector a))))
     -- ^ vector of MoC-specific context wrappers for the function
     -- '<++>'
     -> Vector (Stream (e a))
     -- ^ input vector of signals
     -> Stream (e (Vector a))
     -- ^ output signal of vectors
zipx part =  V.reduce1 sync part . farm11 (mapS unit)
  where sync p = comb21 (p (<++>))


-- | 'unzipx' is a template skeleton to unzip a signal carrying
-- vectors into a vector of multiple signals. It required a function
-- that /splits/ a vector of values into a vector of event partitions
-- belonging to output signals. Unlike 'zipx', it also requires the
-- number of output signals. The reason for this is that it is
-- impossible to determine the length of the output vector without
-- \"sniffing\" the content of the input events, which is out of the
-- scope of skeletons and may lead to unsafe behavior. The length of
-- the output vector is needed in order to avoid infinite recurrence.
--
-- There exist helper instances of the 'unzipx' skeleton interface for
-- all supported MoCs.
--
-- <<fig/eqs-skel-vector-unzipx.png>>
-- <<fig/skel-vector-comm-unzipx.png>>
unzipx :: (MoC e)
       => (Vector a -> Vector (Ret e a))
       -> Integer
       -> Stream (e (Vector a))
       -> Vector (Stream (e a))
unzipx parts n = farm11 rebuild . recuri transpose . mapS parts
  where transpose = fanoutn (n-1) (mapS tail)
        rebuild   = (-*) . mapS first



-- zipx :: (MoC e)
--      => Vector (Fun e a (Vector a))
--      -- ^ vector of functions which partition events to vectors
--      -> Vector (Stream (e a))
--      -- ^ input vector of signals
--      -> Stream (e (Vector a))
--      -- ^ output signal of vectors
-- zipx parts = reduce transpose . farm21 ev2vec parts
--   where transpose = (liftA2 . liftA2) (<++>)
--         ev2vec p  = (p -.-) 


-- unzipx :: (MoC e)
--        => Vector (Vector a -> (Ret e a))
--        -> Vector (Vector a -> Vector a)
--        -> Stream (e (Vector a))
--        -> Vector (Stream (e a))
-- unzipx takes drops = farm11 (-*) . farm21 (fmap . fmap) takes . recur1 (fmap . fmap) drops

