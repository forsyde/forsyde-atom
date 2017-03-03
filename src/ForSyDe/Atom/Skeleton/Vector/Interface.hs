{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skeleton.Vector.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The library for the 'Vector' type. Contains the main skeletons.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skeleton.Vector.Interface where

import ForSyDe.Atom.Behavior
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.Skeleton.Vector.Core
import ForSyDe.Atom.Skeleton.Vector.Lib

import Prelude hiding (tail, map, length)

-- |
-- #wrap21f# /(*) a context wrapper of type @wrap21@ for a certain/
-- /MoC./
--
-- #wrap11f# /(**) a context wrapper of type @wrap11@ for a certain/
-- /MoC./
--
-- 'zipx' is a template skeleton for \"zipping"\ a vector of
-- signals. It synchronizes all signals (of the same MoC) in a vector
-- and outputs one signal carrying vector extended values. It requires
-- a @wrap21@ and @wrap11@ context wrappers for a particular MoC
-- @e@. All MoCs provide helpers to instantiate this template skeleton
-- with their own context wrappers.
-- 
-- <<includes/figs/skel-zipx-formula.png>>
-- <<includes/figs/skel-zipx-graph.png>>
--
-- __Note:__ the definition above ignores the context wrappers, which
-- are implementation features rather than part of the formalism.
zipx :: MoC e =>
        ((Value (Vector a) -> Value (Vector a) -> Value (Vector a))
             -> (Context e, [Value (Vector a)] -> (Context e, [Value (Vector a)] -> [Value (Vector a)])))
        -- ^ (<#wrap21f *>)
        -> ((Value a -> Value (Vector a)) -> (Context e, [Value a] -> [Value (Vector a)]))
        -- ^ (<#wrap11f **>)
        -> Vector (Stream (e [Value a]))  -- ^ vector of signals of MoC @e@
        -> Stream (e [Value (Vector a)]) -- ^ synchronized result signal of MoC @e@ 
zipx   w21 w11 = red catEv . map unitEv
  where catEv  = (comb21 . w21 . psi21) (<++>)
        unitEv = (comb11 . w11 . psi11) unit

-- |
-- #wrap11v# /(*) a context wrapper of type @wrap11@ for a certain/
-- /MoC./
--
-- #wrap11vv# /(**) a context wrapper of type @wrap11@ for a/
-- /certain MoC. Actually it is the same as the previous one, but the/
-- /type checker needs another instance./
--
-- 'unzipx' is a template skeleton to unzip a signal carrying vector
-- events into a vector of multiple signals. It requires two @wrap11@
-- context wrappers and a @value@ sniffer for a particular MoC
-- @e@. All MoCs provide helpers to instantiate this template skeleton
-- with their own context wrappers.
--
-- <<includes/figs/skel-unzipx-formula.png>>
-- <<includes/figs/skel-unzipx-graph.png>>
--
-- __Note:__ the definition above ignores the context wrappers, which
-- are implementation features rather than part of the formalism.
unzipx :: MoC e =>
          (e [Value (Vector a)] -> [Value (Vector a)]) -- ^ unwraps the value from an event
          -> ((Value (Vector a) -> Value a) -> (Context e, [Value (Vector a)] -> [Value a]))
          -- ^ (<#wrap11v *>)
          -> ((Value (Vector a) -> Value (Vector a)) -> (Context e, [Value (Vector a)] -> [Value (Vector a)]))
          -- ^ (<#wrap11vv **>)
          -> Stream (e [Value (Vector a)]) -- ^ signal of MoC @e@ carrying vectors
          -> Vector (Stream (e [Value a])) -- ^ vector of synced signals of MoC @e@
unzipx sniff w1 w2 sv = (map getFst . scan' selFun) sv
  where getFst = (comb11 . w1 . psi11) first
        selFun = fanoutn n ((comb11 . w2 . psi11) tail)
        n      = (length . unsafeFromValue . head . sniff . headS) sv - 1 
-- TODO: Extremely unsafe, since it sniffs for the first event in a
-- value to decide the length of the `scan` network!!! New safer
-- (probably recursive) definition is needed, based on zipped lists.

