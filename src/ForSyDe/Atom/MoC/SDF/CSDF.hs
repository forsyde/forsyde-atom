{-# LANGUAGE PostfixOperators, TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SDF.CSDF
-- Copyright   :  (c) George Ungureanu, KTH/EECS/ESY 2018-2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The @CSDF@ library implements a DSL of atoms operating according to the
-- cyclo-static dataflow model of computation, in terms of the atoms of
-- "ForSyDe.Atom.MoC.SDF". As such SADF patterns are operating on
-- 'ForSyDe.Atom.MoC.SDF.SDF' signals, i.e. they have the exact same time semantics,
-- hence there is no need for MoC interfaces between these two MoCs.
--
-- CSDF is another attempt to increase SDF's expresivity in a controlled manner by
-- allowing dynamic firing rates in a static sequence which repeat cyclically. To implement this library we reused everything: the CSDF actors are implemented as specific "ForSyDe.Atom.MoC.SDF.SADF" patterns, and everything else is re-exported from "ForSyDe.Atom.MoC.SDF".
--
--
-- Useful pointers:
--
-- * "ForSyDe.Atom" contains general guidelines for using the API
--
-- * "ForSyDe.Atom.MoC.SDF" defines the synchronous dataflow MoC.
--
-- * "ForSyDe.Atom.MoC.SDF.SADF" defines the scenario-aware dataflow MoC.
--
-- * the <ForSyDe-Atom.html#naming_conv naming convention> rules on how to interpret
--   the function names based on their number of inputs and outputs.
----------------------------------------------------------------------

module ForSyDe.Atom.MoC.SDF.CSDF (

  -- * Re-Exported from SDF

  -- | These constructors and utilities are re-exported from "ForSyDe.Atom.MoC.SDF"
  -- for convenience.
  Signal, Prod, Cons, signal, delay,

  -- * CSDF actors
  
  actor11, actor12, actor13, actor14,
  actor21, actor22, actor23, actor24,
  actor31, actor32, actor33, actor34,
  actor41, actor42, actor43, actor44,
  ) where

import ForSyDe.Atom.MoC.SDF.SADF
import ForSyDe.Atom.MoC.SDF.Core
import ForSyDe.Atom.MoC.SDF.SDF (delay)
import ForSyDe.Atom.MoC hiding (delay)

actor11 :: [(Cons, Prod, [a1] -> [b1])] 
        -> Signal a1
        -> Signal b1
actor21 :: [((Cons, Cons), Prod, [a1] -> [a2] -> [b1])]
        -> Signal a1 -> Signal a2
        -> Signal b1
actor31 :: [((Cons, Cons, Cons), Prod, [a1] -> [a2] -> [a3] -> [b1])]
        -> Signal a1 -> Signal a2 -> Signal a3
        -> Signal b1
actor41 :: [((Cons, Cons, Cons, Cons), Prod,
              [a1] -> [a2] -> [a3] -> [a4] -> [b1])]
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
        -> Signal b1
actor12 :: [(Cons, (Prod, Prod), [a1] -> ([b1], [b2]))]
        -> Signal a1
        -> (Signal b1, Signal b2)
-- | A CSDF actor is very similar to a SDF actor, but instead of only one set of
-- execution parameter (production rate, consumption rate, function), it takes a
-- finite list of such parameters, and cycles through them with each firing. 
--
-- Constructors: @actor[1-4][1-4]@.
-- 
-- >>> let s1 = signal [1..10]
-- >>> let f a = [sum a]
-- >>> actor11 [(3,1,f),(2,1,f)] s1
-- {6,9,21,19}
--
-- <<fig/moc-ddf-csdf-actor.png>>
actor22 :: [((Cons, Cons), (Prod, Prod),
                    [a1] -> [a2] -> ([b1], [b2]))]
         -- ^ control signal
         -> Signal a1 -- ^ data signal
         -> Signal a2 -- ^ data signal 
         -> (Signal b1, Signal b2)
         -- ^ output data signals
actor32 :: [((Cons, Cons, Cons), (Prod, Prod),
                    [a1] -> [a2] -> [a3] -> ([b1], [b2]))]
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2)
actor42 :: [((Cons, Cons, Cons, Cons), (Prod, Prod),
                    [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))]
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2)
actor13 :: [(Cons, (Prod, Prod, Prod), [a1] -> ([b1], [b2], [b3]))]
         -> Signal a1
         -> (Signal b1, Signal b2, Signal b3)
actor23 :: [((Cons, Cons), (Prod, Prod, Prod),
                    [a1] -> [a2] -> ([b1], [b2], [b3]))]
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3)
actor33 :: [((Cons, Cons, Cons), (Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))]
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3)
actor43 :: [((Cons, Cons, Cons, Cons), (Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))]
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3)
actor14 :: [(Cons, (Prod, Prod, Prod, Prod),
                    [a1] -> ([b1], [c], [d], [e]))]
         -> Signal a1
         -> (Signal b1, Signal c, Signal d, Signal e)
actor24 :: [((Cons, Cons), (Prod, Prod, Prod, Prod),
                    [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))]
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
actor34 :: [((Cons, Cons, Cons), (Prod, Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))]
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
actor44 :: [((Cons, Cons, Cons, Cons), (Prod, Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))]
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3, Signal b4)

scenSelect fs = detector11 0 ns od ([],fs)
  where ns ([],l)   _ = (l,l)
        ns (_:[],l) _ = (l,l)
        ns (_:fs,l) _ = (fs,l)
        od =  (,) 1 . (:[]) . head . fst

actor11 fs s1 = kernel11 (scenSelect fs s1) s1
actor21 fs s1 = kernel21 (scenSelect fs s1) s1
actor31 fs s1 = kernel31 (scenSelect fs s1) s1
actor41 fs s1 = kernel41 (scenSelect fs s1) s1
actor12 fs s1 = kernel12 (scenSelect fs s1) s1
actor22 fs s1 = kernel22 (scenSelect fs s1) s1
actor32 fs s1 = kernel32 (scenSelect fs s1) s1
actor42 fs s1 = kernel42 (scenSelect fs s1) s1
actor13 fs s1 = kernel13 (scenSelect fs s1) s1
actor23 fs s1 = kernel23 (scenSelect fs s1) s1
actor33 fs s1 = kernel33 (scenSelect fs s1) s1
actor43 fs s1 = kernel43 (scenSelect fs s1) s1
actor14 fs s1 = kernel14 (scenSelect fs s1) s1
actor24 fs s1 = kernel24 (scenSelect fs s1) s1
actor34 fs s1 = kernel34 (scenSelect fs s1) s1
actor44 fs s1 = kernel44 (scenSelect fs s1) s1
