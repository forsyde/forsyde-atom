{-# OPTIONS_HADDOCK hide, show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.Interface
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE.Hybrid where

import           ForSyDe.Atom.MoC.Atom
-- import qualified ForSyDe.Atom.MoC.DE.Core as DE
import qualified ForSyDe.Atom.MoC.DE.Interface as DE
-- import qualified ForSyDe.Atom.MoC.SY.Core as SY
import qualified ForSyDe.Atom.MoC.SY.Interface as SY
import           ForSyDe.Atom.Utility




-- stateMachine11 :: (SY.Sig a1                                        -> SY.Sig b1)
--                 -> DE.Sig a1                                        -> DE.Sig b1
-- stateMachine21 :: (SY.Sig a1 -> SY.Sig a2                           -> SY.Sig b1)
--                 -> DE.Sig a1 -> DE.Sig a2                           -> DE.Sig b1
-- stateMachine31 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3              -> SY.Sig b1)
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3              -> DE.Sig b1
-- stateMachine41 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3 -> SY.Sig a4 -> SY.Sig b1)
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3 -> DE.Sig a4 -> DE.Sig b1
-- stateMachine12 :: (SY.Sig a1                                        -> (SY.Sig b1, SY.Sig b2))
--                 -> DE.Sig a1                                        -> (DE.Sig b1, DE.Sig b2)
-- stateMachine22 :: (SY.Sig a1 -> SY.Sig a2                           -> (SY.Sig b1, SY.Sig b2))
--                 -> DE.Sig a1 -> DE.Sig a2                           -> (DE.Sig b1, DE.Sig b2)
-- stateMachine32 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3              -> (SY.Sig b1, SY.Sig b2))
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3              -> (DE.Sig b1, DE.Sig b2)
-- stateMachine42 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3 -> SY.Sig a4 -> (SY.Sig b1, SY.Sig b2))
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3 -> DE.Sig a4 -> (DE.Sig b1, DE.Sig b2)
-- stateMachine13 :: (SY.Sig a1                                        -> (SY.Sig b1, SY.Sig b2, SY.Sig b3))
--                 -> DE.Sig a1                                        -> (DE.Sig b1, DE.Sig b2, DE.Sig b3)
-- stateMachine23 :: (SY.Sig a1 -> SY.Sig a2                           -> (SY.Sig b1, SY.Sig b2, SY.Sig b3))
--                 -> DE.Sig a1 -> DE.Sig a2                           -> (DE.Sig b1, DE.Sig b2, DE.Sig b3)
-- stateMachine33 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3              -> (SY.Sig b1, SY.Sig b2, SY.Sig b3))
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3              -> (DE.Sig b1, DE.Sig b2, DE.Sig b3)
-- stateMachine43 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3 -> SY.Sig a4 -> (SY.Sig b1, SY.Sig b2, SY.Sig b3))
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3 -> DE.Sig a4 -> (DE.Sig b1, DE.Sig b2, DE.Sig b3)
-- stateMachine14 :: (SY.Sig a1                                        -> (SY.Sig b1, SY.Sig b2, SY.Sig b3, SY.Sig b4))
--                 -> DE.Sig a1                                        -> (DE.Sig b1, DE.Sig b2, DE.Sig b3, DE.Sig b4)
-- stateMachine24 :: (SY.Sig a1 -> SY.Sig a2                           -> (SY.Sig b1, SY.Sig b2, SY.Sig b3, SY.Sig b4))
--                 -> DE.Sig a1 -> DE.Sig a2                           -> (DE.Sig b1, DE.Sig b2, DE.Sig b3, DE.Sig b4)
-- stateMachine34 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3              -> (SY.Sig b1, SY.Sig b2, SY.Sig b3, SY.Sig b4))
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3              -> (DE.Sig b1, DE.Sig b2, DE.Sig b3, DE.Sig b4)
-- stateMachine44 :: (SY.Sig a1 -> SY.Sig a2 -> SY.Sig a3 -> SY.Sig a4 -> (SY.Sig b1, SY.Sig b2, SY.Sig b3, SY.Sig b4))
--                 -> DE.Sig a1 -> DE.Sig a2 -> DE.Sig a3 -> DE.Sig a4 -> (DE.Sig b1, DE.Sig b2, DE.Sig b3, DE.Sig b4)



stateMachine11 syproc de1 = let (ts, sy1) = DE.toSY de1
                            in  SY.toDE ts $     syproc sy1 
stateMachine12 syproc de1 = let (ts, sy1) = DE.toSY de1
                            in  SY.toDE2 ts <>   syproc sy1
stateMachine13 syproc de1 = let (ts, sy1) = DE.toSY de1
                            in  SY.toDE3 ts <>>  syproc sy1
stateMachine14 syproc de1 = let (ts, sy1) = DE.toSY de1
                            in  SY.toDE4 ts <>>> syproc sy1

stateMachine21 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                                in  SY.toDE ts $     syproc sy1 sy2
stateMachine22 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                                in  SY.toDE2 ts <>   syproc sy1 sy2
stateMachine23 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                                in  SY.toDE3 ts <>>  syproc sy1 sy2
stateMachine24 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                                in  SY.toDE4 ts <>>> syproc sy1 sy2

stateMachine31 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                                    in  SY.toDE ts $     syproc sy1 sy2 sy3
stateMachine32 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                                    in  SY.toDE2 ts <>   syproc sy1 sy2 sy3
stateMachine33 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                                    in  SY.toDE3 ts <>>  syproc sy1 sy2 sy3
stateMachine34 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                                    in  SY.toDE4 ts <>>> syproc sy1 sy2 sy3

stateMachine41 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                        in  SY.toDE ts $     syproc sy1 sy2 sy3 sy4
stateMachine42 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                        in  SY.toDE2 ts <>   syproc sy1 sy2 sy3 sy4
stateMachine43 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                        in  SY.toDE3 ts <>>  syproc sy1 sy2 sy3 sy4
stateMachine44 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                        in  SY.toDE4 ts <>>> syproc sy1 sy2 sy3 sy4
