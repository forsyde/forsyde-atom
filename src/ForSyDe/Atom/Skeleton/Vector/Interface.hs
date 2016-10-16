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
import ForSyDe.Atom.MoC
import ForSyDe.Atom.Signal
import ForSyDe.Atom.Skeleton.Vector.Core
import ForSyDe.Atom.Skeleton.Vector.Lib

import Prelude hiding (tail, map, length)


-- TODO: Not fully correct, since <{1,2},{3,_}> would become {<1,3>, _}.
-- This is because the MoC class explicitly asks for `Value`s. This might
-- be fixable once the Behavior Layer is described as a type class
zipx   w21 w11 = red catEv . map unitEv
  where catEv  = (comb21 . w21 . psi21) (<++>)
        unitEv = (comb11 . w11 . psi11) unit

-- TODO: not fully correct. See zipx. Extremely unsafe even!!!
unzipx w1 w2 sv = (map getFst . scan' selFun) sv
  where getFst = (comb11 . w1 . psi11) first
        selFun = fanoutn n ((comb11 . w2 . psi11) tail)
        n      = (length . unsafeFromValue . head . sniff . headS) sv - 1 
