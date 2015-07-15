-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable 
-- 
-- Models of Computation in ForSyDe define the semantics of the computation
-- and communication within process networks. In order to describe these
-- semantics with respect to the applicative implementation style, the 
-- signal conduits associated with each MoC are instances of the 'Signal'
-- type class which provides basic methods for altering the events.
--
-- The MoCs defined in the curent implementation of ForSyDe-Haskell are:
--
--  * 'ForSyDe.MoC.SY.SY' (the synchronous model of computation)
-----------------------------------------------------------------------------

module ForSyDe.MoC(  
         module ForSyDe.MoC.Signal,
       ) where


import ForSyDe.MoC.Signal


