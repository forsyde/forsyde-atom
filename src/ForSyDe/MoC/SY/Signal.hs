{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015; 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the sychronous Signal. The functions necessary for
-- the standard ForSyDe framework are re-exported by the parent module 
-- 'ForSyDe.MoC.SY.SY'. 
--
-- This module should be separately imported only for experimental purpose,
-- and should not be included in a standard ForSyDe design- 
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY.Signal where

import ForSyDe.Core

infixl 4 §-, -§-
-- | this is the basic functor operator used in process constructors. It takes a function on a token and applies it on a 'Signal'. It returns a 'Stream' (the key for constructing functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(§-) :: (a -> b) -> Signal a -> Signal b
(§-) = (<$>) 

-- | this is the basic lifting operator used in process constructors. It inputs a stream of functions on a tokens and applies it on a 'Signal'. It returns a 'Stream' (the key for constructing functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(-§-) :: Signal (a -> b) -> Signal a -> Signal b
(-§-) = (<*>)

-- | turns the result of the applicative operators §- and -§- back into 'Signal's.
tokenize = id

-- | operator for the default delay function
(->-) :: Signal a -> a -> Signal a
xs ->- i = i :- xs



