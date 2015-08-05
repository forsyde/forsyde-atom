{-# LANGUAGE TypeFamilies, FlexibleInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Stream' datatype and
-- functions operating on it.
-----------------------------------------------------------------------------

module ForSyDe.Core.Signal (
  MoC (..),
  -- ** Nested vectors of signals
  VecSig(..),
) where

import ForSyDe.Core.Stream
import ForSyDe.Core.Vector


infixl 4 ->-
infixl 4 -#-


-- | The 'Signal' type class describes the characteristics and behavior of 
--   'ForSyDe.MoC'-bound signals. 
class (Applicative s) => MoC s where

  -- | The data type associated with a filtered output.
  type TokenType s a

  -- | The data type associated with a filtered output.
  type Padded s a

  -- | The function 'toS' converts a MoC-bound signal to a stream of events (its base type)
  --toS :: s a -> Stream a

  -- | The function 'fromS' converts a (base) stream of events to a MoC-bound signal
 -- fromS :: Stream a -> s a

  --liftS :: (Stream a -> Stream b) -> s a -> s b

  -- | operator for the default delay function
  (->-) :: s (TokenType s a) -> TokenType s a -> s (TokenType s a) 

  -- | operator for filtering signals. The output data type is defined by the 'TokenType' type synonym
  (-#-) :: s (TokenType s a) -> (a -> Bool) -> s (TokenType s a) 

  ----------------------------------------
  --liftS f = fromS . f . toS
  --xs ->- x  = liftS (x :-) xs


-- | Due to Haskell's type system, a generic class for describing nested (composed) types is 
--   not possible. Thus in order to force the designer into using only signals as base,
--   multiple instances of nested vectors are overlapped. The maximum nested depth is 8. 
--   
--   @VecSig a@ could be translated to: either a 'Signal' or a 'Signal' wrapped inside an 
--   "arbitrary" number of 'Vector's.
class VecSig a
instance {-# OVERLAPS #-} MoC s => VecSig (s a)
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (s a))
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (Vector (s a)))
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (Vector (Vector (s a))))
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (Vector (Vector (Vector (s a)))))
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (Vector (Vector (Vector (Vector (s a))))))
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a))))))))
instance {-# OVERLAPS #-} MoC s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))))




