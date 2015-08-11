{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide, prune, show-extensions #-}
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

module ForSyDe.Core.Signals where

import ForSyDe.Core.Vector
import ForSyDe.Core.AbsentExt


infixl 4 §§-, -§§-, ->-

-- | The 'Signal' type class describes the characteristics and behavior of 
--   'ForSyDe.Signals'-bound signals. 
class Signals (s :: * -> *) where

  -- | The type of the token carried by a signal
  type TokT s a

  -- | functor operator for signals. It bypasses the token wrapper, if any.
  (§§-) :: (a -> b) -> s a -> s b

  -- | lift operator for signals. It bypasses the token wrapper, if any.
  (-§§-) :: s (a -> b) -> s a -> s b

  -- | operator for the default delay function
  (->-) :: s a -> TokT s a -> s a

  -- | function for filtering signals. The output data type is defined by the 'TokenType' type synonym
  filt :: (a -> Bool) -> s a -> s a 
 
  -- | transpose function which has to respect the semantics and the form of each MoC-bound signal.
  zipx :: Vector (s a) -> s (Vector (AbstExt a))

  -- | inverse transpose function which has to respect the semantics and the form of each MoC-bound signal.
  unzipx :: s (Vector (AbstExt a)) -> Vector (s a)

  -- | helper function for safely turning a signal into the type adopted by the transposed form. Used for convenience.
  safe :: s (Vector a) -> s (Vector (AbstExt a)) 
  safe = ((§§-) . (§>)) Prst 
  
-- | Due to Haskell's type system, a generic class for describing nested (composed) types is 
--   not possible. Thus in order to force the designer into using only signals as base,
--   multiple instances of nested vectors are overlapped. The maximum nested depth is 8. 
--   
--   @VecSig a@ could be translated to: either a 'Signal' or a 'Signal' wrapped inside an 
--   "arbitrary" number of 'Vector's.
class VecSig a
instance {-# OVERLAPS #-} Signals s => VecSig (s a)
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (s a))
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (Vector (s a)))
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (Vector (Vector (s a))))
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (Vector (Vector (Vector (s a)))))
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (Vector (Vector (Vector (Vector (s a))))))
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a))))))))
instance {-# OVERLAPS #-} Signals s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))))

