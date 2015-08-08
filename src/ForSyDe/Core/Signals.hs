{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
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

module ForSyDe.Core.Signals where

import ForSyDe.Core.Vector
import ForSyDe.Core.AbsentExt


infixl 4 §§-, -§§-, ->-

-- | The 'Signal' type class describes the characteristics and behavior of 
--   'ForSyDe.Signals'-bound signals. 
class Signals (s :: * -> *) where

  -- | The data type associated with a filtered output.
  type TokT s a

  -- | operator for functional application on signals
  (§§-) :: (a -> b) -> s a -> s b

  -- | operator for zipping signals
  (-§§-) :: s (a -> b) -> s a -> s b

  -- | operator for the default delay function
  (->-) :: s a -> TokT s a -> s a

  -- | operator for filtering signals. The output data type is defined by the 'TokenType' type synonym
  filt :: (a -> Bool) -> s a -> s a 
 
  zipx :: Vector (s a) -> s (Vector (AbstExt a))

  unzipx :: s (Vector (AbstExt a)) -> Vector (s a)

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

