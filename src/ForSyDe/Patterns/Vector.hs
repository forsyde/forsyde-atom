{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Vector
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the extensions for the 'Vector' data type in order
-- to enable the exploitation of algorithmic skeleton-based patterns in
-- ForSyDe process networks.
-----------------------------------------------------------------------------
module ForSyDe.Patterns.Vector(
  -- ** Basic operators
  (§>), (<§>), pipeV, scanV,
  -- ** Nested vectors of signals
  VecSig(..)
) where

import ForSyDe.Core
import ForSyDe.MoC.Signal

infixl 4 §>, <§>

instance Functor Vector where
  fmap _ NullV   = NullV
  fmap f (x:>xs) = f x :> fmap f xs

instance Applicative Vector where
  pure x = x :> NullV
  _         <*> NullV     = NullV
  NullV     <*> _         = NullV
  (f :> fs) <*> (x :> xs) = f x :> fs <*> xs


-- | operator for functional application on vectors
(§>) :: (a -> b) -> Vector a -> Vector b 
(§>) = (<$>)

-- | operator for zip-applying (zapp) two vectors
(<§>) :: Vector (a -> b) -> Vector a -> Vector b 
(<§>) = (<*>)


-- | The 'pipe' function constructs a serial composition from a vector of functions.
--
-- __OBS__: composition is right associative thus the input vector should contain functions from right to left
pipeV         :: Vector (b -> b) -> b -> b
pipeV NullV   = id
pipeV (v:>vs) = v . pipeV vs 

-- | The 'scan' function constructs a parallel prefix vector from a vector of functions.
--
-- __OBS__: composition is right associative thus the input vector should contain functions from right to left
scanV         :: Vector (b -> b) -> b -> Vector b
scanV NullV   = pure NullV  
scanV (x:>xs) = (:>) <$> x . pipeV xs <*> scanV xs

-- | Due to Haskell's type system, a generic class for describing nested (composed) types is 
--   not possible. Thus in order to force the designer into using only signals as base,
--   multiple instances of nested vectors are overlapped. The maximum nested depth is 8. 
--   
--   @VecSig a@ could be translated to: either a 'Signal' or a 'Signal' wrapped inside an 
--   "arbitrary" number of 'Vector's.
class VecSig a
instance {-# OVERLAPS #-} Signal s => VecSig (s a)
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (s a))
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (Vector (s a)))
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (Vector (Vector (s a))))
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (Vector (Vector (Vector (s a)))))
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (Vector (Vector (Vector (Vector (s a))))))
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a))))))))
instance {-# OVERLAPS #-} Signal s => VecSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))))

