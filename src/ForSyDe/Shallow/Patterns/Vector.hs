{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies,MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Core.Vector
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the data type 'Vector' and the
-- corresponding functions. It is a development of the module
-- defined by Reekie.  Though the vector is modeled as a list, it
-- should be viewed as an array, i.e. a vector has a fixed
-- size. Unfortunately, it is not possible to have the size of the
-- vector as a parameter of the vector data type, due to restrictions
-- in Haskells type system. Still most operations are defined for
-- vectors with the same size.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Patterns.Vector where

import ForSyDe.Shallow.Core

import qualified ForSyDe.Shallow.MoC.SY as SY

infixl 4 §>, <§>

-- | provides 'fmap'
instance Functor Vector where
  fmap _ NullV   = NullV
  fmap f (x:>xs) = f x :> fmap f xs

-- | provides 'pure', '<*>', '<$>'
instance Applicative Vector where
  pure x = x :> NullV
  _         <*> NullV     = NullV
  NullV     <*> _         = NullV
  (f :> fs) <*> (x :> xs) = f x :> fs <*> xs

(<§>) :: Vector (a -> b) -> Vector a -> Vector b 
(<§>) = (<*>)

(§>) :: (a -> b) -> Vector a -> Vector b 
(§>) = (<$>)

-- | The 'pipe' function constructs a serial composition from a vector of functions.
--
-- /OBS/: composition is right associative thus the input vector should contain functions from right to left
pipeV         :: Vector (b -> b) -> b -> b
pipeV NullV   = id
pipeV (v:>vs) = v . pipeV vs 

-- | The 'scan' function constructs a parallel prefix vector from a vector of functions.
--
-- /OBS/: composition is right associative thus the input vector should contain functions from right to left
scanV         :: Vector (b -> b) -> b -> Vector b
scanV NullV   = pure NullV  
scanV (x:>xs) = (:>) <$> x . pipeV xs <*> scanV xs

-- | 'zipxPN' transforms a vector of signals into a signal of vectors.
zipxPN :: (Signals s) => Vector (s a) -> s (Vector a)
zipxPN NullV = signal $ repeat NullV
zipxPN (x:>xs) =  (:>) §- x -§- zipxPN xs

class Signals s => UnzippableS s where
  unzipxPN :: s (Vector a) -> Vector (s a)

instance UnzippableS SY.SignalSY where
  unzipxPN SY.NullS     = vector $ repeat SY.NullS
  unzipxPN (x SY.:- xs) = (SY.:-) §> x <§> unzipxPN xs

class VSig a
instance {-# OVERLAPS #-} UnzippableS s => VSig (s a)
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (s a))
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (Vector (s a)))
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (Vector (Vector (s a))))
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (Vector (Vector (Vector (s a)))))
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (Vector (Vector (Vector (Vector (s a))))))
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a))))))))
instance {-# OVERLAPS #-} UnzippableS s => VSig (Vector (Vector (Vector (Vector (Vector (Vector (Vector (Vector (s a)))))))))

