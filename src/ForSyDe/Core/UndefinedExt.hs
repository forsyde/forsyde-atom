-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.AbsentExt
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'UndefinedExt' is used to extend existing data types with the value
--  \'undefined\', which models an unkown or undetermined value of a value.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Core.UndefinedExt where


-----------------------------------------------------------------------------
-- ARGUMENT DATA TYPE - ABSENT EXTENDED
-----------------------------------------------------------------------------

data UExt a =  U | D a deriving (Eq)


instance Show a => Show (UExt a) where
  showsPrec _ = showsAE
    where showsAE U     = (++) "?"       
          showsAE (D x) = (++) (show x)

instance Read a => Read (UExt a) where
  readsPrec _       =  readsAE 
    where readsAE s = [(U, r1) | ("?", r1) <- lex s] ++ [(D x, r2) | (x, r2) <- reads s]

instance Functor UExt where
  fmap _ U     = U
  fmap f (D x) = D (f x)

instance Applicative UExt where
  pure  = D
  _      <*> U   = U
  U   <*> _      = U
  (D x) <*> (D y) = D (x y)
