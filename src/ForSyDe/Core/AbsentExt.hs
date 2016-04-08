{-# OPTIONS_HADDOCK hide #-}
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
-- The 'AbstExt' is used to extend existing data types with the value
--  \'absent\', which models the absence of a value.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Core.AbsentExt where

-- |The data type 'AbstExt' has two constructors. The constructor 'Abst' is used to model the absence of a value, while the constructor 'Prst' is used to model present values.
data AbstExt a =  Abst | Prst a deriving (Eq)

-- | Instance of 'Show'. \'_\' represents the value 'Abst' while a present value is represented with its value.
instance Show a => Show (AbstExt a) where
 showsPrec _ x = showsAbstExt x
   where showsAbstExt Abst     = (++) "\10178"       
         showsAbstExt (Prst x) = (++) (show x)

-- | Instance of 'Read'. \'_\' represents the value 'Abst' while a present value is represented with its value.
instance Read a => Read (AbstExt a) where
  readsPrec _ x       =  readsAbstExt x 
   where readsAbstExt s =  [(Abst, r1) | ("_", r1) <- lex s] ++ [(Prst x, r2) | (x, r2) <- reads s]


instance Functor AbstExt where
  fmap _ Abst     = Abst
  fmap f (Prst x) = Prst (f x)


instance Applicative AbstExt where
  pure a  = Prst a
  Abst     <*> Abst     = Abst
  (Prst x) <*> (Prst y) = Prst (x y)
  _        <*> _        = error "Undecidable occurrence of an absent and present"


-- |The function 'fromAbstExt' converts a value from a extended value.
fromAbstExt       :: a -> AbstExt a -> a
-- |The function 'fromAbstExt' converts a value from a extended value.
unsafeFromAbstExt ::  AbstExt a -> a
-- |The functions 'isPresent' checks for the presence of a value.
isPresent         :: AbstExt a -> Bool
-- |The functions 'isAbsent' checks for the absence of a value.
isAbsent          :: AbstExt a -> Bool
-- | The function 'abstExt' converts a usual value to a present value. 
abstExt           :: a -> AbstExt a  

abstExt v                  = Prst v
fromAbstExt x Abst         = x   
fromAbstExt _ (Prst y)     = y
unsafeFromAbstExt (Prst y) = y
unsafeFromAbstExt Abst     = error "Should not be absent"
isPresent Abst             = False
isPresent (Prst _)         = True
isAbsent                   = not . isPresent

