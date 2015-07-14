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
module ForSyDe.Core.AbsentExt( 
         AbstExt (Abst, Prst), fromAbstExt, abstExt, psi, 
         isAbsent, isPresent, abstExtFunc
       ) where

-- |The data type 'AbstExt' has two constructors. The constructor 'Abst' is used to model the absence of a value, while the constructor 'Prst' is used to model present values.
data AbstExt a =  Abst | Prst a deriving (Eq)

-- | Instance of 'Show'. \'_\' represents the value 'Abst' while a present value is represented with its value.
instance Show a => Show (AbstExt a) where
 showsPrec _ x = showsAbstExt x
   where showsAbstExt Abst     = (++) "_"       
         showsAbstExt (Prst x) = (++) (show x)

-- | Instance of 'Read'. \'_\' represents the value 'Abst' while a present value is represented with its value.
instance Read a => Read (AbstExt a) where
  readsPrec _ x       =  readsAbstExt x 
   where readsAbstExt s =  [(Abst, r1) | ("_", r1) <- lex s] ++ [(Prst x, r2) | (x, r2) <- reads s]


-- |The function 'fromAbstExt' converts a value from a extended value.
fromAbstExt       :: a -> AbstExt a -> a
-- |The functions 'isPresent' checks for the presence of a value.
isPresent       :: AbstExt a -> Bool
-- |The functions 'isAbsent' checks for the absence of a value.
isAbsent       :: AbstExt a -> Bool
-- |The function 'abstExtFunc' extends a function in order to process absent extended values. If the input is (\"bottom\"), the output will also be  (\"bottom\").
abstExtFunc       :: (a -> b) -> AbstExt a -> AbstExt b
-- | The function 'psi' is identical to 'abstExtFunc' and should be used in future.
psi :: (a -> b) -> AbstExt a -> AbstExt b
-- | The function 'abstExt' converts a usual value to a present value. 
abstExt :: a -> AbstExt a


abstExt v              =  Prst v
fromAbstExt x Abst     =  x   
fromAbstExt _ (Prst y) =  y   
isPresent Abst         =  False
isPresent (Prst _)     =  True
isAbsent               =  not . isPresent
psi                    = abstExtFunc
abstExtFunc f          = f' 
  where f' Abst     = Abst
        f' (Prst x) = Prst (f x)

