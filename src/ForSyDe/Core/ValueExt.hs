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
-- The 'AExt' is used to extend existing data types with the value
--  \'absent\', which models the absence of a value.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Core.ValueExtensions where

import ForSyDe.Core.Vector

-----------------------------------------------------------------------------

infixl 5 #
class Predicate t where
  (#) :: t Bool -> t a -> t a

infixl 5 >¤
class Store t where
  (>¤) ::  t (Vector a) -> t a -> t (Vector a)

-----------------------------------------------------------------------------

-- |The data type 'AExt' has two constructors. The constructor 'A' is used to model the absence of an event, while the constructor 'P' is used to model present values.
data AExt a =  A | P a deriving (Eq)

-- | Instance of 'Show'. \'_\' represents the value 'A' while a present value is represented with its value.
instance Show a => Show (AExt a) where
 showsPrec _ x = showsAExt x
   where showsAExt A     = (++) "\10178"       
         showsAExt (P x) = (++) (show x)

-- | Instance of 'Read'. \'_\' represents the value 'A' while a present value is represented with its value.
instance Read a => Read (AExt a) where
  readsPrec _ x       =  readsAExt x 
   where readsAExt s =  [(A, r1) | ("_", r1) <- lex s] ++ [(P x, r2) | (x, r2) <- reads s]


instance Functor AExt where
  fmap _ A     = A
  fmap f (P x) = P (f x)


instance Applicative AExt where
  pure a  = P a
  A     <*> A     = A
  (P x) <*> (P y) = P (x y)
  _        <*> _        = error "Undecidable occurrence of an absent and present"


-- |The function 'fromAExt' converts a value from a extended value.
fromAExt       :: a -> AExt a -> a
-- |The function 'fromAExt' converts a value from a extended value.
unsafeFromAExt ::  AExt a -> a
-- |The functions 'isPresent' checks for the presence of a value.
isPresent         :: AExt a -> Bool
-- |The functions 'isAbsent' checks for the absence of a value.
isAbsent          :: AExt a -> Bool
-- | The function 'AExt' converts a usual value to a present value. 
AExt           :: a -> AExt a  

AExt v                  = P v
fromAExt x A         = x   
fromAExt _ (P y)     = y
unsafeFromAExt (P y) = y
unsafeFromAExt A     = error "Should not be absent"
isPresent A             = False
isPresent (P _)         = True
isAbsent                   = not . isPresent



-----------------------------------------------------------------------------
-- |The data type 'UExt' has two constructors. The constructor 'U' is used to model an unknown value (similar to \'anything\' in VHDL), while the constructor 'D' is used to model defined values.
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

instance Predicate UExt where
  c # a = if c == D True then a else U

instance Store UExt where
  buff >¤ U   = buff
  buff >¤ D a = (:>) <$> D a <*> buff

isdefined U = False
isdefined _ = True  

------------------------------------------------------------------------------
