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
module ForSyDe.Core.ValueExt where

import ForSyDe.Core.Vector

------------------------------------------------------------------

-- |The data type 'Value' has three constructors. The constructor 'Abst' is used to model the absence of a value, while the constructor 'Prst' is used to model present values. The third one 'Undef' is used to model an undefined value, the equivalent of \'anything\' in VHDL.
data Value a =  Abst | Undef | Prst a deriving (Eq)

-- | Instance of 'Show'. \'_\' represents the value 'Abst' while a present value is represented with its value.
instance Show a => Show (Value a) where
 showsPrec _ x = showsValue x
   where showsValue Abst     = (++) "\10178"       
         showsValue Undef    = (++) "?"
         showsValue (Prst x) = (++) (show x)

-- | Instance of 'Read'. \'_\' represents the value 'Abst' while a present value is represented with its value.
instance Read a => Read (Value a) where
  readsPrec _ x       = readsValue x 
   where readsValue s = [(Abst, r1) | ("_", r1) <- lex s]
           ++ [(Undef, r2) | ("?", r2) <- lex s]
           ++ [(Prst x, r3) | (x, r3) <- reads s]


instance Functor Value where
  fmap _ Abst     = Abst
  fmap _ Undef    = Undef
  fmap f (Prst x) = Prst (f x)


instance Applicative Value where
  pure   = Prst 
  (Prst x) <*> (Prst y) = Prst (x y)
  Undef    <*> (Prst _) = Undef
  (Prst _) <*> Undef    = Undef
  Undef    <*> Undef    = Undef
  Abst     <*> Abst     = Abst
  _        <*> _        = error "Undecidable occurrence of an absent and present"  

-- |The function 'fromValue' converts a value from a extended value.
fromValue       :: a -> Value a -> a
-- |The function 'fromValue' converts a value from a extended value.
unsafeFromValue ::  Value a -> a
-- |The functions 'isPresent' checks for the presence of a value.
isPresent         :: Value a -> Bool
-- |The functions 'isAbsent' checks for the absence of a value.
isAbsent          :: Value a -> Bool
-- |The functions 'isUndefined' checks if the value is defined.
isUndefined       :: Value a -> Bool
-- | The function 'abstExt' converts a usual value to a present value. 
value           :: a -> Value a  

value                    = pure
fromValue x Abst         = x   
fromValue _ (Prst y)     = y
unsafeFromValue (Prst y) = y
unsafeFromValue Abst     = error "Should not be absent or undefined"
isAbsent Abst            = True
isAbsent _               = False
isUndefined Undef        = True
isUndefined _            = False
isPresent (Prst _)       = True
isPresent _              = False

-- | replaces one wrapped value with another one based on a wrapped boolean predicate 
predicate :: Value a    -- ^ value to replace with
          -> Value Bool -- ^ wrapped boolean predicate
          -> Value a    -- ^ value to replace
          -> Value a    -- ^ result
predicate t p x = if p == Prst True then x else t

infixl 5 >¤
-- | serial storage infix operator. Stores only present values
(>¤) :: Value (Vector a) -- ^ input storing buffer wrapped as value
     -> Value a          -- ^ value to be storred
     -> Value (Vector a) -- ^ output buffer
buff >¤ Abst   = buff
buff >¤ Undef  = buff
buff >¤ Prst a = (:>) <$> Prst a <*> buff

------------------------------------------------------------------------------
