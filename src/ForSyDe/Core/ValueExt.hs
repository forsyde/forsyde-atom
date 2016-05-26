{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.AbsentExt
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016;
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- 
-----------------------------------------------------------------------------
module ForSyDe.Core.ValueExt where

import ForSyDe.Core.Vector

-- |The data type 'Value' wraps arbtrary types to extend them with
-- tokens carrying special behavioural semantics, as defined by
-- ForSyDe
data Value a = Abst   -- ^ denotes an absent event
             | Undef  -- ^ denotes an undefined value
             | Value a -- ^ a wrapped "present" event with a "defined"
                       -- value
             deriving (Eq)

-- | Shows 'Abst' as \'&#8869;\', 'Undef' as \'?\' while a wrapped
-- value is represented with its value.
instance Show a => Show (Value a) where
 showsPrec _ x = showsValue x
   where showsValue Abst     = (++) "\10178"       
         showsValue Undef    = (++) "?"
         showsValue (Value x) = (++) (show x)

-- | Reads the \'_\' character to an 'Abst', a \'?\' to an 'Undef',
-- and a normal value to wrapped one.
instance Read a => Read (Value a) where
  readsPrec _ x       = readsValue x 
   where readsValue s = [(Abst, r1) | ("_", r1) <- lex s]
           ++ [(Undef, r2) | ("?", r2) <- lex s]
           ++ [(Value x, r3) | (x, r3) <- reads s]

-- | 'Functor' instance. Bypasses the special values and maps a
-- function to the wrapped value. Provides the @(\<$\>)@ operator.
instance Functor Value where
  fmap _ Abst      = Abst
  fmap _ Undef     = Undef
  fmap f (Value x) = Value (f x)

-- | 'Applicative' instance. Check source code for the lifting
-- rules. Provides the @(\<*\>)@ operator.
instance Applicative Value where
  pure = Value 
  (Value x) <*> (Value y) = Value (x y)
  Undef     <*> (Value _) = Undef
  (Value _) <*> Undef     = Undef
  Undef     <*> Undef     = Undef
  Abst      <*> Abst      = Abst
  _         <*> _         = error "Illegal occurrence of an absent and present event"

-----------------------------------------------------------------------------
-- Behavioural implementations
-----------------------------------------------------------------------------

-- | The equivalent of the 'fmap' function or the @(\<$\>)@ infix
-- operator for extended values. It bypasses the special values and
-- maps a function @f@ to a wrapped value.
--
-- >>> let (a,u,v)=(Abst,Undef,Value 1)
-- >>> (a,u,v)
-- (⟂,?,1)
-- >>> let f = (+1)
-- >>> (mapValue f a, mapValue f u, mapValue f v)
-- (⟂,?,2)
-- >>> (f <$> a, f <$> u, f <$> v)
-- (⟂,?,2)
mapValue :: (a -> b) -> Value a -> Value b
mapValue = fmap

-- | The equivalent of the applicative operator @(\<*\>)@ for extended
-- values. Check the source source code for the Applicative instance
-- to see the lifting rules.
--
-- >>> let (a,u,v)=(Abst,Undef,Value 1)
-- >>> (a,u,v)
-- (⟂,?,1)
-- >>> let f = (+1)
-- >>> liftValue (Value (+1)) v
 -- 2
-- >>> liftValue (Value (+1)) u
-- ?
-- >>> liftValue (Value (+1)) a
-- *** Exception: Illegal occurrence of an absent and present event
-- >>> -- A more interesting application is when we combine <$> and <*>
-- >>> (+) <$> v <*> v
-- 2
-- >>> (+) <$> v <*> u
-- ?
-- >>> (+) <$> a <*> v
-- *** Exception: Illegal occurrence of an absent and present event
liftValue :: Value (a -> b) -> Value a -> Value b
liftValue = (<*>)

-- | replaces one wrapped value with another one based on a wrapped boolean predicate 
predicate :: Value a    -- ^ value to replace with
          -> Value Bool -- ^ wrapped boolean predicate
          -> Value a    -- ^ value to replace
          -> Value a    -- ^ result
predicate t p x = if p == Value True then x else t

-----------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------

-- |The function 'fromValue' converts a value from a extended value.
fromValue         :: a -> Value a -> a
-- |The function 'fromValue' converts a value from a extended value.
unsafeFromValue   ::  Value a -> a
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
fromValue _ (Value y)     = y
unsafeFromValue (Value y) = y
unsafeFromValue Abst     = error "Should not be absent or undefined"
isAbsent Abst            = True
isAbsent _               = False
isUndefined Undef        = True
isUndefined _            = False
isPresent (Value _)       = True
isPresent _              = False



infixl 5 >¤
-- | serial storage infix operator. Stores only present values
(>¤) :: Value (Vector a) -- ^ input storing buffer wrapped as value
     -> Value a          -- ^ value to be storred
     -> Value (Vector a) -- ^ output buffer
buff >¤ Abst   = buff
buff >¤ Undef  = buff
buff >¤ Value a = (:>) <$> Value a <*> buff

------------------------------------------------------------------------------
