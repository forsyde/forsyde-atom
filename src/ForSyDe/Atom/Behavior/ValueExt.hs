{-# LANGUAGE PostfixOperators #-}
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
-- This module implements the atoms and wrappers for the behavior
-- layer in ForSyDe processes.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Behavior.ValueExt where

import ForSyDe.Atom.Behavior.Atoms

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
-- rules. Provides the @(\<*\>)@ operator
instance Applicative Value where
  pure = Value 
  (Value x) <*> (Value y) = Value (x y)
  Undef     <*> (Value _) = Undef
  (Value _) <*> Undef     = Undef
  Undef     <*> Undef     = Undef
  Abst      <*> Abst      = Abst
  _         <*> _         = error "Illegal occurrence of an absent and present event"


instance Behavior Value where
  -- | The exact equivalent of the 'fmap' function or the '<$>' infix
  -- operator for extended values. It bypasses the special values and
  -- maps a function @f@ to a wrapped value. It may be regarded as the
  -- "default behavior" of a process.
  --
  -- >>> let (a,u,v)=(Abst,Undef,Value 1)
  -- >>> (a,u,v)
  -- (⟂,?,1)
  -- >>> let f = (+1)
  -- >>> (f >$ a, f >$ u, f >$ v)
  -- (⟂,?,2)
  -- >>> (f <$> a, f <$> u, f <$> v)
  -- (⟂,?,2)
  --
  -- As seen in the example above, '>$' is interchangeable with '<$>'.
  (>$) = fmap

  -- | The exact equivalent of the applicative operator '<*>' for
  -- extended values. It may be regarded as the default behavior in case
  -- two events interact with each other. Check the source source code
  -- for the Applicative instance to see the lifting rules.
  --
  -- >>> let (a,u,v)=(Abst,Undef,Value 1)
  -- >>> (a,u,v)
  -- (⟂,?,1)
  -- >>> (+) >$ v >* v
  -- 2
  -- >>> (+) >$ v >* u
  -- ?
  -- >>> (+) >$ a >* v
  -- *** Exception: Illegal occurrence of an absent and present event
  --
  -- As with '>$', '>*' is also interchangeable with '<*>'.
  (>*) = (<*>)

  -- | Reduce atom. Used to recursively fold multiple values from
  -- different signals. Undefined values affect the result.
  --
  -- >>> let x = ((:),Value []) >% (Value 1) >% Abst >% (Value 2) 
  -- >>> snd x
  -- [2,1]
  -- >>> snd $ x >% Undef 
  -- Undef
  (f, k) >% Abst  = (f, k)
  (f, k) >% x     = (f, f <$> x <*> k)


  -- | Unsafe reduce atom. Used to recursively fold multiple values from
  -- different signals. Undefined values are ignored!
  --
  -- >>> let x = ((:),Value []) >% (Value 1) >% Abst >% (Value 2) >% Undef
  -- >>> snd x
  -- [2,1]
  (f, k) >%! Abst  = (f, k)
  (f, k) >%! Undef = (f, k)
  (f, k) >%! x     = (f, f <$> x <*> k)


  -- | Predicate atom which replaces a value with another value.
  -- The 'Abst' event persists.
  --
  -- >>> let ps = map Value [False, True]
  -- >>> zipWith (>#) ps $ repeat (Value 1, Value 5)
  -- [1,5]
  -- >>> zipWith (>#) ps $ repeat (Undef, Value 5)
  -- [?,5]
  -- >>> zipWith (>#) ps $ repeat (Abst, Value 5)
  -- [⟂,⟂]
  Value True  ># (_, v)    = v
  Value False ># (x, _)    = x

  -- | Unsafe predicate atom which replaces a value with another
  -- value. Overwrites even 'Abst' events.
  --
  -- >>> let ps = map Value [False, True]
  -- >>> zipWith (>#!) ps $ repeat (Value 1, Value 5)
  -- [1,5]
  -- >>> zipWith (>#!) ps $ repeat (Undef, Value 5)
  -- [?,5]
  -- >>> zipWith (>#!) ps $ repeat (Abst, Value 5)
  -- [⟂,5]
  Value True  >#! (_, v)    = v
  Value False >#! (x, _)    = x



-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

-- |The function 'fromValue' converts a value from a extended value.
fromValue         :: a -> Value a -> a
-- |The function 'fromValue' converts a value from a extended value.
unsafeFromValue   ::  Value a -> a
-- |The functions 'isPresent' checks for the presence of a value.
isPresent         :: Value a -> Value Bool
-- |The functions 'isAbsent' checks for the absence of a value.
isAbsent          :: Value a -> Value Bool
-- |The functions 'isUndefined' checks if the value is defined.
isUndefined       :: Value a -> Value Bool
-- | The function 'abstExt' converts a usual value to a present value. 
value           :: a -> Value a  

value                     = pure
fromValue x Abst          = x   
fromValue x Undef         = x   
fromValue _ (Value y)     = y
unsafeFromValue (Value y) = y
unsafeFromValue _         = error "Should not be absent or undefined"
isAbsent Abst       = Value True
isAbsent _          = Value False
isUndefined Undef   = Value True
isUndefined _       = Value False
isPresent (Value _) = Value True
isPresent _         = Value False
isNotPresent a = not <$> isPresent a
