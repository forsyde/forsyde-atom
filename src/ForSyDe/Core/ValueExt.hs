-----------------1------------------------------------------------------------
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
module ForSyDe.Core.ValueExt(

  -- | The behavior layer is enabled by the 'Value' type constructor.
  
  Value(..),

  -- * Utility functions

  -- | These functions are provided for user convenience

  value, fromValue, unsafeFromValue, isPresent, isAbsent, isUndefined,

  -- * Behavior atoms

  -- | These are the primitive (undivisible) blocks for implementing
  -- behavior wrappers. Each atom has a distinct behavioural semantic
  -- which reflects a real (analizable, implementable) behavior.
  
  (>$), (>*), (>%), (>%!), (>#), (>#!),

  -- * Behavior wrappers
  
  -- ** @psi@
  
  -- | The @psi@/XY/ wrapper wraps a function with /X/ inputs and /Y/
  -- outputs in a default ForSyDe behavior. E.g.:
  --
  -- >>> let (v,u,a)=(Value 1, Undef, Abst)
  -- >>> (v,u,a)
  -- (1,?,⟂)
  -- >>> let f a b c = (a+b,b-c)
  -- >>> let wf = psi32 f
  -- >>> t: wf
  -- wf :: Num b => Value b -> Value b -> Value b -> (Value b, Value b)
  -- >>> wf v v v
  -- (2,0)
  -- >>> wf v u v
  -- (?,?)
  -- >>> wf v a v
  -- (*** Exception: Illegal occurrence of an absent and present event
  
  psi11, psi12, psi13, psi14, psi15, psi16, psi17, psi18,
  psi21, psi22, psi23, psi24, psi25, psi26, psi27, psi28,
  psi31, psi32, psi33, psi34, psi35, psi36, psi37, psi38,
  psi41, psi42, psi43, psi44, psi45, psi46, psi47, psi48,
  psi51, psi52, psi53, psi54, psi55, psi56, psi57, psi58,
  psi61, psi62, psi63, psi64, psi65, psi66, psi67, psi68,
  psi71, psi72, psi73, psi74, psi75, psi76, psi77, psi78,
  psi81, psi82, psi83, psi84, psi85, psi86, psi87, psi88,

  -- ** @store@

  -- | The @store@/X/ wrapper pushes the present defined values into a
  -- given list (e.g. FIFO) buffer.
  --
  -- >>> store5 (Value [1,2]) (Value 3) Undef (Value 4) Abst Undef
  -- [4,3,1,2]
  -- >>> store5 Undef         (Value 3) Undef (Value 4) Abst Undef
  -- ?
  -- >>> store5 Abst          (Value 3) Undef (Value 4) Abst Undef
  -- *** Exception: Illegal occurrence of an absent and present event
  
  store1, store2, store3, store4, store5, store6, store7, store8,

  -- ** @reduce@

  -- | The @reduce@/X/ merges the values belonging to multiple signals
  -- based on a given rule
  --
  -- >>> reduce5 (+) (Value 3) (Value 4) Abst (Value 1) Abst
  -- 8
  -- >>> reduce5 (+) (Value 3) (Value 4) Abst (Value 1) Undef
  -- ?

  reduce2, reduce3, reduce4, reduce5, reduce6, reduce7, reduce8,

  -- ** @replace@

  -- | The @replace@/X/ class of wrappers replaces a value with an
  -- @[ V - another value | U - undefined value | A - absent event ]@
  -- based on a boolean predicate.
  
  replaceV, replaceU, replaceA, unsafeReplaceV, unsafeReplaceU,
  
  ) where

import ForSyDe.Core.Utilities

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

-----------------------------------------------------------------------------
-- Behavioural implementations
-----------------------------------------------------------------------------

infixl 4 >$, >*, >%, >%!, >#, >#!
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

(>$) :: (a -> b) -> Value a -> Value b
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
(>*) :: Value (a -> b) -> Value a -> Value b
(>*) = (<*>)

-- | Reduce atom. Used to recursively fold multiple values from
-- different signals. Undefined values affect the result.
--
-- >>> let x = ((:),Value []) >% (Value 1) >% Abst >% (Value 2) 
-- >>> snd x
-- [2,1]
-- >>> snd $ x >% Undef 
-- Undef
(>%) :: ((a -> b -> b), Value b) -- ^ (folding function, kernel value)
     -> Value a                  -- ^ current term
     -> ((a -> b -> b), Value b) -- ^ (same folding function to be
                                 -- passed to the next term, folding
                                 -- result)
(f, k) >% Abst  = (f, k)
(f, k) >% x     = (f, f <$> x <*> k)


-- | Unsafe reduce atom. Used to recursively fold multiple values from
-- different signals. Undefined values are ignored!
--
-- >>> let x = ((:),Value []) >% (Value 1) >% Abst >% (Value 2) >% Undef
-- >>> snd x
-- [2,1]
(>%!) :: ((a -> b -> b), Value b) -- ^ (folding function, kernel value)
      -> Value a                  -- ^ current term
      -> ((a -> b -> b), Value b) -- ^ (same folding function to be
                                  -- passed to the next term, folding
                                  -- result)
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
(>#) :: Value Bool         -- ^ boolean predicate
     -> (Value a, Value a) -- ^ (value to replace, value to replace with)
     -> Value a            -- ^ result
_           ># (_, Abst) = Abst
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
(>#!) :: Value Bool         -- ^ boolean predicate
      -> (Value a, Value a) -- ^ (value to replace, value to replace with)
      -> Value a            -- ^ result
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


-----------------------------------------------------------------------------
-- Behavior wrappers
-----------------------------------------------------------------------------

psi11 f a1                      =         (f >$ a1)
psi12 f a1                      = funzip2 (f >$ a1)
psi13 f a1                      = funzip3 (f >$ a1)
psi14 f a1                      = funzip4 (f >$ a1)
psi15 f a1                      = funzip5 (f >$ a1)
psi16 f a1                      = funzip6 (f >$ a1)
psi17 f a1                      = funzip7 (f >$ a1)
psi18 f a1                      = funzip8 (f >$ a1)
psi21 f a1 a2                   =         (f >$ a1 >* a2)
psi22 f a1 a2                   = funzip2 (f >$ a1 >* a2)
psi23 f a1 a2                   = funzip3 (f >$ a1 >* a2)
psi24 f a1 a2                   = funzip4 (f >$ a1 >* a2)
psi25 f a1 a2                   = funzip5 (f >$ a1 >* a2)
psi26 f a1 a2                   = funzip6 (f >$ a1 >* a2)
psi27 f a1 a2                   = funzip7 (f >$ a1 >* a2)
psi28 f a1 a2                   = funzip8 (f >$ a1 >* a2)
psi31 f a1 a2 a3                =         (f >$ a1 >* a2 >* a3)
psi32 f a1 a2 a3                = funzip2 (f >$ a1 >* a2 >* a3)
psi33 f a1 a2 a3                = funzip3 (f >$ a1 >* a2 >* a3)
psi34 f a1 a2 a3                = funzip4 (f >$ a1 >* a2 >* a3)
psi35 f a1 a2 a3                = funzip5 (f >$ a1 >* a2 >* a3)
psi36 f a1 a2 a3                = funzip6 (f >$ a1 >* a2 >* a3)
psi37 f a1 a2 a3                = funzip7 (f >$ a1 >* a2 >* a3)
psi38 f a1 a2 a3                = funzip8 (f >$ a1 >* a2 >* a3)
psi41 f a1 a2 a3 a4             =         (f >$ a1 >* a2 >* a3 >* a4)
psi42 f a1 a2 a3 a4             = funzip2 (f >$ a1 >* a2 >* a3 >* a4)
psi43 f a1 a2 a3 a4             = funzip3 (f >$ a1 >* a2 >* a3 >* a4)
psi44 f a1 a2 a3 a4             = funzip4 (f >$ a1 >* a2 >* a3 >* a4)
psi45 f a1 a2 a3 a4             = funzip5 (f >$ a1 >* a2 >* a3 >* a4)
psi46 f a1 a2 a3 a4             = funzip6 (f >$ a1 >* a2 >* a3 >* a4)
psi47 f a1 a2 a3 a4             = funzip7 (f >$ a1 >* a2 >* a3 >* a4)
psi48 f a1 a2 a3 a4             = funzip8 (f >$ a1 >* a2 >* a3 >* a4)
psi51 f a1 a2 a3 a4 a5          =         (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi52 f a1 a2 a3 a4 a5          = funzip2 (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi53 f a1 a2 a3 a4 a5          = funzip3 (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi54 f a1 a2 a3 a4 a5          = funzip4 (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi55 f a1 a2 a3 a4 a5          = funzip5 (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi56 f a1 a2 a3 a4 a5          = funzip6 (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi57 f a1 a2 a3 a4 a5          = funzip7 (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi58 f a1 a2 a3 a4 a5          = funzip8 (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi61 f a1 a2 a3 a4 a5 a6       =         (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi62 f a1 a2 a3 a4 a5 a6       = funzip2 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi63 f a1 a2 a3 a4 a5 a6       = funzip3 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi64 f a1 a2 a3 a4 a5 a6       = funzip4 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi65 f a1 a2 a3 a4 a5 a6       = funzip5 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi66 f a1 a2 a3 a4 a5 a6       = funzip6 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi67 f a1 a2 a3 a4 a5 a6       = funzip7 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi68 f a1 a2 a3 a4 a5 a6       = funzip8 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi71 f a1 a2 a3 a4 a5 a6 a7    =         (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi72 f a1 a2 a3 a4 a5 a6 a7    = funzip2 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi73 f a1 a2 a3 a4 a5 a6 a7    = funzip3 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi74 f a1 a2 a3 a4 a5 a6 a7    = funzip4 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi75 f a1 a2 a3 a4 a5 a6 a7    = funzip5 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi76 f a1 a2 a3 a4 a5 a6 a7    = funzip6 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi77 f a1 a2 a3 a4 a5 a6 a7    = funzip7 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi78 f a1 a2 a3 a4 a5 a6 a7    = funzip8 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi81 f a1 a2 a3 a4 a5 a6 a7 a8 =         (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi82 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip2 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi83 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip3 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi84 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip4 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi85 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip5 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi86 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip6 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi87 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip7 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi88 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip8 (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)


store1 buff a1                      = at22 $ ((:), buff) >%! a1
store2 buff a1 a2                   = at22 $ ((:), buff) >%! a1 >%! a2
store3 buff a1 a2 a3                = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3
store4 buff a1 a2 a3 a4             = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4
store5 buff a1 a2 a3 a4 a5          = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5
store6 buff a1 a2 a3 a4 a5 a6       = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5 >%! a6
store7 buff a1 a2 a3 a4 a5 a6 a7    = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5 >%! a6 >%! a7
store8 buff a1 a2 a3 a4 a5 a6 a7 a8 = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5 >%! a6 >%! a7 >%! a8


reduce2 f a1 a2                   = at22 $ (f, a1) >% a2
reduce3 f a1 a2 a3                = at22 $ (f, a1) >% a2 >% a3
reduce4 f a1 a2 a3 a4             = at22 $ (f, a1) >% a2 >% a3 >% a4
reduce5 f a1 a2 a3 a4 a5          = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5
reduce6 f a1 a2 a3 a4 a5 a6       = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5 >% a6
reduce7 f a1 a2 a3 a4 a5 a6 a7    = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5 >% a6 >% a7
reduce8 f a1 a2 a3 a4 a5 a6 a7 a8 = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5 >% a6 >% a7 >% a8


replaceV       v p x = p >#  (x,v)
replaceU         p x = p >#  (x,Undef)
replaceA         p x = p >#  (x,Abst)
unsafeReplaceV v p x = p >#! (x,v)
unsafeReplaceU   p x = p >#! (x,Undef)
