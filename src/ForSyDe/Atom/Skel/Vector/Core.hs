{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skel.Vector.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The core implementation of the 'Vector' type
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skel.Vector.Core where

import ForSyDe.Atom.Skel

import Prelude hiding (null)

infixr 3 :>
infixl 5 <:
infixr 5 <++>

-- | Although the name 'Vector' is borrowed from <ForSyDe-Atom.html#reekie95
-- [Reekie95]> since it is more suggestive in the context of process networks, the
-- 'Vector' type is in fact modeling an infinite list defined as a category in
-- <ForSyDe-Atom.html#skillicorn05 [Skillicorn05]>. According to this definition it
-- should be implemented as following:
--
-- > data Vector a = Null                   -- null element
-- >               | Unit a                 -- singleton vector
-- >               | Vector a <++> Vector a -- concatenate two vectors
--
-- This construction suggests the possibility of splitting a 'Vector' into multiple
-- parts and evaluating it in parallel. For simplicity and to ensure that the
-- structure is flat and homogeneous, 'Vector' is implemented using the same
-- constructors as a regular Haskell list (see below). When defining skeletons of
-- vectors we will not use the real constructors though, but the theoretical ones
-- defined above and provided as <#g:2 functions>. This way we align ForSyDe-Atom's
-- 'Vector' type with the skeleton theory and its theorems.
--
-- Another particularity of 'Vector' is that it instantiates the reduction atom '=\='
-- as a /right fold/, as it is the most efficient lazy implementation of lists. As a
-- consequence reduction is performed __/from right to left/__. This is noticed
-- especially in the case of pipeline-based skeletons (see definition of
-- 'ForSyDe.Atom.Skel.pipe' as a reduction with the right-associative composition
-- operator '.') is performed from right to left. Thus for 'reduce'-based skeletons
-- (e.g. 'prefix', 'suffix', 'recur', 'cascade', 'mesh') the result vectors shall be
-- read from end to beginning.
data Vector a = Null
              -- ^ Null element. Terminates a vector.
              | a :> (Vector a)
              -- ^ appends an element at the head of a vector.
              deriving (Eq)

--------------------
-- "Constructors" --
--------------------

-- | Constructs a null vector.
--
-- >>> null
-- <>
null = Null

-- | Constructs a singleton vector.
--
-- >>> unit 1
-- <1>
unit a = a :> Null

-- | Constructs a vector by appending two existing vectors.
--
-- >>> unit 1 <++> unit 2
-- <1,2>
Null    <++> ys = ys
(x:>xs) <++> ys = x :> (xs <++> ys) 

---------------
-- Instances --
---------------

-- | Provides an implementation for '=.='.
instance Functor Vector where
  fmap _ Null   = Null
  fmap f (x:>xs) = f x :> fmap f xs


-- | Provides an implementation for '=*='.
instance Applicative Vector where
  pure x = x :> pure x
  _         <*> Null      = Null
  Null      <*> _         = Null
  (f :> fs) <*> (x :> xs) = f x :> fs <*> xs

-- | Provides an implementation for '=\='.
instance Foldable Vector where
  foldr k z = go
    where go Null    = z
          go (y:>ys) = y `k` go ys

-- | Ensures that 'Vector' is a structure associated with the Skeleton Layer.
instance Skeleton Vector where
  (=.=) = (<$>)
  (=*=) = (<*>)
  _ =\= Null   = error "[Skel.Vector] cannot reduce empty vector" 
  f =\= v      = foldr1 f v
  Null =<<= s = s
  ps   =<<= s = (.) =\= ps $ s
  first (x:>_) = x

-- | The vector 1 :> 2 :> Null is represented as \<1,2\>.
instance (Show a) => Show (Vector a) where
  showsPrec p = showParen (p>1) . showVector
    where
      showVector (x :> xs)  = showChar '<' . showEvent x . showVector' xs
      showVector (Null)     = showChar '<' . showChar '>'
      showVector' (x :> xs) = showChar ',' . showEvent x . showVector' xs
      showVector' (Null)    = showChar '>'
      showEvent x           = shows x

-- | The vector 1 :> 2 :> Null is read using the string \"\<1,2\>\".
instance (Read a) => Read (Vector a) where
  readsPrec d = readParen (d>1) readVecSigtart
    where
      readVecSigtart = (\ a -> [(xs,c) | ("<",b) <- lex a , (xs,c) <- readVector (',' : b) ++ readNull b])
      readVector r   = readEvent r ++ readNull r
      readEvent a    = [(x :> xs,d) | (",",b) <- lex a , (x,c) <- reads b , (xs,d) <- readVector c]
      readNull a     = [(Null,b) | (">",b) <- lex a]


---------------
-- Utilities --
---------------

-- | Converts a list to a vector.
vector []     = Null
vector (x:xs) = x :> (vector xs)

-- | Converts a vector to a list.
fromVector Null    = []
fromVector (x:>xs) = x : fromVector xs

-- | Creates the infinite vector:
--
-- > <1,2,3,4,...>
--
-- Used mainly for operation on indexes.
indexes :: Vector Int
indexes = vector [1..]

-- | Returns @True@ if the argument is a null vector.
isNull Null = True
isNull _    = False

-- | Appends an element at the end of a vector.
xs <: x = xs <++> unit x         
