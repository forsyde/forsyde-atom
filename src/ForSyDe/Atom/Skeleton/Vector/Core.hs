{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skeleton.Vector.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The core implementation of the 'Vector' type
-----------------------------------------------------------------------------

module ForSyDe.Atom.Skeleton.Vector.Core where

import ForSyDe.Atom.Skeleton

import Prelude hiding (null)


infixr 3 :>
infixl 5 <:
infixr 5 <++>

-- | The data type 'Vector', or at least its interpretation, is the
-- exact equivalent of a catamorphic list, as defined in
-- <#bird96 [2]>. Its name though is borrowed from <#reekie95 [4]>,
-- since it is more suggestive in the context of process networks.
--
-- Due to reasons of efficiency, and to ensure that the structure is
-- flat and homogeneous, 'Vector' is implemented using the same
-- constructors as an infinite list, as seen below. In the
-- implementation of skeletons though, we shall interpret 'Vector' as
-- being <#g:2 \"constructed\"> in a different fashion, one which
-- suggests that a 'Vector' is divided and evaluated in parallel.
data Vector a = Null            -- ^ Null element. Terminates a vector.
              | a :> (Vector a) -- ^ appends an element at the head of a vector.
              deriving (Eq)

--------------------
-- "Constructors" --
--------------------

-- | Constructs a null vector.
null = Null

-- | Constructs a singleton vector.
unit a = a :> Null

-- | Constructs a vector by appending two existing vectors.
Null    <++> ys = ys
(x:>xs) <++> ys = x :> (xs <++> ys) 

---------------
-- Instances --
---------------

-- | Provides an implementation for '=$='.
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
  (=$=) = (<$>)
  (=*=) = (<*>)
  _ =\= Null = error "reduce: empty vector" 
  f =\= v    = foldr1 f v

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

-- | Creates the vector:
--
-- > <1,2,3,4,...>
--
-- Used mainly for operation on indexes. __OBS:__ Do not use it with
-- any selector skeleton, since this will lead to circular reference.
indexes = ixf 1
  where ixf n = n :> ixf (n+1)

-- | Returns @True@ if the argument is a null vector.
isNull Null = True
isNull _    = False

-- | Appends an element at the end of a vector.
xs <: x = xs <++> unit x         
