{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skeleton.Vector
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the data type 'Vector' and the
-- corresponding functions. It is a development of the module
-- defined by Reekie.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skeleton.Vector where

import ForSyDe.Atom.Skeleton

import Prelude hiding (head, last, init, tail, map)

infixr 3 :>
infixl 5 <:
-- infixr 5 <++>

-- | The data type 'Vector' is modeled similar to a list.
data Vector a = Null
              | a :> (Vector a) deriving (Eq)

instance Functor Vector where
  fmap _ Null   = Null
  fmap f (x:>xs) = f x :> fmap f xs

instance Applicative Vector where
  pure x = x :> pure x
  _         <*> Null      = Null
  Null      <*> _         = Null
  (f :> fs) <*> (x :> xs) = f x :> fs <*> xs

instance Foldable Vector where
  foldr k z = go
    where go Null    = z
          go (y:>ys) = y `k` go ys

instance Skeleton Vector where
  (=$=) = (<$>)
  (=*=) = (<*>)
  (=\=) = foldl1

-- | The vector 1 :> 2 :> Null is represented as \<1,2\>.
instance (Show a) => Show (Vector a) where
  showsPrec p = showParen (p>1) . showVector
    where
      showVector (x :> xs)  = showChar '<' . showEvent x . showVector' xs
      showVector (Null)   = showChar '<' . showChar '>'
      showVector' (x :> xs) = showChar ',' . showEvent x . showVector' xs
      showVector' (Null)  = showChar '>'
      showEvent x           = shows x

-- | The vector 1 :> 2 >- Null is read using the string \"\<1,2\>\".
instance (Read a) => Read (Vector a) where
  readsPrec d = readParen (d>1) readVecSigtart
    where
      readVecSigtart = (\ a -> [(xs,c) | ("<",b) <- lex a , (xs,c) <- readVector (',' : b) ++ readNull b])
      readVector r    = readEvent r ++ readNull r
      readEvent a     = [(x :> xs,d) | (",",b) <- lex a , (x,c) <- reads b , (xs,d) <- readVector c]
      readNull a      = [(Null,b) | (">",b) <- lex a]

-- Utilities

vector []     = Null
vector (x:xs) = x :> (vector xs)

fromVector Null    = []
fromVector (x:>xs) = x : fromVector xs

-- "Constructors"

unit a = a :> Null

Null    <++> ys = ys
(x:>xs) <++> ys = x :> (xs <++> ys) 

xs <: x = xs <++> unit x         

-- Skeletons

map :: (a -> b) -> Vector a -> Vector b
map = (=$=)

red :: (a -> a -> a) -> Vector a -> a
red = (=\=)

last  :: Vector a -> a
first :: Vector a -> a
inits :: Vector a -> Vector (Vector a)
tails :: Vector a -> Vector (Vector a)

first = red (\x y -> x)
last  = red (\x y -> y)
inits = red (\x y -> x <++> ((last  x <++>) =$=) y) . map (unit . unit)
tails = red (\x y -> ((<++> first y) =$=) x <++> y) . map (unit . unit)


-- -- | The function 'vector' converts a list into a vector.
-- vector     :: [a] -> Vector a

-- -- | The function 'fromVector' converts a vector into a list.
-- fromVector :: Vector a -> [a]

-- -- | The function 'unitV' creates a vector with one element. 
-- unitV    :: a -> Vector a

-- -- | The function 'nullV' returns 'True' if a vector is empty. 
-- nullV    :: Vector a -> Bool

-- -- | The function 'lengthV' returns the number of elements in a value. 
-- lengthV  :: Vector a -> Int

-- -- | The function 'atV' returns the n-th element in a vector, starting from zero.
-- atV      :: (Num a, Eq a) => Vector b -> a -> b

-- -- | The function 'atV' returns the n-th element in a vector, starting from zero.
-- getV   :: (Num a, Eq a) => a -> Vector b -> b

-- -- |  The function 'replaceV' replaces an element in a vector.
-- replaceV :: Vector a -> Int -> a -> Vector a

-- -- | The functions 'headV' returns the first element of a vector.
-- headV :: Vector a -> a

-- -- | The function 'lastV' returns the last element of a vector.
-- lastV :: Vector a -> a

-- -- | The functions 'tailV' returns all, but the first element of a vector.
-- tailV :: Vector a -> Vector a 

-- -- | The function 'initV' returns all but the last elements of a vector.
-- initV :: Vector a -> Vector a 

-- -- | The function 'takeV' returns the first n elements of a vector.
-- takeV :: (Num a, Ord a) => a -> Vector b -> Vector b

-- -- | The function 'dropV' drops the first n elements of a vector.
-- dropV :: (Num a, Ord a) => a -> Vector b -> Vector b



-- -- | The function 'selectV' selects elements in the vector. The first argument gives the initial element, starting from zero, the second argument gives the stepsize between elements and the last argument gives the number of elements. 
-- selectV :: Int -> Int -> Int -> Vector a -> Vector a


-- -- | The function 'groupV' groups a vector into a vector of vectors of size n.
-- groupV :: Int -> Vector a -> Vector (Vector a)

-- -- | The operator '(<:)' adds an element at the end of a vector.
-- (<:)  :: Vector a -> a -> Vector a

-- -- | The operator '(<+>)' concatinates two vectors.
-- (<+>) :: Vector a -> Vector a -> Vector a




-- -- | The higher-order function 'mapV' applies a function on all elements of a vector.
-- mapV :: (a -> b) -> Vector a -> Vector b    


-- -- | The higher-order function 'zipWithV' applies a function pairwise on to vectors.
-- zipWithV :: (a -> b -> c) -> Vector a -> Vector b -> Vector c


-- -- | The higher-order functions 'foldlV' folds a function from the right to the left  over a vector using an initial value.
-- foldlV :: (a -> b -> a) -> a -> Vector b -> a 

-- -- | The higher-order functions 'foldrV' folds a function from the left to the right over a vector using an initial value.
-- foldrV :: (b -> a -> a) -> a -> Vector b -> a

-- -- | The higher-function 'filterV' takes a predicate function and a vector and creates a new vector with the elements for which the predicate is true. 
-- filterV :: (a -> Bool) -> Vector a -> Vector a

-- -- | The function 'zipV' zips two vectors into a vector of tuples.
-- zipV   :: Vector a -> Vector b -> Vector (a, b)

-- -- | The function 'unzipV' unzips a vector of tuples into two vectors.
-- unzipV :: Vector (a, b) -> (Vector a, Vector b)



-- -- | The function 'shiftlV' shifts a value from the left into a vector. 
-- shiftlV :: Vector a -> a-> Vector a 

-- -- | The function 'shiftrV' shifts a value from the right into a vector. 
-- shiftrV :: Vector a -> a -> Vector a

-- -- | The function 'rotlV' rotates a vector to the left. Note that this fuctions does not change the size of a vector.
-- rotlV   :: Vector a -> Vector a

-- -- | The function 'rotrV' rotates a vector to the right. Note that this fuction does not change the size of a vector.
-- rotrV   :: Vector a -> Vector a


-- -- | The function 'concatV' transforms a vector of vectors to a single vector. 
-- concatV   :: Vector (Vector a) -> Vector a

-- -- | The function 'reverseV' reverses the order of elements in a vector. 
-- reverseV  :: Vector a -> Vector a


-- -- | The function 'iterateV' generates a vector with a given number of elements starting from an initial element using a supplied function for the generation of elements. 
-- --
-- -- > Vector> iterateV 5 (+1) 1
-- --
-- -- > <1,2,3,4,5> :: Vector Integer
-- iterateV :: (Num a, Eq a) => a -> (b -> b) -> b -> Vector b

-- -- | The function 'generateV' behaves in the same way, but starts with the application of the supplied function to the supplied value. 
-- --
-- -- > Vector> generateV 5 (+1) 1
-- -- 
-- -- > <2,3,4,5,6> :: Vector Integer
-- generateV :: (Num a, Eq a) => a -> (b -> b) -> b -> Vector b

-- -- | The function 'copyV' generates a vector with a given number of copies of the same element. 
-- --
-- -- > Vector> copyV 7 5 
-- -- 
-- -- > <5,5,5,5,5,5,5> :: Vector Integer
-- copyV     :: (Num a, Eq a) => a -> b -> Vector b



-- unitV x = x :> Null

-- nullV Null   = True
-- nullV _             = False

-- lengthV Null   = 0
-- lengthV (_:>xs) = 1 + lengthV xs

-- replaceV vs n x 
--     | n <= lengthV vs && n >= 0 = takeV n vs <+> unitV x 
--      <+> dropV (n+1) vs
--     | otherwise                 =  vs

-- Null   `atV` _ = error "atV: Vector has not enough elements"
-- (x:>_)  `atV` 0 = x
-- (_:>xs) `atV` n = xs `atV` (n-1)

-- getV a b = atV b a

-- headV Null   = error "headV: Vector is empty"
-- headV (v:>_) = v

-- tailV Null   = error "tailV: Vector is empty"
-- tailV (_:>vs) = vs

-- lastV Null      = error "lastV: Vector is empty"
-- lastV (v:>Null) = v
-- lastV (_:>vs)    = lastV vs

-- initV Null      = error "initV: Vector is empty"
-- initV (_:>Null) = Null
-- initV (v:>vs)    = v :> initV vs

-- takeV 0 _                   = Null
-- takeV _ Null               = Null
-- takeV n (v:>vs) | n <= 0    = Null
--                 | otherwise = v :> takeV (n-1) vs

-- dropV 0 vs                  = vs
-- dropV _ Null               = Null
-- dropV n (v:>vs) | n <= 0    = v :> vs
--                 | otherwise = dropV (n-1) vs

-- selectV f s n vs | n <= 0               
--                      = Null
--                  | (f+s*n-1) > lengthV vs 
--                     = error "selectV: Vector has not enough elements"
--                  | otherwise            
--                     = atV vs f :> selectV (f+s) s (n-1) vs

-- groupV n v 
--       | lengthV v < n = Null
--       | otherwise     = selectV 0 1 n v 
--                         :> groupV n (selectV n 1 (lengthV v-n) v)


-- mapV _ Null   = Null
-- mapV f (x:>xs) = f x :> mapV f xs

-- zipWithV f (x:>xs) (y:>ys) = f x y :> (zipWithV f xs ys)
-- zipWithV _ _       _       = Null

-- foldlV _ a Null   = a
-- foldlV f a (x:>xs) = foldlV f (f a x) xs

-- foldrV _ a Null   = a 
-- foldrV f a (x:>xs) = f x (foldrV f a xs)

-- filterV _ Null   = Null
-- filterV p (v:>vs) = if (p v) then
--                      v :> filterV p vs
--                   else 
--                      filterV p vs

-- zipV (x:>xs) (y:>ys) = (x, y) :> zipV xs ys
-- zipV _       _       = Null

-- unzipV Null           = (Null, Null)
-- unzipV ((x, y) :> xys) = (x:>xs, y:>ys) 
--  where (xs, ys) = unzipV xys

-- shiftlV vs v = v :> initV vs

-- shiftrV vs v = tailV vs <: v

-- rotrV Null = Null
-- rotrV vs    = tailV vs <: headV vs

-- rotlV Null = Null
-- rotlV vs    = lastV vs :> initV vs

-- concatV = foldrV (<+>) Null

-- reverseV Null   = Null
-- reverseV (v:>vs) = reverseV vs <: v

-- generateV 0 _ _ = Null
-- generateV n f a = x :> generateV (n-1) f x 
--                 where x = f a

-- iterateV 0 _ _ = Null
-- iterateV n f a = a :> iterateV (n-1) f (f a)

-- copyV k x = iterateV k id x 


-- -- | The 'pipe' function constructs a serial composition from a vector of functions.
-- --
-- -- __OBS__: composition is right associative thus the input vector should contain functions from right to left
-- pipeV         :: Vector (b -> b) -> b -> b
-- pipeV Null   = id
-- pipeV (v:>vs) = v . pipeV vs 

-- -- | The 'scan' function constructs a parallel prefix vector from a vector of functions.
-- --
-- -- __OBS__: composition is right associative thus the input vector should contain functions from right to left
-- scanV         :: Vector (b -> b) -> b -> Vector b
-- scanV Null   = pure Null  
-- scanV (x:>xs) = (:>) <$> x . pipeV xs <*> scanV xs
