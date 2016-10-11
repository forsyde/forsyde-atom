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
import ForSyDe.Atom.Utility
import Data.Maybe

import Prelude hiding (head, last, init, tail, map, reverse, length, concat, take, drop)

infixr 3 :>
infixl 5 <:
infixr 5 <++>

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
      showVector (Null)     = showChar '<' . showChar '>'
      showVector' (x :> xs) = showChar ',' . showEvent x . showVector' xs
      showVector' (Null)    = showChar '>'
      showEvent x           = shows x

-- | The vector 1 :> 2 >- Null is read using the string \"\<1,2\>\".
instance (Read a) => Read (Vector a) where
  readsPrec d = readParen (d>1) readVecSigtart
    where
      readVecSigtart = (\ a -> [(xs,c) | ("<",b) <- lex a , (xs,c) <- readVector (',' : b) ++ readNull b])
      readVector r   = readEvent r ++ readNull r
      readEvent a    = [(x :> xs,d) | (",",b) <- lex a , (x,c) <- reads b , (xs,d) <- readVector c]
      readNull a     = [(Null,b) | (">",b) <- lex a]

-- Utilities

vector []     = Null
vector (x:xs) = x :> (vector xs)

fromVector Null    = []
fromVector (x:>xs) = x : fromVector xs

indexes = ixf 1
  where ixf n = n :> ixf (n+1)

fanout x = x :> fanout x

fanoutn n x | n == 0    = Null
            | otherwise = x :> fanoutn (n-1) x

-- "Constructors"

unit a = a :> Null

Null    <++> ys = ys
(x:>xs) <++> ys = x :> (xs <++> ys) 

xs <: x = xs <++> unit x         

-- Skeletons
a = vector [1,2,3,4,5,6,7,8,9]

map :: (a -> b) -> Vector a -> Vector b
red :: (a -> a -> a) -> Vector a -> a
map = (=$=)
red = (=\=)

map11 p v1                      = (p =$= v1)
map21 p v1 v2                   = (p =$= v1 =*= v2)
map31 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3)
map41 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4)
map51 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5)
map61 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6)
map71 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7)
map81 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 =*= v8)

map12 p v1                      = (p =$= v1 |<)
map22 p v1 v2                   = (p =$= v1 =*= v2 |<)
map32 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3 |<)
map42 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4 |<)
map52 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<)
map62 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<)
map72 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<)
map82 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v5 =*= v8 |<)

map13 p v1                      = (p =$= v1 |<<)
map23 p v1 v2                   = (p =$= v1 =*= v2 |<<)
map33 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3 |<<)
map43 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4 |<<)
map53 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<<)
map63 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<<)
map73 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<<)
map83 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v5 =*= v8 |<<)

map14 p v1                      = (p =$= v1 |<<<)
map24 p v1 v2                   = (p =$= v1 =*= v2 |<<<)
map34 p v1 v2 v3                = (p =$= v1 =*= v2 =*= v3 |<<<)
map44 p v1 v2 v3 v4             = (p =$= v1 =*= v2 =*= v3 =*= v4 |<<<)
map54 p v1 v2 v3 v4 v5          = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<<<)
map64 p v1 v2 v3 v4 v5 v6       = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<<<)
map74 p v1 v2 v3 v4 v5 v6 v7    = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<<<)
map84 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =$= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 =*= v8 |<<<)

red1 p v1                      = p =\= v1
red2 p v1 v2                   = map21 p v1 (tail v2) =<<= first v2
red3 p v1 v2 v3                = map31 p v1 v2 (tail v3) =<<= first v3
red4 p v1 v2 v3 v4             = map41 p v1 v2 v3 (tail v4) =<<= first v4
red5 p v1 v2 v3 v4 v5          = map51 p v1 v2 v3 v4 (tail v5) =<<= first v5
red6 p v1 v2 v3 v4 v5 v6       = map61 p v1 v2 v3 v4 v5 (tail v6) =<<= first v6
red7 p v1 v2 v3 v4 v5 v6 v7    = map71 p v1 v2 v3 v4 v5 v6 (tail v7) =<<= first v7
red8 p v1 v2 v3 v4 v5 v6 v7 v8 = map81 p v1 v2 v3 v4 v5 v6 v7 (tail v8) =<<= first v8

red1' p i v1                      = p =\= v1 <: i 
red2' p i v1 v2                   = map21 p v1 v2 =<<= i
red3' p i v1 v2 v3                = map31 p v1 v2 v3 =<<= i
red4' p i v1 v2 v3 v4             = map41 p v1 v2 v3 v4 =<<= i
red5' p i v1 v2 v3 v4 v5          = map51 p v1 v2 v3 v4 v5 =<<= i
red6' p i v1 v2 v3 v4 v5 v6       = map61 p v1 v2 v3 v4 v5 v6 =<<= i
red7' p i v1 v2 v3 v4 v5 v6 v7    = map71 p v1 v2 v3 v4 v5 v6 v7 =<<= i
red8' p i v1 v2 v3 v4 v5 v6 v7 v8 = map81 p v1 v2 v3 v4 v5 v6 v7 v8 =<<= i

pref1 ps s = map11 (=<<= s) (inits ps)

-- Skeletons proven by injectivity (equivalent factorized forms exist)

tail Null    = error "tail: Vector is empty"
tail (x:>xs) = xs

init Null      = error "init: Vector is empty"
init (_:>Null) = Null
init (v:>vs)   = v :> init vs

-- Permutators

last    :: Vector a -> a
first   :: Vector a -> a
reverse :: Vector a -> Vector a
inits   :: Vector a -> Vector (Vector a)
tails   :: Vector a -> Vector (Vector a)
length  :: Vector a -> Int

length  = red (+) . map (\_ -> 1)
concat  = red (<++>)
first   = red (\x y -> x)
last    = red (\x y -> y)
reverse = red (\x y -> y <++> x)                      . map unit
inits   = red (\x y -> x <++> map (last  x <++>) y) . map (unit . unit)
tails   = red (\x y -> map (<++> first y) x <++> y) . map (unit . unit)

-- Index-based selectors

get ix  = red2' (\i x y -> if i == ix then Just x else y) Nothing indexes 
take n  = red2' (\i x y -> if i < n then x <++> y else x) Null    indexes . map unit
drop n  = red2' (\i x y -> if i > n then x <++> y else y) Null    indexes . map unit

v <@>  ix = get v ix
v <@!> ix = fromJust $ get v ix


group n v = map (take n) $ pref1 dropseries v
  where dropseries = unit id <++> fanoutn nstages (drop n)
        nstages    = ceiling $ fromIntegral (length v) / fromIntegral n - 1


-- tail      = (<@!> 2) . tails
-- init      = (<@!> 2) . reverse . inits
-- group n  = red2' (\i x y -> if i `mod` n == 0 then x <++> y else x <++> y ) Null indexes . map (unit . unit)


-- nullV Null   = True
-- nullV _             = False

-- replaceV vs n x 
--     | n <= lengthV vs && n >= 0 = takeV n vs <+> unitV x 
--      <+> dropV (n+1) vs
--     | otherwise                 =  vs

-- selectV f s n vs | n <= 0               
--                      = Null
--                  | (f+s*n-1) > lengthV vs 
--                     = error "selectV: Vector has not enough elements"
--                  | otherwise            
--                     = atV vs f :> selectV (f+s) s (n-1) vs


-- foldlV _ a Null   = a
-- foldlV f a (x:>xs) = foldlV f (f a x) xs

-- foldrV _ a Null   = a 
-- foldrV f a (x:>xs) = f x (foldrV f a xs)

-- filterV _ Null   = Null
-- filterV p (v:>vs) = if (p v) then
--                      v :> filterV p vs
--                   else 
--                      filterV p vs

-- shiftlV vs v = v :> initV vs

-- shiftrV vs v = tailV vs <: v

-- rotrV Null = Null
-- rotrV vs    = tailV vs <: headV vs

-- rotlV Null = Null
-- rotlV vs    = lastV vs :> initV vs

-- generateV 0 _ _ = Null
-- generateV n f a = x :> generateV (n-1) f x 
--                 where x = f a

-- iterateV 0 _ _ = Null
-- iterateV n f a = a :> iterateV (n-1) f (f a)

-- copyV k x = iterateV k id x 














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
