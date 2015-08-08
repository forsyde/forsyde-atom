{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Vector
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
module ForSyDe.Core.Vector ( 
  Vector (..), 
  -- ** Basic operators
  (§>), (<§>), pipeV, scanV,
  -- ** functions
  vector, fromVector, unitV, nullV, lengthV,
  atV, getV, replaceV, headV, tailV, lastV, initV, takeV, dropV, 
  selectV, groupV, (<+>), (<:), mapV, foldlV, foldrV, 
  zipWithV, filterV, zipV, unzipV, 
  concatV, reverseV, shiftlV, shiftrV, rotrV, rotlV, 
  generateV, iterateV, copyV 
) where


infixr 3 :>
infixl 5 <:
infixr 5 <+>
infixl 4 §>, <§>

-- | The data type 'Vector' is modeled similar to a list. As described in the core library,
--   it is only a data container used in functions.
--
--   NOT TRUE: It may only be carried as token of signals, and __MUST NOT__ enclose signals as such,  
--   unless the 'ForSyDe.Patterns.Patterns' module is explicitly loaded. Only then 'Vector' enables 
--   signals to be bundled in arbitrary forms and has methods for exploiting process network
--   patterns.
data Vector a = NullV
              | a :> (Vector a) deriving (Eq)


instance Functor Vector where
  fmap _ NullV   = NullV
  fmap f (x:>xs) = f x :> fmap f xs

instance Applicative Vector where
  pure x = x :> pure x
  _         <*> NullV     = NullV
  NullV     <*> _         = NullV
  (f :> fs) <*> (x :> xs) = f x :> fs <*> xs

instance Foldable Vector where
  foldr f z NullV     = z
  foldr f z (x :> xs) = f x (foldr f z xs)


-- | operator for functional application on vectors
(§>) :: (a -> b) -> Vector a -> Vector b 
(§>) = (<$>)

-- | operator for zip-applying (zapp) two vectors
(<§>) :: Vector (a -> b) -> Vector a -> Vector b 
(<§>) = (<*>)


-- | The 'pipe' function constructs a serial composition from a vector of functions.
--
-- __OBS__: composition is right associative thus the input vector should contain functions from right to left
pipeV         :: Vector (b -> b) -> b -> b
pipeV NullV   = id
pipeV (v:>vs) = v . pipeV vs 

-- | The 'scan' function constructs a parallel prefix vector from a vector of functions.
--
-- __OBS__: composition is right associative thus the input vector should contain functions from right to left
scanV         :: Vector (b -> b) -> b -> Vector b
scanV NullV   = pure NullV  
scanV (x:>xs) = (:>) <$> x . pipeV xs <*> scanV xs


-- | The function 'vector' converts a list into a vector.
vector     :: [a] -> Vector a

-- | The function 'fromVector' converts a vector into a list.
fromVector :: Vector a -> [a]

-- | The function 'unitV' creates a vector with one element. 
unitV    :: a -> Vector a

-- | The function 'nullV' returns 'True' if a vector is empty. 
nullV    :: Vector a -> Bool

-- | The function 'lengthV' returns the number of elements in a value. 
lengthV  :: Vector a -> Int

-- | The function 'atV' returns the n-th element in a vector, starting from zero.
atV      :: (Num a, Eq a) => Vector b -> a -> b

-- | The function 'atV' returns the n-th element in a vector, starting from zero.
getV   :: (Num a, Eq a) => a -> Vector b -> b

-- |  The function 'replaceV' replaces an element in a vector.
replaceV :: Vector a -> Int -> a -> Vector a

-- | The functions 'headV' returns the first element of a vector.
headV :: Vector a -> a

-- | The function 'lastV' returns the last element of a vector.
lastV :: Vector a -> a

-- | The functions 'tailV' returns all, but the first element of a vector.
tailV :: Vector a -> Vector a 

-- | The function 'initV' returns all but the last elements of a vector.
initV :: Vector a -> Vector a 

-- | The function 'takeV' returns the first n elements of a vector.
takeV :: (Num a, Ord a) => a -> Vector b -> Vector b

-- | The function 'dropV' drops the first n elements of a vector.
dropV :: (Num a, Ord a) => a -> Vector b -> Vector b



-- | The function 'selectV' selects elements in the vector. The first argument gives the initial element, starting from zero, the second argument gives the stepsize between elements and the last argument gives the number of elements. 
selectV :: Int -> Int -> Int -> Vector a -> Vector a


-- | The function 'groupV' groups a vector into a vector of vectors of size n.
groupV :: Int -> Vector a -> Vector (Vector a)

-- | The operator '(<:)' adds an element at the end of a vector.
(<:)  :: Vector a -> a -> Vector a

-- | The operator '(<+>)' concatinates two vectors.
(<+>) :: Vector a -> Vector a -> Vector a




-- | The higher-order function 'mapV' applies a function on all elements of a vector.
mapV :: (a -> b) -> Vector a -> Vector b    


-- | The higher-order function 'zipWithV' applies a function pairwise on to vectors.
zipWithV :: (a -> b -> c) -> Vector a -> Vector b -> Vector c


-- | The higher-order functions 'foldlV' folds a function from the right to the left  over a vector using an initial value.
foldlV :: (a -> b -> a) -> a -> Vector b -> a 

-- | The higher-order functions 'foldrV' folds a function from the left to the right over a vector using an initial value.
foldrV :: (b -> a -> a) -> a -> Vector b -> a

-- | The higher-function 'filterV' takes a predicate function and a vector and creates a new vector with the elements for which the predicate is true. 
filterV :: (a -> Bool) -> Vector a -> Vector a

-- | The function 'zipV' zips two vectors into a vector of tuples.
zipV   :: Vector a -> Vector b -> Vector (a, b)

-- | The function 'unzipV' unzips a vector of tuples into two vectors.
unzipV :: Vector (a, b) -> (Vector a, Vector b)



-- | The function 'shiftlV' shifts a value from the left into a vector. 
shiftlV :: Vector a -> a-> Vector a 

-- | The function 'shiftrV' shifts a value from the right into a vector. 
shiftrV :: Vector a -> a -> Vector a

-- | The function 'rotlV' rotates a vector to the left. Note that this fuctions does not change the size of a vector.
rotlV   :: Vector a -> Vector a

-- | The function 'rotrV' rotates a vector to the right. Note that this fuction does not change the size of a vector.
rotrV   :: Vector a -> Vector a


-- | The function 'concatV' transforms a vector of vectors to a single vector. 
concatV   :: Vector (Vector a) -> Vector a

-- | The function 'reverseV' reverses the order of elements in a vector. 
reverseV  :: Vector a -> Vector a


-- | The function 'iterateV' generates a vector with a given number of elements starting from an initial element using a supplied function for the generation of elements. 
--
-- > Vector> iterateV 5 (+1) 1
--
-- > <1,2,3,4,5> :: Vector Integer
iterateV :: (Num a, Eq a) => a -> (b -> b) -> b -> Vector b

-- | The function 'generateV' behaves in the same way, but starts with the application of the supplied function to the supplied value. 
--
-- > Vector> generateV 5 (+1) 1
-- 
-- > <2,3,4,5,6> :: Vector Integer
generateV :: (Num a, Eq a) => a -> (b -> b) -> b -> Vector b

-- | The function 'copyV' generates a vector with a given number of copies of the same element. 
--
-- > Vector> copyV 7 5 
-- 
-- > <5,5,5,5,5,5,5> :: Vector Integer
copyV     :: (Num a, Eq a) => a -> b -> Vector b


-- | The vector 1 :> 2 :> NullV is represented as \<1,2\>.
instance (Show a) => Show (Vector a) where
  showsPrec p = showParen (p>1) . showVector
    where
      showVector (x :> xs)  = showChar '<' . showEvent x . showVector' xs
      showVector (NullV)   = showChar '<' . showChar '>'
      showVector' (x :> xs) = showChar ',' . showEvent x . showVector' xs
      showVector' (NullV)  = showChar '>'
      showEvent x           = shows x

-- | The vector 1 :> 2 >- NullV is read using the string \"\<1,2\>\".
instance (Read a) => Read (Vector a) where
  readsPrec d = readParen (d>1) readVecSigtart
    where
      readVecSigtart = (\ a -> [(xs,c) | ("<",b) <- lex a , (xs,c) <- readVector (',' : b) ++ readNull b])
      readVector r    = readEvent r ++ readNull r
      readEvent a     = [(x :> xs,d) | (",",b) <- lex a , (x,c) <- reads b , (xs,d) <- readVector c]
      readNull a      = [(NullV,b) | (">",b) <- lex a]

vector []     = NullV
vector (x:xs) = x :> (vector xs)

fromVector NullV   = []
fromVector (x:>xs) = x : fromVector xs

unitV x = x :> NullV

nullV NullV   = True
nullV _             = False

lengthV NullV   = 0
lengthV (_:>xs) = 1 + lengthV xs

replaceV vs n x 
    | n <= lengthV vs && n >= 0 = takeV n vs <+> unitV x 
     <+> dropV (n+1) vs
    | otherwise                 =  vs

NullV   `atV` _ = error "atV: Vector has not enough elements"
(x:>_)  `atV` 0 = x
(_:>xs) `atV` n = xs `atV` (n-1)

getV a b = atV b a

headV NullV   = error "headV: Vector is empty"
headV (v:>_) = v

tailV NullV   = error "tailV: Vector is empty"
tailV (_:>vs) = vs

lastV NullV      = error "lastV: Vector is empty"
lastV (v:>NullV) = v
lastV (_:>vs)    = lastV vs

initV NullV      = error "initV: Vector is empty"
initV (_:>NullV) = NullV
initV (v:>vs)    = v :> initV vs

takeV 0 _                   = NullV
takeV _ NullV               = NullV
takeV n (v:>vs) | n <= 0    = NullV
                | otherwise = v :> takeV (n-1) vs

dropV 0 vs                  = vs
dropV _ NullV               = NullV
dropV n (v:>vs) | n <= 0    = v :> vs
                | otherwise = dropV (n-1) vs

selectV f s n vs | n <= 0               
                     = NullV
                 | (f+s*n-1) > lengthV vs 
                    = error "selectV: Vector has not enough elements"
                 | otherwise            
                    = atV vs f :> selectV (f+s) s (n-1) vs

groupV n v 
      | lengthV v < n = NullV
      | otherwise     = selectV 0 1 n v 
                        :> groupV n (selectV n 1 (lengthV v-n) v)

NullV <+> ys   = ys
(x:>xs) <+> ys = x :> (xs <+> ys) 

xs <: x = xs <+> unitV x         

mapV _ NullV   = NullV
mapV f (x:>xs) = f x :> mapV f xs

zipWithV f (x:>xs) (y:>ys) = f x y :> (zipWithV f xs ys)
zipWithV _ _       _       = NullV

foldlV _ a NullV   = a
foldlV f a (x:>xs) = foldlV f (f a x) xs

foldrV _ a NullV   = a 
foldrV f a (x:>xs) = f x (foldrV f a xs)

filterV _ NullV   = NullV
filterV p (v:>vs) = if (p v) then
                     v :> filterV p vs
                  else 
                     filterV p vs

zipV (x:>xs) (y:>ys) = (x, y) :> zipV xs ys
zipV _       _       = NullV

unzipV NullV           = (NullV, NullV)
unzipV ((x, y) :> xys) = (x:>xs, y:>ys) 
 where (xs, ys) = unzipV xys

shiftlV vs v = v :> initV vs

shiftrV vs v = tailV vs <: v

rotrV NullV = NullV
rotrV vs    = tailV vs <: headV vs

rotlV NullV = NullV
rotlV vs    = lastV vs :> initV vs

concatV = foldrV (<+>) NullV

reverseV NullV   = NullV
reverseV (v:>vs) = reverseV vs <: v

generateV 0 _ _ = NullV
generateV n f a = x :> generateV (n-1) f x 
                where x = f a

iterateV 0 _ _ = NullV
iterateV n f a = a :> iterateV (n-1) f (f a)

copyV k x = iterateV k id x 

