{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skeleton.Vector
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the data type 'Vector' as a categorical type,
-- implementing the atoms for the 'ForSyDe.MoC.Skeleton.Skeleton'
-- class. Algorithmic skeletons are mostly described for 'Vector' in
-- their factorized form (i.e. as a /map-reduce/ pattern, using
-- skeleton layer atoms), assuring that they are catamorphisms. Where
-- practical matters, or efficience are a concern, some skeletons are
-- implemented as recurrences. One can still prove that they are
-- catamorphisms through alternative theorems (e.g. by proving that
-- they have an inverse over 'Vector').
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skeleton.Vector (

  -- * Vector data type

  Vector(..),

  -- * \"Constructors\"

  -- | According to <#bird96 [2]>, 'Vector' should be implemented as
  -- following:
  --
  -- > data Vector a = Null                   -- null element
  -- >               | Unit a                 -- singleton vector
  -- >               | Vector a <++> Vector a -- concatenate two vectors
  --
  -- This construction suggests the possibility of splitting a
  -- 'Vector' into multiple parts and evaluating it in parallel. While
  -- due to practical considerations we have chosen another
  -- implementation (see above), when defining skeletons on vectors we
  -- shall use this particular interpretation. Therefore @Null@,
  -- @Unit@ and @\<++\>@ are provided as functions.
  null, unit, (<++>),

  -- * Utilities

  vector, fromVector, indexes, isNull, (<:),

  -- * Skeletons

  -- | Algorithmic skeletons on vectors are mainly presented in terms
  -- of compositions of the atoms associated with the
  -- 'Skeleton' Layer. When defining them,
  -- we use the following operators:
  --
  -- <<includes/figs/skel-operators-formula.png>>
  --
  -- where:
  --
  -- * (1) is the 'unit' constructor, constructing a singleton vector.
  -- * (2) is the '<++>' constructor, concatenating two vectors.
  -- * (3) is the '<@!>' selector. The subscript notation is used to
  -- denote element at position /n/ in a vector.
  -- * (4) suggests an arbitrary selector which returns a vector with
  -- another one's elements, based on some indices. The shown example
  -- is an alternative notation for the 'tail' skeleton.
  
  -- ** Functional

  -- | This sub-category denotes skeletons (i.e. Skeleton Layer
  -- patterns) which are capable of constructing parallel process
  -- networks from processes (i.e. MoC layer entities) passed as
  -- arguments. Using the applicative mechanism, the designer has a
  -- high degree of freedom when customizing process networks through
  -- systematic partial application, rendering numerous possible
  -- usages for the same patterns. To avoid over-encumberating our
  -- examples with otherwise optional details, only 'map22' and 'red2'
  -- depict a somewhat complete scenario, the rest of the patterns
  -- assuming fully-applied processes as arguments.
  --
  -- Due to Haskell's strict type system and the implementation
  -- mechanisms, we need to provide separate constructors @skelXY@,
  -- where @skel@ is the process network constructor type, @X@ is the
  -- number of inputs and @Y@ is the number of outputs. This module
  -- provides constructors with @ X = [0..8]@ and @ Y = [1..4]@. If
  -- needed, the designer is free to implement her own constructor by
  -- following the atom composition rules in the source code.

  map11, map12, map13, map14,
  map21, map22, map23, map24,
  map31, map32, map33, map34,
  map41, map42, map43, map44,
  map51, map52, map53, map54,
  map61, map62, map63, map64,
  map71, map72, map73, map74,
  map81, map82, map83, map84,

  red1,  red2,  red3,  red4,  red5,  red6,  red7,  red8,
  red1', red2', red3', red4', red5', red6', red7', red8',

  pref1,  pref2,  pref3,  pref4,  pref5,  pref6, 
  pref1', pref2', pref3', pref4', pref5', pref6',

  suf1,  suf2,  suf3,  suf4,  suf5,  suf6,  
  suf1', suf2', suf3', suf4', suf5', suf6',

  pipe1, pipe2, pipe3, pipe4, pipe5, pipe6, pipe7, pipe8,
  
  
  -- ** Transitional

  -- ** Interfaces
  
  -- * Bibliography
  
  -- | #gorlatch03# <http://link.springer.com/chapter/10.1007/978-1-4471-0097-3_1#page-1 [1]> Fischer, J., Gorlatch, S., & Bischof, H. (2003). Foundations of data-parallel skeletons. In /Patterns and skeletons for parallel and distributed computing/ (pp. 1-27). Springer London.
  
  -- | #bird96# [2] Bird, R., & De Moor, O. (1996, January). The algebra of programming. In /NATO ASI DPD/ (pp. 167-203).
  
  -- | #skillicorn05# <https://books.google.se/books?hl=ro&lr=&id=rQwsL5xsZigC&oi=fnd&pg=PP1&dq=skillicorn+foundation+parallel+programming&ots=UJMBr0uO2Q&sig=ncyXxE0gFNkUZwVOYyFb_ezWlGY&redir_esc=y#v=onepage&q=skillicorn%20foundation%20parallel%20programming&f=false [3]> Skillicorn, D. B. (2005). Foundations of parallel programming (No. 6). Cambridge University Press.

  -- | #reekie95# <http://ptolemy.eecs.berkeley.edu/~johnr/papers/pdf/thesis.pdf [4]> Reekie, H. J. (1995). Realtime signal processing: Dataflow, visual, and functional programming.
  ) where

import ForSyDe.Atom.Skeleton.Vector.Core
import ForSyDe.Atom.Skeleton.Vector.Lib
import ForSyDe.Atom.Skeleton.Vector.Interface

import Prelude hiding (null, last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)





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


-- a = vector [1,2,3,4,5,6,7,8,9]
-- b = a <++> fanout 15
