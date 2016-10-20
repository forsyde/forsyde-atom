{-# LANGUAGE PostfixOperators #-}
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
-- The library for the 'Vector' type. Contains the main skeletons.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skeleton.Vector.Lib where

import Data.Maybe
import ForSyDe.Atom.Skeleton
import ForSyDe.Atom.Utility
import Prelude hiding (null, last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)

import ForSyDe.Atom.Skeleton.Vector.Core

-------------------------
-- Functional networks --
-------------------------

map  :: (a -> b) -> Vector a -> Vector b
red  :: (a -> a -> a) -> Vector a -> a
pipe :: Vector (a -> a) -> a -> a
scan :: Vector (a -> a) -> a -> Vector a
map  = (=$=)
red  = (=\=)
pipe = (=<<=)
scan  ps s = map (=<<= s) (inits ps)
scan' ps s = map (=<<= s) (inits $ unit id <++> ps)

-- | @map@ maps a function on a vector (See also: '=$=', '=*=', '|<').
--
-- If the vector contains signals, then the function is, naturally, a
-- process. The example below shows a typical case when the function
-- passed is a /process constructor/ rather than a fully applied
-- process. Using the same powerful applicative mechanism, the process
-- constructor is systematically applied to the first vectors of
-- values, yelding an intermediate vector of processes. This vector of
-- processes is then furher applied to the subsequent vectors of
-- signals, constructing the /data-parallel/ process network depicted
-- below.
--
-- <<includes/figs/skel-map-formula.png>> <<includes/figs/skel-map-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- >  map11, map12, map13, map14,
-- >  map21, map22, map23, map24,
-- >  map31, map32, map33, map34,
-- >  map41, map42, map43, map44,
-- >  map51, map52, map53, map54,
-- >  map61, map62, map63, map64,
-- >  map71, map72, map73, map74,
-- >  map81, map82, map83, map84,
map22 :: (a1 -> a2 -> (b1, b2))
         -- ^ @function22@. if /a/ and /b/ are signals, then this
         -- should be @process22@. See "ForSyDe.Atom.MoC".
         -> Vector a1              -- ^ first input vector
         -> Vector a2              -- ^ second input vector
         -> (Vector b1, Vector b2) -- ^ two output vectors

-- | @red@ reduces a vector to an element based on an associative
-- function. (See also: '=\=', '=<<=', 'map22')
--
-- Again, if the vector that needs to be reduced contains signals,
-- then the function passed as the first argument is a process. As
-- with 'map22', one can systematically build the reduction vector of
-- processes in an applicative manner, by applying a process
-- constructor to a series of vectors until a two-input associative
-- process is obtained. A typical example is depicted below:
--
-- <<includes/figs/skel-red-formula.png>> <<includes/figs/skel-red-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > reduce1, reduce2, reduce3, reduce4, reduce5, re6, reduce7, reduce8,
--
-- __OBS:__ we use the pipe operator '=<<=' which is in fact a special
-- case of the reduce operator '=\=', which allows partial application
-- of functions with an rbitrary number of inputs.
reduce2 :: (a1 -> a2 -> a2 -> a2)
        -- ^ @function31@, which is a constructor for an associative
        -- @function21@ (@a2 -> a2 -> a2@).  @a1@ is obtained from the
        -- first vector, whereas the last two arguments of type @a2@
        -- belong to the second vector, which is being reduced.
        -> Vector a1  -- ^ first input vector
        -> Vector a2  -- ^ second input vector
        -> a2         -- ^ the second input vector reduced to one element

-- | @red'@ is a variant of @red@ which takes a separate initial
-- element. (See also: 'reduce2', '=\=', '=<<=', 'map22')
--
-- For a typical usage example, check the documentation for 'reduce2'.
--
-- <<includes/figs/skel-redp-formula.png>> 
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > reduce1', reduce2', reduce3', reduce4', reduce5', re6', reduce7', reduce8',
--
-- __OBS:__ we use the pipe operator '=<<=' which is in fact a special
-- case of the reduce operator '=\=', which allows partial application
-- of functions with an rbitrary number of inputs.
reduce2' :: (a1 -> a2 -> a2 -> a2)
        -- ^ @function31@, which is a constructor for an associative
        -- @function21@ (@a2 -> a2 -> a2@).  @a1@ is obtained from the
        -- first vector, whereas the last two arguments of type @a2@
        -- belong to the second vector, which is being reduced.
        -> a2         -- ^ initial element, which shall be piped into
                      -- the reduction network.
        -> Vector a1  -- ^ first input vector
        -> Vector a2  -- ^ second input vector
        -> a2         -- ^ the second input vector reduced to one element

-- | @pref@ peforms the /parallel prefix/ operation on a vector. (See
-- also: 'reduce2', 'map22', 'inits')
--
-- In case the last input vector contains signals, then @pref@
-- constructs the following process network, where the process passed
-- as argument is assumed to be fully applied, and to take exactly two
-- signals as input (See 'map22' and 'reduce2' for a more generic
-- example).
--
-- <<includes/figs/skel-pref-formula.png>> <<includes/figs/skel-pref-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > prefix1,  prefix2,  prefix3,  prefix4,  prefix5,  prefix6, 
prefix2 :: (a -> b -> b -> b)
         -- ^ @function31@, which is a constructor for an associative
         -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
         -- first vector, whereas the last two arguments of type @b@
         -- belong to the second vector, which is being reduced.
         -> Vector a  -- ^ first input vector
         -> Vector b  -- ^ second input vector
         -> Vector b  -- ^ parallel prefix on the second input vector 


-- | @prefix'@ is a variant of @prefix@ which takes an initial
-- element. (See also: 'prefix2', 'reduce2', 'map22', 'inits')
--
-- See the example for 'prefix2'.
--
-- <<includes/figs/skel-prefp-formula.png>> 
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > prefix1',  prefix2',  prefix3',  prefix4',  prefix5',  prefix6', 
prefix2' :: (a -> b -> b -> b)
          -- ^ @function31@, which is a constructor for an associative
          -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
          -- first vector, whereas the last two arguments of type @b@
          -- belong to the second vector, which is being reduced.
          -> b         -- ^ initial element, which shall be piped into
                       -- the reduction network.
          -> Vector a  -- ^ first input vector
          -> Vector b  -- ^ second input vector
          -> Vector b  -- ^ parallel prefix on the second input vector 

-- | @suffix@ peforms the /parallel suffix/ operation on a vector. (See
-- also: 'reduce2', 'map22', 'tails')
--
-- In case the last input vector contains signals, then @suffix@
-- constructs the following process network, where the process passed
-- as argument is assumed to be fully applied, and to take exactly two
-- signals as input (See 'map22' and 'reduce2' for a more generic
-- example).
--
-- <<includes/figs/skel-suf-formula.png>> <<includes/figs/skel-suf-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > suffix1,  suffix2,  suffix3,  suffix4,  suffix5,  suffix6, 
suffix2 :: (a -> b -> b -> b)
         -- ^ @function31@, which is a constructor for an associative
         -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
         -- first vector, whereas the last two arguments of type @b@
         -- belong to the second vector, which is being reduced.
         -> Vector a  -- ^ first input vector
         -> Vector b  -- ^ second input vector
         -> Vector b  -- ^ parallel suffix on the second input vector 


-- | @suffix'@ is a variant of @suffix@ which takes an initial
-- element. (See also: 'suffix2', 'reduce2', 'map22', 'tails')
--
-- See the example for 'suffix2'.
--
-- <<includes/figs/skel-sufp-formula.png>> 
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below. To
-- create your own constructors please follow the examples set in the
-- source code.
--
-- > suffix1',  suffix2',  suffix3',  suffix4',  suffix5',  suffix6', 
suffix2' :: (a -> b -> b -> b)
          -- ^ @function31@, which is a constructor for an associative
          -- @function21@ (@b -> b -> b@).  @a@ is obtained from the
          -- first vector, whereas the last two arguments of type @b@
          -- belong to the second vector, which is being reduced.
          -> b         -- ^ initial element, which shall be piped into
                       -- the reduction network.
          -> Vector a  -- ^ first input vector
          -> Vector b  -- ^ second input vector
          -> Vector b  -- ^ parallel suffix on the second input vector 

-- | @pipe@ creates a pipeline of functions from a vector. While
-- itself is a special case of the reduction operaton '=\=', it
-- constructs a more flexible and robust process network (See also:
-- 'reduce2'', 'map22', '=<<=').
--
-- The following example assumes fully applied processes that take
-- exactly one signal as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-pipe-atom-formula.png>> <<includes/figs/skel-pipe-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below,
-- which (except 'pipe0') are the functional equivalent of 'reduce2''
-- but with flipped arguments. Check the interfaces in @ghci@ to see
-- their usage. To create your own constructors please follow the
-- examples set in the source code.
--
-- > pipe0, pipe1, pipe2, pipe3, pipe4, pipe5, pipe6, pipe7, pipe8,
pipe0 :: Vector (a -> a) -- ^ vector of functions
      -> a               -- ^ input
      -> a               -- ^ output 



-- | @systolic@ creates a systolic array of functions from a
-- vector. While itself is a generalization of the parallel prefix
-- 'prefix2', it constructs a more flexible and robust process network
-- (See also: 'prefix2'', 'map22', '=<<=').
--
-- The following example assumes fully applied processes that take
-- exactly one signal as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-systolic-formula.png>> <<includes/figs/skel-systolic-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors below,
-- which (except 'systolic0') are the functional equivalent of
-- 'prefix2'' but with flipped arguments. Check the interfaces in
-- @ghci@ to see their usage. To create your own constructors please
-- follow the examples set in the source code.
--
-- > systolic0, systolic1, systolic2, systolic3, systolic4, systolic5, systolic6, systolic7, systolic8,
systolic0 :: Vector (a -> a) -- ^ vector of functions
      -> a               -- ^ input
      -> Vector a        -- ^ output 

-- | @cascade@ creates a \"cascading mesh\" as a result of piping a
-- vector into a vector of 1D systolic arrays. 
--
-- The following example assumes fully applied processes that take
-- exactly two signals as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-cascade-formula.png>> <<includes/figs/skel-cascade-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors
-- below. Check the interfaces in @ghci@ to see their usage. To create
-- your own constructors please follow the examples set in the source
-- code.
--
-- > cascade0, cascade1, cascade2, cascade3, cascade4, 
cascade2 :: (a2 -> a1 -> a -> a -> a)  -- ^ @function41@ which needs to be applied to @function21@
         -> Vector (Vector a2)         -- ^ fills in the first argument in the function above
         -> Vector (Vector a1)         -- ^ fills in the second argument in the function above
         -> Vector a                   -- ^ first input vector (e.g. of signals)
         -> Vector a                   -- ^ second input vector (e.g. of signals)
         -> Vector a                   -- ^ output

-- | @mesh@ creates a 2D systolic array, similar to 'cascade2'.
--
-- The following example assumes fully applied processes that take
-- exactly two signals as input (See 'map22' and 'reduce2' for a more
-- generic example).
--
-- <<includes/figs/skel-mesh-formula.png>> <<includes/figs/skel-mesh-graph.png>>
--
-- "ForSyDe.Atom.Skeleton.Vector" exports the constructors
-- below. Check the interfaces in @ghci@ to see their usage. To create
-- your own constructors please follow the examples set in the source
-- code.
--
-- > mesh0, mesh1, mesh2, mesh3, mesh4, 
mesh2 :: (a2 -> a1 -> a -> a -> a)  -- ^ @function41@ which needs to be applied to @function21@
      -> Vector (Vector a2)         -- ^ fills in the first argument in the function above
      -> Vector (Vector a1)         -- ^ fills in the second argument in the function above
      -> Vector a                   -- ^ first input vector (e.g. of signals)
      -> Vector a                   -- ^ second input vector (e.g. of signals)
      -> Vector (Vector a)          -- ^ output, a 2D vector

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

reduce1 p v1                      = p =\= v1
reduce2 p v1 v2                   = map21 p v1 (tail v2) =<<= first v2
reduce3 p v1 v2 v3                = map31 p v1 v2 (tail v3) =<<= first v3
reduce4 p v1 v2 v3 v4             = map41 p v1 v2 v3 (tail v4) =<<= first v4
reduce5 p v1 v2 v3 v4 v5          = map51 p v1 v2 v3 v4 (tail v5) =<<= first v5
reduce6 p v1 v2 v3 v4 v5 v6       = map61 p v1 v2 v3 v4 v5 (tail v6) =<<= first v6
reduce7 p v1 v2 v3 v4 v5 v6 v7    = map71 p v1 v2 v3 v4 v5 v6 (tail v7) =<<= first v7
reduce8 p v1 v2 v3 v4 v5 v6 v7 v8 = map81 p v1 v2 v3 v4 v5 v6 v7 (tail v8) =<<= first v8

reduce1' p i v1                      = p =\= v1 <: i 
reduce2' p i v1 v2                   = map21 p v1 v2 =<<= i
reduce3' p i v1 v2 v3                = map31 p v1 v2 v3 =<<= i
reduce4' p i v1 v2 v3 v4             = map41 p v1 v2 v3 v4 =<<= i
reduce5' p i v1 v2 v3 v4 v5          = map51 p v1 v2 v3 v4 v5 =<<= i
reduce6' p i v1 v2 v3 v4 v5 v6       = map61 p v1 v2 v3 v4 v5 v6 =<<= i
reduce7' p i v1 v2 v3 v4 v5 v6 v7    = map71 p v1 v2 v3 v4 v5 v6 v7 =<<= i
reduce8' p i v1 v2 v3 v4 v5 v6 v7 v8 = map81 p v1 v2 v3 v4 v5 v6 v7 v8 =<<= i

prefix1 p                   = map11 (reduce1 p) . inits
prefix2 p v1 v2             = map21 (reduce2 p) (unit v1) (inits v2)
prefix3 p v1 v2 v3          = map31 (reduce3 p) (unit v1) (unit v2) (inits v3)
prefix4 p v1 v2 v3 v4       = map41 (reduce4 p) (unit v1) (unit v2) (unit v3) (inits v4)
prefix5 p v1 v2 v3 v4 v5    = map51 (reduce5 p) (unit v1) (unit v2) (unit v3) (unit v4) (inits v5)
prefix6 p v1 v2 v3 v4 v5 v6 = map61 (reduce6 p) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (inits v6)

prefix1' p i                   = map11 (reduce1' p i) . inits
prefix2' p i v1 v2             = map21 (reduce2' p i) (unit v1) (inits v2)
prefix3' p i v1 v2 v3          = map31 (reduce3' p i) (unit v1) (unit v2) (inits v3)
prefix4' p i v1 v2 v3 v4       = map41 (reduce4' p i) (unit v1) (unit v2) (unit v3) (inits v4)
prefix5' p i v1 v2 v3 v4 v5    = map51 (reduce5' p i) (unit v1) (unit v2) (unit v3) (unit v4) (inits v5)
prefix6' p i v1 v2 v3 v4 v5 v6 = map61 (reduce6' p i) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (inits v6)

suffix1 p                   = map11 (reduce1 p) . tails
suffix2 p v1 v2             = map21 (reduce2 p) (unit v1) (tails v2)
suffix3 p v1 v2 v3          = map31 (reduce3 p) (unit v1) (unit v2) (tails v3)
suffix4 p v1 v2 v3 v4       = map41 (reduce4 p) (unit v1) (unit v2) (unit v3) (tails v4)
suffix5 p v1 v2 v3 v4 v5    = map51 (reduce5 p) (unit v1) (unit v2) (unit v3) (unit v4) (tails v5)
suffix6 p v1 v2 v3 v4 v5 v6 = map61 (reduce6 p) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (tails v6)

suffix1' p i                   = map11 (reduce1' p i) . tails
suffix2' p i v1 v2             = map21 (reduce2' p i) (unit v1) (tails v2)
suffix3' p i v1 v2 v3          = map31 (reduce3' p i) (unit v1) (unit v2) (tails v3)
suffix4' p i v1 v2 v3 v4       = map41 (reduce4' p i) (unit v1) (unit v2) (unit v3) (tails v4)
suffix5' p i v1 v2 v3 v4 v5    = map51 (reduce5' p i) (unit v1) (unit v2) (unit v3) (unit v4) (tails v5)
suffix6' p i v1 v2 v3 v4 v5 v6 = map61 (reduce6' p i) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (tails v6)

pipe0                             = pipe
pipe1 p v1 s                      = map11 p v1 `pipe` s
pipe2 p v1 v2 s                   = map21 p v1 v2 `pipe` s
pipe3 p v1 v2 v3 s                = map31 p v1 v2 v3 `pipe` s
pipe4 p v1 v2 v3 v4 s             = map41 p v1 v2 v3 v4 `pipe` s
pipe5 p v1 v2 v3 v4 v5 s          = map51 p v1 v2 v3 v4 v5 `pipe` s
pipe6 p v1 v2 v3 v4 v5 v6 s       = map61 p v1 v2 v3 v4 v5 v6 `pipe` s
pipe7 p v1 v2 v3 v4 v5 v6 v7 s    = map71 p v1 v2 v3 v4 v5 v6 v7 `pipe` s
pipe8 p v1 v2 v3 v4 v5 v6 v7 v8 s = map81 p v1 v2 v3 v4 v5 v6 v7 v8 `pipe` s

systolic0                             = scan
systolic1 p v1 s                      = map11 p v1 `scan` s
systolic2 p v1 v2 s                   = map21 p v1 v2 `scan` s
systolic3 p v1 v2 v3 s                = map31 p v1 v2 v3 `scan` s
systolic4 p v1 v2 v3 v4 s             = map41 p v1 v2 v3 v4 `scan` s
systolic5 p v1 v2 v3 v4 v5 s          = map51 p v1 v2 v3 v4 v5 `scan` s
systolic6 p v1 v2 v3 v4 v5 v6 s       = map61 p v1 v2 v3 v4 v5 v6 `scan` s
systolic7 p v1 v2 v3 v4 v5 v6 v7 s    = map71 p v1 v2 v3 v4 v5 v6 v7 `scan` s
systolic8 p v1 v2 v3 v4 v5 v6 v7 v8 s = map81 p v1 v2 v3 v4 v5 v6 v7 v8 `scan` s

cascade0 p                 vs1 vs2 = map11 (\            s2 s1 -> map11 p             s1 `scan` s2)                 vs2 `pipe` vs1
cascade1 p vv1             vs1 vs2 = map21 (\v1          s2 s1 -> map21 p v1          s1 `scan` s2) vv1             vs2 `pipe` vs1
cascade2 p vv1 vv2         vs1 vs2 = map31 (\v1 v2       s2 s1 -> map31 p v1 v2       s1 `scan` s2) vv1 vv2         vs2 `pipe` vs1
cascade3 p vv1 vv2 vv3     vs1 vs2 = map41 (\v1 v2 v3    s2 s1 -> map41 p v1 v2 v3    s1 `scan` s2) vv1 vv2 vv3     vs2 `pipe` vs1
cascade4 p vv1 vv2 vv3 vv4 vs1 vs2 = map51 (\v1 v2 v3 v4 s2 s1 -> map51 p v1 v2 v3 v4 s1 `scan` s2) vv1 vv2 vv3 vv4 vs2 `pipe` vs1

mesh0 p                 vs1 vs2 = map11 (\            s2 s1 -> map11 p             s1 `scan` s2)                 vs2 `scan` vs1
mesh1 p vv1             vs1 vs2 = map21 (\v1          s2 s1 -> map21 p v1          s1 `scan` s2) vv1             vs2 `scan` vs1
mesh2 p vv1 vv2         vs1 vs2 = map31 (\v1 v2       s2 s1 -> map31 p v1 v2       s1 `scan` s2) vv1 vv2         vs2 `scan` vs1
mesh3 p vv1 vv2 vv3     vs1 vs2 = map41 (\v1 v2 v3    s2 s1 -> map41 p v1 v2 v3    s1 `scan` s2) vv1 vv2 vv3     vs2 `scan` vs1
mesh4 p vv1 vv2 vv3 vv4 vs1 vs2 = map51 (\v1 v2 v3 v4 s2 s1 -> map51 p v1 v2 v3 v4 s1 `scan` s2) vv1 vv2 vv3 vv4 vs2 `scan` vs1

-------------
-- Queries --
-------------

-- | returns the number of elements in a value.
--
-- <<includes/figs/skel-length-formula.png>>
length Null = 0
length v    = red (+) . map (\_ -> 1) $ v

----------------
-- Generators --
----------------

-- | 'fanout' repeats an element. As a process network it distributes
-- the same value or signal to all the connected processes down the
-- line. Depending on the target platform and the refinement decisions
-- involved, it may be interpreted in the following implementations:
--
--  * global or shared memory in case of a massively parallel platform
--  (e.g. GPU)
--  * a (static) memory or cache location in memory-driven
--  architectures (e.g. CPU)
--  * a fanout in case of a HDL system
--  * a physical copy or a broadcast in case of a distributed system
--
-- For reasons of efficiency, this skeleton is defined recursively,
-- but can be proven that it is a catamorphism, because:
--
-- > first . fanout = id
fanout x = x :> fanout x

-- | 'fanoutn' is the same as 'fanout', but the length of the result
-- is also provided.
fanoutn n x | n <= 0    = Null
            | otherwise = x :> fanoutn (n-1) x

-- | 'generate' creates a vector based on a kernel function (see also
-- 'systolic0'). E.g.:
--
-- >>> generate 5 (+1) 1
-- > <2,3,4,5,6>
--
-- <<includes/figs/skel-generate-formula.png>>
generate n f i | n <= 0    = Null
               | otherwise = fanoutn n f `scan` i

-- | 'iterate' is a version of 'generate' which keeps the initial
-- element as well (see also 'systolic0').  E.g.:
--
-- >>> iterate 5 (+1) 1
-- > <1,2,3,4,5,6>
iterate n f i | n <= 0     = Null
              | otherwise = fanoutn (n-1) f `scan'` i

---------------
-- Selectors --
---------------

first' Null = Null
first' xs   = first xs
last'  Null = Null
last'  xs   = last xs
init'  Null = Null
init'  xs   = init xs
tail'  Null = Null
tail'  xs   = tail xs

tail Null    = error "tail: Vector is empty"
tail (x:>xs) = xs
-- tail      = (<@!> 2) . tails

init Null      = error "init: Vector is empty"
init (_:>Null) = Null
init (v:>vs)   = v :> init vs
-- init      = (<@!> 2) . reverse . inits

-- | concatenates a vector of vectors.
--
-- <<includes/figs/skel-concat-formula.png>>
concat Null = Null
concat v    = red (<++>) v

-- | returns the first element in a vector. Implemented as a reduction
-- (see source code).
first Null = error "first: empty vector"
first v    = red (\x y -> x) v

-- | returns the last element in a vector. Implemented as a reduction
-- (see source code).
last Null = error "last: empty vector"
last  v   = red (\x y -> y) v

-- | reverses the elements in a vector.
--
-- <<includes/figs/skel-reverse-formula.png>>
-- <<includes/figs/skel-reverse-graph.png>>
reverse Null = Null
reverse v    = red (\x y -> y <++> x) . map unit $ v

-- | creates a vector of all the initial segments in a vector.
--
-- <<includes/figs/skel-inits-formula.png>>
-- <<includes/figs/skel-inits-graph.png>>
inits Null = error "inits: null vector"
inits v    = red (\x y -> x <++> map (last  x <++>) y) . map (unit . unit) $ v

-- | creates a vector of all the final segments in a vector.
--
-- <<includes/figs/skel-tails-formula.png>>
-- <<includes/figs/skel-tails-graph.png>>
tails Null = error "inits: null vector"
tails v    = red (\x y -> map (<++> first y) x <++> y) . map (unit . unit) $ v

-- | returns the /n/-th element in a vector, or @Nothing@ if /n > l/.
get _ Null = Nothing
get n v    = reduce2' (\i x y -> if i == n then x else y) Nothing indexes . map Just $ v

-- | takes the first /n/ elements of a vector.
--
-- <<includes/figs/skel-take-formula.png>>
--
-- >>> take 5 $ vector [1,2,3,4,5,6,7,8,9]
-- > <1,2,3,4,5>
take _ Null = Null
take n v    = reduce2' (\i x y -> if i < n then x <++> y else x) Null indexes . map unit $ v

-- | drops the first /n/ elements of a vector.
--
-- <<includes/figs/skel-drop-formula.png>>
--
-- >>> drop 5 $ vector [1,2,3,4,5,6,7,8,9]
-- > <6,7,8,9>
drop _ Null = Null
drop n v    = reduce2' (\i x y -> if i > n then x <++> y else y) Null indexes . map unit $ v

-- | returns a vector containing only the elements of another vector
-- whose index satisfies a predicate.
--
-- <<includes/figs/skel-filterIdx-formula.png>>
-- <<includes/figs/skel-filterIdx-graph.png>>
filterIdx _ Null = Null
filterIdx f v    = reduce2' (\i x y -> if f i   then x <++> y else y) Null indexes . map unit $ v

-- | replaces the /n/-th element in a vector with another.
--
-- <<includes/figs/skel-replace-formula.png>>
--
-- >>> replace 5 15 $ vector [1,2,3,4,5,6,7,8,9]
-- > <1,2,3,4,15,6,7,8,9>
replace _ _ Null = Null
replace n r v    = reduce2' (\i x y -> if i == n then r :> y else x <++> y) Null indexes . map unit $ v

-- | takes the first elements in a vector until the first element that
-- does not fulfill a predicate.
--
-- <<includes/figs/skel-takewhile-formula.png>>
--
-- >>> takeWhile (<5) $ vector [1,2,3,4,5,6,7,8,9]
-- > <1,2,3,4>
takeWhile _ Null = Null
takeWhile f v    = concat . reduce1 selfunc . map (unit . unit) $ v
  where selfunc x y = if (f . first . first) y && (not . isNull . last) x then x <++> y else x

-- | does a stride-selection on a vector.
--
-- <<includes/figs/skel-stride-formula.png>>
-- <<includes/figs/skel-stride-graph.png>>
stride :: Integer -- ^ first index
       -> Integer -- ^ stride length
       -> Vector a -> Vector a
stride _ _ Null = Null
stride f s v    = let stridef i x y | i >= f && (i-f) `mod` s == 0 = x <++> y
                                    | otherwise                    = y
                  in  reduce2' stridef Null indexes . map unit $ v
                 
-- | groups a vector into sub-vectors of /n/ elements.
--
-- <<includes/figs/skel-group-formula.png>>
-- <<includes/figs/skel-group-graph.png>>
group _ Null = unit Null
group n v    = let groupf i x y  | i `mod` n == 0 = x <++> y
                                 | otherwise      = (first x <++> first' y) :> tail' y
               in  reduce2' groupf Null indexes . map (unit . unit) $ v

-- | right-shifts a vector with an element.
--
-- <<includes/figs/skel-shiftr-graph.png>>
shiftr vs v = v :> init vs

-- | left-shifts a vector with an element.
--
-- <<includes/figs/skel-shiftrl-graph.png>>
shiftl vs v = tail vs <: v

-- | rotates a vector to the left.
--
-- <<includes/figs/skel-rotl-graph.png>>
rotl   Null = Null
rotl   vs   = tail vs <: first vs

-- | rotates a vector to the right.
--
-- <<includes/figs/skel-rotr-graph.png>>
rotr   Null = Null
rotr   vs   = last vs :> init vs


-- | the same as 'get' but with flipped arguments.
v <@  ix = get ix v

-- | unsafe version of '<@>'. Throws an exception if /n > l/.
--
-- <<includes/figs/skel-get-formula.png>>
v <@! ix | isNothing e = error "get!: index out of bounds"
         | otherwise   = fromJust e
  where e = get ix v

-- | > = filterIdx odd
odds      = filterIdx odd

-- | > = filterIdx even
evens     = filterIdx even

-- | selects the elements in a vector at the incexes contained by another vector.
--
-- The following versions of this skeleton are available, the number
-- suggesting how many nested vectors it is operating upon.
--
-- > gather1, gather2, gather3, gather4, gather5
gather1 :: Vector Integer -- ^ vector of indexes
       -> Vector a       -- ^ input vector
       -> Vector a
gather1 ix v     =  map                          (v <@!) ix
gather2 ix vv    = ((=$=).(=$=))                   (vv <@!) ix
gather3 ix vvv   = ((=$=).(=$=).(=$=))             (vvv <@!) ix
gather4 ix vvvv  = ((=$=).(=$=).(=$=).(=$=))       (vvvv <@!) ix
gather5 ix vvvvv = ((=$=).(=$=).(=$=).(=$=).(=$=)) (vvvvv <@!) ix

-- | the same as 'gather1' but with flipped arguments
--
-- <<includes/figs/skel-gather-formula.png>>
-- <<includes/figs/skel-gather-graph.png>>
--
-- The following versions of this skeleton are available, the number
-- suggesting how many nested vectors it is operating upon.
--
-- > (<@!>), (<<@!>>), (<<<@!>>>), (<<<<@!>>>>), (<<<<<@!>>>>>),
(<@!>) :: Vector a        -- ^ input vector
       -> Vector Integer  -- ^ vector of indexes
       -> Vector a
v     <@!>     ix = gather1 ix v
v    <<@!>>    ix = gather2 ix v    
v   <<<@!>>>   ix = gather3 ix v   
v  <<<<@!>>>>  ix = gather4 ix v  
v <<<<<@!>>>>> ix = gather5 ix v 


-- | scatters the elements in a vector based on the indexes contained by another vector.
--
-- <<includes/figs/skel-scatter-formula.png>>
-- <<includes/figs/skel-scatter-graph.png>>
--
-- >>> scatter (vector [2,4,5]) (vector [0,0,0,0,0,0,0,0]) (vector [1,1,1])
-- > <0,1,0,1,1,0,0,0>
scatter Null hv _    = hv
scatter _    hv Null = hv
scatter ix   hv v    = reduce2' (\i r h -> replace i (first r) h) hv ix . map unit $ v

-- | performs a bit-reverse permutation.
--
-- <<includes/figs/skel-bitrev-graph.png>>
--
-- >>> bitrev $ vector ["000","001","010","011","100","101","110","111"]
-- >                   <"111","011","101","001","110","010","100","000">
bitrev (x:>Null) = unit x
bitrev xs        = bitrev (evens xs) <++> bitrev (odds xs)

-- | splits a vector in two equal parts.
--
-- <<includes/figs/skel-duals-graph.png>>
duals   v = let k = length v `div` 2
            in  map21 (,) (take k v) (drop k v)

-- | concatenates a previously split vector. See also 'duals'
--
-- <<includes/figs/skel-unduals-graph.png>>
unduals v = let (x,y) = (v |<) 
            in  x <++> y



-- filter f = reduce1' (\x y -> if f (first x) then x <++> y else y) Null . map unit

-- group n v = map (take n) $ prefix1 dropseries v
--   where dropseries = unit id <++> fanoutn nstages (drop n)
--         nstages    = ceiling $ fromIntegral (length v) / fromIntegral n - 1

