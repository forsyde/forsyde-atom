{-# LANGUAGE PostfixOperators #-}
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
-- The library for the 'Vector' type. Contains the main skeletons.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skel.Vector.Lib where

import Data.Maybe
import ForSyDe.Atom.Skel as S
import ForSyDe.Atom.Skel.Vector.Core
import ForSyDe.Atom.Utility.Tuple

import Prelude hiding (null, last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.SY.Core as SY
-- >>> import ForSyDe.Atom.MoC.SY.Lib
-- >>> import ForSyDe.Atom.Skel hiding (farm21)

-------------------------
-- Functional networks --
-------------------------

------- FARM -------

-- | @farm@ is simply the 'Vector' instance of the skeletom @farm@
-- pattern (see 'ForSyDe.Atom.Skel.farm22'). If the function taken
-- as argument is a process, then it creates a farm network of data
-- parallel processes.
--
-- Constructors: @farm[1-4][1-4]@.
--
-- >>> let v1 = vector [1,2,3,4,5]
-- >>> S.farm21 (+) v1 v1
-- <2,4,6,8,10>
-- >>> let s1 = SY.signal [1,2,3,4,5]
-- >>> let v2 = vector [s1,s1,s1]
-- >>> S.farm11 (comb11 (+1)) v2
-- <{2,3,4,5,6},{2,3,4,5,6},{2,3,4,5,6}>
-- >>> S.farm21 (\x -> comb11 (+x)) v1 v2
-- <{2,3,4,5,6},{3,4,5,6,7},{4,5,6,7,8}>
--
-- <<fig/eqs-skel-vector-farm.png>>
--
-- <<fig/skel-vector-func-farm.png>>
-- <<fig/skel-vector-func-farm-net.png>>
farm22 :: (a1 -> a2 -> (b1, b2)) -- ^ function (e.g. process)
       -> Vector a1              -- ^ first input vector
       -> Vector a2              -- ^ second input vector
       -> (Vector b1, Vector b2) -- ^ two output vectors
farm11 :: (a1 -> b1)
       -> Vector a1
       -> Vector b1
farm12 :: (a1 -> (b1, b2))
       -> Vector a1
       -> (Vector b1, Vector b2)
farm13 :: (a1 -> (b1, b2, b3))
       -> Vector a1
       -> (Vector b1, Vector b2, Vector b3)
farm14 :: (a1 -> (b1, b2, b3, b4))
       -> Vector a1
       -> (Vector b1, Vector b2, Vector b3, Vector b4)
farm21 :: (a1 -> a2 -> b1)
       -> Vector a1 -> Vector a2
       -> Vector b1
farm23 :: (a1 -> a2 -> (b1, b2, b3))
       -> Vector a1 -> Vector a2
       -> (Vector b1, Vector b2, Vector b3)
farm24 :: (a1 -> a2 -> (b1, b2, b3, b4))
       -> Vector a1 -> Vector a2
       -> (Vector b1, Vector b2, Vector b3, Vector b4)
farm31 :: (a1 -> a2 -> a3 -> b1)
       -> Vector a1 -> Vector a2 -> Vector a3
       -> Vector b1
farm32 :: (a1 -> a2 -> a3 -> (b1, b2))
       -> Vector a1 -> Vector a2 -> Vector a3
       -> (Vector b1, Vector b2)
farm33 :: (a1 -> a2 -> a3 -> (b1, b2, b3))
       -> Vector a1 -> Vector a2 -> Vector a3
       -> (Vector b1, Vector b2, Vector b3)
farm34 :: (a1 -> a2 -> a3 -> (b1, b2, b3, b4))
       -> Vector a1 -> Vector a2 -> Vector a3
       -> (Vector b1, Vector b2, Vector b3, Vector b4)
farm41 :: (a1 -> a2 -> a3 -> a4 -> b1)
       -> Vector a1 -> Vector a2 -> Vector a3 -> Vector a4
       -> Vector b1
farm42 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2))
       -> Vector a1 -> Vector a2 -> Vector a3 -> Vector a4
       -> (Vector b1, Vector b2)
farm43 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
       -> Vector a1 -> Vector a2 -> Vector a3 -> Vector a4
       -> (Vector b1, Vector b2, Vector b3)
farm44 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
       -> Vector a1 -> Vector a2 -> Vector a3 -> Vector a4
       -> (Vector b1, Vector b2, Vector b3, Vector b4)

farm11 = S.farm11
farm21 = S.farm21
farm31 = S.farm31
farm41 = S.farm41
farm12 = S.farm12
farm22 = S.farm22
farm32 = S.farm32
farm42 = S.farm42
farm13 = S.farm13
farm23 = S.farm23
farm33 = S.farm33
farm43 = S.farm43
farm14 = S.farm14
farm24 = S.farm24
farm34 = S.farm34
farm44 = S.farm44


------- REDUCE -------

-- | As the name suggests, it reduces a vector to an element based on
-- an associative function. If the function is not associative, it can be treated like a pipeline.
--
-- 'Vector' instantiates the skeletons for both
-- 'ForSyDe.Atom.Skel.reduce' and 'ForSyDe.Atom.Skel.reducei'.
--
-- >>> let v1 = vector [1,2,3,4,5]
-- >>> S.reduce (+) v1
-- 15
-- >>> let s1 = SY.signal [1,2,3,4,5]
-- >>> let s2 = SY.signal [10,10,10,10,10]
-- >>> let v2 = vector [s1,s1,s1]
-- >>> S.reduce (comb21 (+)) v2
-- {3,6,9,12,15}
-- >>> S.reducei (comb21 (+)) s2 v2
-- {13,16,19,22,25}
--
-- <<fig/skel-vector-func-reducei.png>>
-- <<fig/skel-vector-func-reducei-net.png>>
reduce  :: (a -> a -> a) -> Vector a -> a
reducei :: (a -> a -> a) -> a -> Vector a -> a
reduce  = S.reduce
reducei = S.reducei

reduce1  p   v1 vs       = S.farm21 p v1 (init vs) =<<= S.last vs
reduce2  p   v1 v2 vs    = S.farm31 p v1 v2 (init vs) =<<= S.last vs
reduce3  p   v1 v2 v3 vs = S.farm41 p v1 v2 v3 (init vs) =<<= S.last vs
reducei1 p i v1 vs       = S.farm21 p v1 vs =<<= i
reducei2 p i v1 v2 vs    = S.farm31 p v1 v2 vs =<<= i
reducei3 p i v1 v2 v3 vs = S.farm41 p v1 v2 v3 vs =<<= i


------- PREFIX -------

-- | @prefix@ peforms the /parallel prefix/ operation on a vector.
-- Equivalent process networks are constructed if processes are passed
-- as arguments.
--
-- Similar to 'reduce' and 'reducei', two versions 'prefix' and
-- @prefixi@ are provided.
--
-- >>> let v1 = vector [1,2,3,4,5]
-- >>> prefix (+) v1
-- <15,14,12,9,5>
-- >>> let s1 = SY.signal [1,2,3,4,5]
-- >>> let s2 = SY.signal [10,10,10,10,10]
-- >>> let v2 = vector [s1,s1,s1]
-- >>> prefix (comb21 (+)) v2
-- <{3,6,9,12,15},{2,4,6,8,10},{1,2,3,4,5}>
-- >>> prefixi (comb21 (+)) s2 v2
-- <{13,16,19,22,25},{12,14,16,18,20},{11,12,13,14,15}>
--
-- <<fig/eqs-skel-vector-prefix.png>>
--
-- <<fig/skel-vector-func-prefix.png>>
-- <<fig/skel-vector-func-prefix-net.png>>
--
-- <<fig/skel-vector-func-prefixi.png>>
-- <<fig/skel-vector-func-prefixi-net.png>>

prefix  p   = S.farm11 (S.reduce p) . tails
prefixi p i = S.farm11 (S.reducei p i) . tails

-- prefix1 p v1 vs
--   = S.farm21 (reduce1 p) (tails v1) (tails vs)
-- prefix2 p v1 v2 vs
--   = S.farm31 (reduce2 p) (tails v1) (tails v2) (tails vs)
-- prefix3 p v1 v2 v3 vs
--   = S.farm41 (reduce3 p) (tails v1) (tails v2) (tails v3) (tails vs)
-- prefixi1 p i v1 vs
--   = S.farm21 (reducei1 p i) (tails v1) (tails vs)
-- prefixi2 p i v1 v2 vs
--   = S.farm31 (reducei2 p i) (tails v1) (tails v2) (tails vs)
-- prefixi3 p i v1 v2 v3 vs
--   = S.farm41 (reducei3 p i) (tails v1) (tails v2) (tails v3) (tails vs)

    
------- SUFFIX -------

-- | @suffix@ peforms the /parallel suffix/ operation on a vector.
-- Equivalent process networks are constructed if processes are passed
-- as arguments.
--
-- Similar to 'reduce' and 'reducei', two versions 'suffix' and
-- @suffixi@ are provided.
--
-- >>> let v1 = vector [1,2,3,4,5]
-- >>> suffix (+) v1
-- <1,3,6,10,15>
-- >>> let s1 = SY.signal [1,2,3,4,5]
-- >>> let s2 = SY.signal [10,10,10,10,10]
-- >>> let v2 = vector [s1,s1,s1]
-- >>> suffix (comb21 (+)) v2
-- <{1,2,3,4,5},{2,4,6,8,10},{3,6,9,12,15}>
-- >>> suffixi (comb21 (+)) s2 v2
-- <{11,12,13,14,15},{12,14,16,18,20},{13,16,19,22,25}>
--
-- <<fig/eqs-skel-vector-suffix.png>>
--
-- <<fig/skel-vector-func-suffix.png>>
-- <<fig/skel-vector-func-suffix-net.png>>
--
-- <<fig/skel-vector-func-suffixi.png>>
-- <<fig/skel-vector-func-suffixi-net.png>>

suffix p    = S.farm11 (S.reduce p) . inits
suffixi p i = S.farm11 (S.reducei p i) . inits

-- suffix1 p v1 vs
--   = S.farm21 (reduce1 p) (inits v1) (inits vs)
-- suffix2 p v1 v2 vs
--   = S.farm31 (reduce2 p) (fanout v1) (fanout v2) (inits vs)
-- suffix3 p v1 v2 v3 vs
--   = S.farm41 (reduce3 p) (fanout v1) (fanout v2) (fanout v3) (inits vs)
-- suffixi1 p i v1 vs
--   = S.farm21 (reducei1 p i) (tails v1) (inits vs)
-- suffixi2 p i v1 v2 vs
--   = S.farm31 (reducei2 p i) (fanout v1) (fanout v2) (inits vs)
-- suffixi3 p i v1 v2 v3 vs
--   = S.farm41 (reducei3 p i) (fanout v1) (fanout v2) (fanout v3) (inits vs)


------- PIPE -------

-- | @pipe@ creates a pipeline of functions from a vector. 'pipe'
--  simply instantiates the '=<<=' atom whereas @pipeX@ instantiate
--  their omologi from the "ForSyDe.Atom.Skel" module (see
--  'ForSyDe.Atom.Skeletom.pipe2').
--
-- __OBS:__ the pipelining is done in the order dictated by the
-- function composition operator: from right to left.
--
-- Constructors: @pipe[1-4]@.
--
-- >>> let v1 = vector [(+1),(+1),(+1)]
-- >>> S.pipe v1 1
-- 4
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let v2 = vector [1,2,3,4]
-- >>> S.pipe1 (\x -> comb11 (+x)) v2 s1
-- {11,12,13,14}
--
-- <<fig/skel-vector-func-pipe.png>>
-- <<fig/skel-vector-func-pipe-net.png>>
pipe :: Vector (a -> a) -- ^ vector of functions
      -> a               -- ^ input
      -> a               -- ^ output
pipe1 :: (a1 -> a -> a)
      -> Vector a1
      -> a -> a
pipe2 :: (a1 -> a2 -> a -> a)
      -> Vector a1 -> Vector a2
      -> a -> a
pipe3 :: (a1 -> a2 -> a3 -> a -> a)
      -> Vector a1 -> Vector a2 -> Vector a3
      -> a -> a
pipe4 :: (a1 -> a2 -> a3 -> a4 -> a -> a)
      -> Vector a1 -> Vector a2 -> Vector a3 -> Vector a4
      -> a -> a

pipe = S.pipe
pipe1 = S.pipe1
pipe2 = S.pipe2
pipe3 = S.pipe3
pipe4 = S.pipe4


------- RECUR -------

-- | @recur@ creates a systolic array from a vector of
-- functions. Just like 'pipe' and @pipeX@, there exists a raw
-- 'recur' version with an infix operator '=/=', and the enhanced
-- @recurX@ which is meant for systematic partial application of a
-- function on an arbitrary number of vectors until the desired vector
-- of functions is obtained.
--
-- Constructors: @(=/=)@, @recur@, @recuri@, @recur[1-4][1-4]@.
--
-- >>> let v1 = vector [(+1),(+1),(+1)]
-- >>> recur v1 1
-- <4,3,2>
-- >>> recuri v1 1
-- <4,3,2,1>
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let v2 = vector [1,2,3,4]
-- >>> recur1 (\x -> comb11 (+x)) v2 s1
-- <{11,12,13,14},{10,11,12,13},{8,9,10,11},{5,6,7,8}>
--
-- <<fig/eqs-skel-vector-recur.png>>
--
-- <<fig/skel-vector-func-recur.png>>
-- <<fig/skel-vector-func-recur-net.png>>
recur :: Vector (a -> a) -- ^ vector of functions
      -> a               -- ^ input
      -> Vector a        -- ^ output 

infixl 2 =/=
-- | Infix operator for 'recur'.
(=/=) = recur
recur  ps s              = S.farm11 (=<<= s) (tails ps)
recuri ps s              = S.farm11 (=<<= s) (tails $ ps <: id)
recur1 p v1 s            = S.farm11 p v1 =/= s
recur2 p v1 v2 s         = S.farm21 p v1 v2 =/= s
recur3 p v1 v2 v3 s      = S.farm31 p v1 v2 v3 =/= s
recur4 p v1 v2 v3 v4 s   = S.farm41 p v1 v2 v3 v4 =/= s
recuri1 p v1 s            = S.farm11 p v1 `recuri` s
recuri2 p v1 v2 s         = S.farm21 p v1 v2 `recuri` s
recuri3 p v1 v2 v3 s      = S.farm31 p v1 v2 v3 `recuri` s
recuri4 p v1 v2 v3 v4 s   = S.farm41 p v1 v2 v3 v4 `recuri` s



------- CASCADE -------

-- | @cascade@ creates a \"cascading mesh\" as a result of piping a
-- vector into a vector of recur arrays. 
--
-- Constructors: @cascade@, @cascade[1-4]@.
--
-- >>> let v1 = vector [1,2,3,4]
-- >>> cascade (+) v1 v1
-- <238,119,49,14>
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let vs = vector [s1, s1, s1]
-- >>> cascade (comb21 (+)) vs vs
-- <{20,40,60,80},{10,20,30,40},{4,8,12,16}>
-- >>> let vv = vector [vector [1,-1,1], vector [-1,1,-1], vector [1,-1,1] ]
-- >>> cascade1 (\x -> comb21 (\y z-> x*(y+z))) vv vs vs
-- <{16,32,48,64},{8,16,24,32},{-2,-4,-6,-8}>
--
-- <<fig/eqs-skel-vector-cascade.png>>
--
-- <<fig/skel-vector-func-cascade.png>>
-- <<fig/skel-vector-func-cascade-net.png>>
cascade2 :: (a2 -> a1 -> a -> a -> a)
         -- ^ @function41@ which needs to be applied to @function21@
         -> Vector (Vector a2)
         -- ^ fills in the first argument in the function above
         -> Vector (Vector a1)
         -- ^ fills in the second argument in the function above
         -> Vector a
         -- ^ first input vector (e.g. of signals)
         -> Vector a
         -- ^ second input vector (e.g. of signals)
         -> Vector a
         -- ^ output

cascade  p vs1 vs2
  = S.farm11 (\s2 s1 -> S.farm11 p s1 =/= s2)
    vs2 =<<= vs1
cascade1 p vv1 vs1 vs2
  = S.farm21 (\v1 s2 s1 -> S.farm21 p v1 s1 =/= s2)
    vv1 vs2 =<<= vs1
cascade2 p vv1 vv2 vs1 vs2
  = S.farm31 (\v1 v2 s2 s1 -> S.farm31 p v1 v2 s1 =/= s2)
    vv1 vv2 vs2 =<<= vs1
cascade3 p vv1 vv2 vv3 vs1 vs2
  = S.farm41 (\v1 v2 v3 s2 s1 -> S.farm41 p v1 v2 v3 s1 =/= s2)
    vv1 vv2 vv3 vs2 =<<= vs1
cascade4 p vv1 vv2 vv3 vv4 vs1 vs2
  = S.farm51 (\v1 v2 v3 v4 s2 s1 -> S.farm51 p v1 v2 v3 v4 s1 =/= s2)
    vv1 vv2 vv3 vv4 vs2 =<<= vs1



------- MESH -------

-- | @mesh@ creates a 2D systolic array as a result of piping a vector
-- into a vector of 1D systolic arrays.
--
-- Constructors: @mesh@, @mesh[1-4]@.
--
-- >>> let v1 = vector [1,2,3,4]
-- >>> mesh (+) v1 v1
-- <<238,119,49,14>,<119,70,35,13>,<49,35,22,11>,<14,13,11,8>>
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let vs = vector [s1, s1, s1]
-- >>> mesh (comb21 (+)) vs vs
-- <<{20,40,60,80},{10,20,30,40},{4,8,12,16}>,<{10,20,30,40},{6,12,18,24},{3,6,9,12}>,<{4,8,12,16},{3,6,9,12},{2,4,6,8}>>
-- >>> let vv = vector [vector [1,-1,1], vector [-1,1,-1], vector [1,-1,1]]
-- >>> mesh1 (\x -> comb21 (\y z-> x*(y+z))) vv vs vs
-- <<{16,32,48,64},{8,16,24,32},{-2,-4,-6,-8}>,<{8,16,24,32},{-6,-12,-18,-24},{-3,-6,-9,-12}>,<{-2,-4,-6,-8},{-3,-6,-9,-12},{2,4,6,8}>>
--
-- <<fig/eqs-skel-vector-mesh.png>>
--
-- <<fig/skel-vector-func-mesh.png>>
-- <<fig/skel-vector-func-mesh-net.png>>
mesh2 :: (a2 -> a1 -> a -> a -> a)
      -- ^ @function41@ which needs to be applied to @function21@
      -> Vector (Vector a2)
      -- ^ fills in the first argument in the function above
      -> Vector (Vector a1)
      -- ^ fills in the second argument in the function above
      -> Vector a
      -- ^ first input vector (e.g. of signals)
      -> Vector a
      -- ^ second input vector (e.g. of signals)
      -> Vector (Vector a)
      -- ^ output, a 2D vector

mesh  p vs1 vs2
  = S.farm11 (\ s2 s1 -> S.farm11 p s1 =/= s2)
    vs2 =/= vs1
mesh1 p vv1 vs1 vs2
  = S.farm21 (\v1 s2 s1 -> S.farm21 p v1 s1 =/= s2)
    vv1 vs2 =/= vs1
mesh2 p vv1 vv2 vs1 vs2
  = S.farm31 (\v1 v2 s2 s1 -> S.farm31 p v1 v2 s1 =/= s2)
    vv1 vv2 vs2 =/= vs1
mesh3 p vv1 vv2 vv3 vs1 vs2
  = S.farm41 (\v1 v2 v3 s2 s1 -> S.farm41 p v1 v2 v3 s1 =/= s2)
    vv1 vv2 vv3 vs2 =/= vs1
mesh4 p vv1 vv2 vv3 vv4 vs1 vs2
  = S.farm51 (\v1 v2 v3 v4 s2 s1 -> S.farm51 p v1 v2 v3 v4 s1 =/= s2)
    vv1 vv2 vv3 vv4 vs2 =/= vs1

-------------
-- Queries --
-------------

-- | returns the number of elements in a value.
--
-- >>> length $ vector [1,2,3,4,5]
-- 5
--
-- <<fig/eqs-skel-vector-length.png>>
length Null = 0
length v    = S.reduce (+) . S.farm11 (\_ -> 1) $ v

-- | returns a vector with the indexes from another vector.
--
-- >>> index $ vector [1,1,1,1,1,1,1]
-- <1,2,3,4,5,6,7>
index = S.farm21 (\x _ -> x) indexes


-- ----------------
-- -- Generators --
-- ----------------

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
--  * a broadcast in case of a distributed system

-- For reasons of efficiency, this skeleton is defined recursively,
-- but can be proven that it is a catamorphism, because:
--
-- > first . fanout = id
fanout x = x :> fanout x

-- | 'fanoutn' is the same as 'fanout', but the length of the result
-- is also provided.
--
-- >>> fanoutn 5 1
-- <1,1,1,1,1>
fanoutn n x | n <= 0    = Null
            | otherwise = x :> fanoutn (n-1) x

-- | 'generate' creates a vector based on a kernel function. It is
-- just a restricted version of 'recur'.
--
-- >>> generate 5 (+1) 1
-- <6,5,4,3,2>
--
-- <<fig/eqs-skel-vector-generate.png>>
generate n f i | n <= 0    = Null
               | otherwise = fanoutn n f =/= i

-- | 'iterate' is a version of 'generate' which keeps the initial
-- element as well. It is a restricted version of @recuri@.
--
-- >>> iterate 5 (+1) 1
-- <5,4,3,2,1>
iterate n f i | n <= 0     = Null
              | otherwise = fanoutn (n-1) f `recuri` i


---------------
-- Selectors --
---------------

first' Null = Null
first' xs   = S.first xs
last'  Null = Null
last'  xs   = S.last xs
init'  Null = Null
init'  xs   = init xs
tail'  Null = Null
tail'  xs   = tail xs

-- | Instance of 'ForSyDe.Atom.Skel.first'
--
-- >>> S.first $ vector [1,2,3,4,5]
-- 1
first :: Vector a -> a
first = S.first

-- | Instance of 'ForSyDe.Atom.Skel.last'
--
-- >>> S.last $ vector [1,2,3,4,5]
-- 5
last  :: Vector a -> a
last  = S.last

-- | Returns the tail of a vector.
--
-- >>> tail $ vector [1,2,3,4,5]
-- <2,3,4,5>
--
-- <<fig/eqs-skel-vector-tail.png>>
tail Null   = error "[Skel.Vector] cannot return tail of empty vector"
tail (x:>xs)= xs
-- tail     = (<@!> 2) . tails

-- | Returns the initial segment of a vector.
--
-- >>> init $ vector [1,2,3,4,5]
-- <1,2,3,4>
--
-- <<fig/eqs-skel-vector-init.png>>
init Null      = error "init: Vector is empty"
init (_:>Null) = Null
init (v:>vs)   = v :> init vs
-- init      = (<@!> 2) . reverse . inits

-- | concatenates a vector of vectors.
--
-- >>> concat $ vector [vector[1,2,3,4], vector[5,6,7]]
-- <1,2,3,4,5,6,7>
--
-- <<fig/eqs-skel-vector-concat.png>>
concat Null = Null
concat v    = (<++>) =\= v

-- | reverses the elements in a vector.
--
-- >>> reverse $ vector [1,2,3,4,5]
-- <5,4,3,2,1>
--
-- <<fig/eqs-skel-vector-reverse.png>>
--
-- <<fig/skel-vector-comm-reverse.png>>
-- <<fig/skel-vector-comm-reverse-net.png>>
reverse Null = Null
reverse v    = S.reduce (\x y -> y <++> x) . S.farm11 unit $ v

-- | creates a vector of all the initial segments in a vector.
--
-- >>> inits $ vector [1,2,3,4,5]
-- <<1>,<1,2>,<1,2,3>,<1,2,3,4>,<1,2,3,4,5>>
--
-- <<fig/eqs-skel-vector-inits.png>>
--
-- <<fig/skel-vector-comm-inits.png>>
-- <<fig/skel-vector-comm-inits-net.png>>
inits Null = error "inits: null vector"
inits v    = S.reduce sel . S.farm11 (unit . unit) $ v
  where sel x y = x <++> S.farm11 (S.last  x <++>) y

-- | creates a vector of all the final segments in a vector.
--
-- >>> tails $ vector [1,2,3,4,5]
-- <<1,2,3,4,5>,<2,3,4,5>,<3,4,5>,<4,5>,<5>>
--
-- <<fig/eqs-skel-vector-tails.png>>
--
-- <<fig/skel-vector-comm-tails.png>>
-- <<fig/skel-vector-comm-tails-net.png>>
tails Null = error "inits: null vector"
tails v    = S.reduce sel . S.farm11 (unit . unit) $ v
  where sel x y = S.farm11 (<++> S.first y) x <++> y
    
-- | returns the /n/-th element in a vector, or @Nothing@ if /n > l/.
--
-- >>> get 3 $ vector [1,2,3,4,5]
-- Just 3
--
-- <<fig/eqs-skel-vector-get.png>>
get _ Null = Nothing
get n v    = reducei1 sel Nothing indexes . S.farm11 Just $ v
  where sel i x y = if i == n then x else y

-- | takes the first /n/ elements of a vector.
--
-- >>> take 5 $ vector [1,2,3,4,5,6,7,8,9]
-- <1,2,3,4,5>
--
-- <<fig/eqs-skel-vector-take.png>>
take _ Null = Null
take n v    = reducei1 sel Null indexes . S.farm11 unit $ v
  where sel i x y = if i < n then x <++> y else x

-- | drops the first /n/ elements of a vector.
--
-- >>> drop 5 $ vector [1,2,3,4,5,6,7,8,9]
-- <6,7,8,9>
--
-- <<fig/eqs-skel-vector-drop.png>>
drop _ Null = Null
drop n v    = reducei1 sel Null indexes . S.farm11 unit $ v
  where sel i x y = if i > n then x <++> y else y

-- | returns a vector containing only the elements of another vector
-- whose index satisfies a predicate.
--
-- >>> filterIdx (\x -> x `mod` 3 == 0) $ vector [0,1,2,3,4,5,6,7,8,9]
-- <2,5,8>
--
-- <<fig/eqs-skel-vector-filteridx.png>>
--
-- <<fig/skel-vector-comm-filteridx.png>>
-- <<fig/skel-vector-comm-filteridx-net.png>>
filterIdx _ Null = Null
filterIdx f v    = reducei1 sel Null indexes . S.farm11 unit $ v
  where sel i x y = if f i   then x <++> y else y
    
-- | replaces the /n/-th element in a vector with another.
--
-- >>> replace 5 15 $ vector [1,2,3,4,5,6,7,8,9]
-- <1,2,3,4,15,6,7,8,9>
--
-- <<fig/eqs-skel-vector-replace.png>>
--
-- <<fig/skel-vector-comm-replace.png>>
-- <<fig/skel-vector-comm-replace-net.png>>
replace _ _ Null = Null
replace n r v    = reducei1 sel Null indexes . S.farm11 unit $ v
  where sel i x y = if i == n then r :> y else x <++> y

-- | takes the first elements in a vector until the first element that
-- does not fulfill a predicate.
--
-- >>> takeWhile (<5) $ vector [1,2,3,4,5,6,7,8,9]
-- <1,2,3,4>
--
-- <<fig/eqs-skel-vector-takewhile.png>>
takeWhile _ Null = Null
takeWhile f v    = concat . S.reduce sel . S.farm11 (unit . unit) $ v
  where sel x y = if end x y then x <++> y else x
        end x y = (f . S.first . S.first) y &&
                  (not . isNull . S.last) x

-- | does a stride-selection on a vector.
--
-- >>> stride 1 3 $ vector [1,2,3,4,5,6,7,8,9]
-- <1,4,7>
--
-- <<fig/eqs-skel-vector-stride.png>>
--
-- <<fig/skel-vector-comm-inits.png>>
-- <<fig/skel-vector-comm-inits-net.png>>
stride :: Int -- ^ first index
       -> Int -- ^ stride length
       -> Vector a -> Vector a
stride _ _ Null = Null
stride f s v = reducei1 sel Null indexes . S.farm11 unit $ v
  where sel i x y | i >= f && (i-f) `mod` s == 0 = x <++> y
                  | otherwise                    = y
                 
-- | groups a vector into sub-vectors of /n/ elements.
--
-- >>> group 3 $ vector [1,2,3,4,5,6,7,8]
-- <<1,2,3>,<4,5,6>,<7,8>>
--
-- <<fig/eqs-skel-vector-group.png>>
--
-- <<fig/skel-vector-comm-group.png>>
-- <<fig/skel-vector-comm-group-net.png>>
group _ Null = unit Null
group n v = reducei1 sel Null indexes . S.farm11 (unit . unit) $ v
  where sel i x y
          | i `mod` n == 0 = x <++> y
          | otherwise      = (S.first x <++> first' y) :> tail' y

-- | right-shifts a vector with an element.
--
-- >>> vector [1,2,3,4] `shiftr` 8
-- <8,1,2,3>
--
-- <<fig/skel-vector-comm-shiftr.png>>
-- <<fig/skel-vector-comm-shiftr-net.png>>
shiftr vs v = v :> init vs

-- | left-shifts a vector with an element.
--
-- >>> vector [1,2,3,4] `shiftl` 8
-- <2,3,4,8>
--
-- <<fig/skel-vector-comm-shiftl.png>>
-- <<fig/skel-vector-comm-shiftl-net.png>>
shiftl vs v = tail vs <: v

-- | rotates a vector to the left.
--
-- >>> rotl $ vector [1,2,3,4]
-- <2,3,4,1>
--
-- <<fig/skel-vector-comm-rotl.png>>
-- <<fig/skel-vector-comm-rotl-net.png>>
rotl   Null = Null
rotl   vs   = tail vs <: S.first vs

-- | rotates a vector to the right.
--
-- >>> rotr $ vector [1,2,3,4]
-- <4,1,2,3>
--
-- <<fig/skel-vector-comm-rotr.png>>
-- <<fig/skel-vector-comm-rotr-net.png>>
rotr   Null = Null
rotr   vs   = S.last vs :> init vs

-- | rotates a vector to the left or to the right depending on the index:
--
-- * @(> 0)@ : rotates the vector right with the corresponding number
-- of positions.
-- 
-- * @(= 0)@ : does not modify the vector.
-- 
-- * @(< 0)@ : rotates the vector left with the corresponding number
-- of positions.
rotate :: Int -> Vector a -> Vector a
rotate i | i < 0 = doPipe rotl (-i)
         | i > 0 = doPipe rotr i
         | otherwise = id
  where doPipe f i = ForSyDe.Atom.Skel.Vector.Lib.pipe (fanoutn i f)

-- | the same as 'get' but with flipped arguments.
v <@  ix = get ix v

-- | unsafe version of '<@'. Throws an exception if /n > l/.
v <@! ix | isNothing e = error "get!: index out of bounds"
         | otherwise   = fromJust e
  where e = get ix v

-- | <<fig/eqs-skel-vector-odds.png>>
odds      = filterIdx odd

-- | <<fig/eqs-skel-vector-evens.png>>
evens     = filterIdx even

-- | selects the elements in a vector at the incexes contained by another vector.
--
-- The following versions of this skeleton are available, the number
-- suggesting how many nested vectors it is operating upon: @gather[1-5]@
--
-- >>> let ix = vector [vector [1,3,4], vector [3,5,1], vector [5,8,9]]
-- >>> let v = vector [11,12,13,14,15]
-- >>> gather2 ix v
-- <<Just 11,Just 13,Just 14>,<Just 13,Just 15,Just 11>,<Just 15,Nothing,Nothing>>
--
-- <<fig/eqs-skel-vector-gather.png>>
--
-- <<fig/skel-vector-comm-gather.png>>
-- <<fig/skel-vector-comm-gather-net.png>>
gather1 :: Vector Int -- ^ vector of indexes
       -> Vector a       -- ^ input vector
       -> Vector (Maybe a)
gather1 ix v     =  S.farm11                       (v <@) ix
gather2 ix vv    = ((=.=).(=.=))                   (vv <@) ix
gather3 ix vvv   = ((=.=).(=.=).(=.=))             (vvv <@) ix
gather4 ix vvvv  = ((=.=).(=.=).(=.=).(=.=))       (vvvv <@) ix
gather5 ix vvvvv = ((=.=).(=.=).(=.=).(=.=).(=.=)) (vvvvv <@) ix

-- | the same as 'gather1' but with flipped arguments
--
-- The following versions of this skeleton are available, the number
-- suggesting how many nested vectors it is operating upon.
--
-- > (<@>), (<<@>>), (<<<@>>>), (<<<<@>>>>), (<<<<<@>>>>>),
(<@>) :: Vector a        -- ^ input vector
       -> Vector Int  -- ^ vector of indexes
       -> Vector (Maybe a)
v     <@>     ix = gather1 ix v
v    <<@>>    ix = gather2 ix v    
v   <<<@>>>   ix = gather3 ix v   
v  <<<<@>>>>  ix = gather4 ix v  
v <<<<<@>>>>> ix = gather5 ix v 


-- | scatters the elements in a vector based on the indexes contained by another vector.
--
-- >>> scatter (vector [2,4,5]) (vector [0,0,0,0,0,0,0,0]) (vector [1,1,1])
-- <0,1,0,1,1,0,0,0>
--
-- <<fig/eqs-skel-vector-scatter.png>>
--
-- <<fig/skel-vector-comm-scatter.png>>
-- <<fig/skel-vector-comm-scatter-net.png>>
scatter Null hv _    = hv
scatter _    hv Null = hv
scatter ix   hv v    = reducei1 sel hv ix . S.farm11 unit $ v
  where sel i r h = replace i (S.first r) h



-- filter f = reduce1' (\x y -> if f (first x) then x <++> y else y) Null . map unit

-- group n v = map (take n) $ prefix1 dropseries v
--   where dropseries = unit id <++> fanoutn nstages (drop n)
--         nstages    = ceiling $ fromIntegral (length v) / fromIntegral n - 1


-- map  :: (a -> b) -> Vector a -> Vector b
-- red  :: (a -> a -> a) -> Vector a -> a
-- pipe :: Vector (a -> a) -> a -> a
-- scan :: Vector (a -> a) -> a -> Vector a
-- map  = (=.=)
-- red  = (=\=)
-- pipe = (=<<=)
-- scan  ps s = map (=<<= s) (inits ps)
-- scan' ps s = map (=<<= s) (inits $ unit id <++> ps)


