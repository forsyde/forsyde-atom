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
import ForSyDe.Atom.Skeleton as S
import ForSyDe.Atom.Skeleton.Vector.Core
import ForSyDe.Atom.Utility

import Prelude hiding (null, last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.SY.Core as SY
-- >>> import ForSyDe.Atom.MoC.SY.Lib
-- >>> import ForSyDe.Atom.Skeleton hiding (farm21)

-------------------------
-- Functional networks --
-------------------------

------- FARM -------

-- | @farm@ is simply the 'Vector' instance of the skeletom @farm@
-- pattern (see 'ForSyDe.Atom.Skeleton.farm22'). If the function taken
-- as argument is a process, then it creates a farm network of data
-- parallel processes.
--
-- The following constructors are provided:
--
-- >  farm11, farm12, farm13, farm14,
-- >  farm21, farm22, farm23, farm24,
-- >  farm31, farm32, farm33, farm34,
-- >  farm41, farm42, farm43, farm44,
-- >  farm51, farm52, farm53, farm54,
-- >  farm61, farm62, farm63, farm64,
-- >  farm71, farm72, farm73, farm74,
-- >  farm81, farm82, farm83, farm84,
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
-- <<docfiles/figs/eqs-skel-vector-farm.png>>
--
-- <<docfiles/figs/skel-vector-farm.png>>
-- <<docfiles/figs/skel-vector-farm-net.png>>
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
-- 'ForSyDe.Atom.Skeleton.reduce' and 'ForSyDe.Atom.Skeleton.reducei'.
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
-- <<docfiles/figs/skel-vector-reducei.png>>
-- <<docfiles/figs/skel-vector-reducei-net.png>>
reduce  :: (a -> a -> a) -> Vector a -> a
reducei :: (a -> a -> a) -> a -> Vector a -> a
reduce  = S.reduce
reducei = S.reducei

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
-- <1,3,6,10,15>
-- >>> let s1 = SY.signal [1,2,3,4,5]
-- >>> let s2 = SY.signal [10,10,10,10,10]
-- >>> let v2 = vector [s1,s1,s1]
-- >>> prefix (comb21 (+)) v2
-- <{1,2,3,4,5},{2,4,6,8,10},{3,6,9,12,15}>
-- >>> prefixi (comb21 (+)) s2 v2
-- <{11,12,13,14,15},{12,14,16,18,20},{13,16,19,22,25}>
--
-- <<docfiles/figs/eqs-skel-vector-prefix.png>>
--
-- <<docfiles/figs/skel-vector-prefix.png>>
-- <<docfiles/figs/skel-vector-prefix-net.png>>
--
-- <<docfiles/figs/skel-vector-prefixi.png>>
-- <<docfiles/figs/skel-vector-prefixi-net.png>>

prefix  p   = S.farm11 (S.reduce p) . inits
prefixi p i = S.farm11 (S.reducei p i) . inits

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
-- <15,14,12,9,5>
-- >>> let s1 = SY.signal [1,2,3,4,5]
-- >>> let s2 = SY.signal [10,10,10,10,10]
-- >>> let v2 = vector [s1,s1,s1]
-- >>> suffix (comb21 (+)) v2
-- <{3,6,9,12,15},{2,4,6,8,10},{1,2,3,4,5}>
-- >>> suffixi (comb21 (+)) s2 v2
-- <{13,16,19,22,25},{12,14,16,18,20},{11,12,13,14,15}>
--
-- <<docfiles/figs/eqs-skel-vector-suffix.png>>
--
-- <<docfiles/figs/skel-vector-suffix.png>>
-- <<docfiles/figs/skel-vector-suffix-net.png>>
--
-- <<docfiles/figs/skel-vector-suffixi.png>>
-- <<docfiles/figs/skel-vector-suffixi-net.png>>

suffix p    = S.farm11 (S.reduce p) . tails
suffixi p i = S.farm11 (S.reducei p i) . tails

------- PIPE -------

-- | @pipe@ creates a pipeline of functions from a vector. 'pipe'
--  simply instantiates the '=<<=' atom whereas @pipeX@ instantiate
--  their omologi from the "ForSyDe.Atom.Skeleton" module (see
--  'ForSyDe.Atom.Skeletom.pipe2').
--
-- __OBS:__ the pipelining is done in the order dictated by the
-- function composition operator: from right to left.
--
-- The following constructors are provided:
--
-- > pipe, pipe1, pipe2, pipe3, pipe4
--
-- >>> let v1 = vector [(+1),(+1),(+1)]
-- >>> S.pipe v1 1
-- 4
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let v2 = vector [1,2,3,4]
-- >>> S.pipe1 (\x -> comb11 (+x)) v2 s1
-- {11,12,13,14}
--
-- <<docfiles/figs/skel-vector-pipe.png>>
-- <<docfiles/figs/skel-vector-pipe-net.png>>
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
-- The following constructors are provided:
--
-- > (=/=), recur,
-- > recur1, recur2, recur3, recur4,
--
-- >>> let v1 = vector [(+1),(+1),(+1)]
-- >>> recur v1 1
-- <2,3,4>
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let v2 = vector [1,2,3,4]
-- >>> recur1 (\x -> comb11 (+x)) v2 s1
-- <{2,3,4,5},{4,5,6,7},{7,8,9,10},{11,12,13,14}>
--
-- <<docfiles/figs/eqs-skel-vector-recur.png>>
--
-- <<docfiles/figs/skel-vector-recur.png>>
-- <<docfiles/figs/skel-vector-recur-net.png>>
recur :: Vector (a -> a) -- ^ vector of functions
      -> a               -- ^ input
      -> Vector a        -- ^ output 

infixl 2 =/=
-- | Infix operator for 'recur'.
(=/=) = recur
recur  ps s              = S.farm11 (=<<= s) (tails ps)
recur1 p v1 s            = S.farm11 p v1 =/= s
recur2 p v1 v2 s         = S.farm21 p v1 v2 =/= s
recur3 p v1 v2 v3 s      = S.farm31 p v1 v2 v3 =/= s
recur4 p v1 v2 v3 v4 s   = S.farm41 p v1 v2 v3 v4 =/= s

------- CASCADE -------

-- | @cascade@ creates a \"cascading mesh\" as a result of piping a
-- vector into a vector of recur arrays. 
--
-- The following constructors are provided:
--
-- > cascade, cascade1, cascade2, cascade3, cascade4, 
--
-- >>> let v1 = vector [1,2,3,4]
-- >>> cascade (+) v1 v1
-- <11,36,86,175>
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let vs = vector [s1, s1, s1, s1]
-- >>> cascade (comb21 (+)) vs vs
-- <{5,10,15,20},{15,30,45,60},{35,70,105,140},{70,140,210,280}>
-- >>> let vv = vector [vector [1,-1,1,-1], vector [-1,1,-1,1], vector [1,-1,1,-1], vector [-1,1,-1,1] ]
-- >>> cascade1 (\x -> comb21 (\y z-> x*(y+z))) vv vs vs
-- <{1,2,3,4},{-1,-2,-3,-4},{-3,-6,-9,-12},{4,8,12,16}>
--
-- <<docfiles/figs/eqs-skel-vector-cascade.png>>
--
-- <<docfiles/figs/skel-vector-cascade.png>>
-- <<docfiles/figs/skel-vector-cascade-net.png>>
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


-- >>> let v1 = vector [1,2,3,4]
-- >>> mesh (+) v1 v1
-- <<2,4,7,11>,<4,9,17,29>,<7,19,40,74>,<11,36,86,175>>
-- >>> mesh (comb21 (+)) vs vs
-- <<{2,4,6,8},{3,6,9,12},{4,8,12,16}>,<{3,6,9,12},{6,12,18,24},{10,20,30,40}>,<{4,8,12,16},{10,20,30,40},{20,40,60,80}>>

------- MESH -------

-- | @mesh@ creates a 2D systolic array as a result of piping a vector
-- into a vector of 1D systolic arrays.
--
-- The following constructors are provided:
--
-- > mesh, mesh1, mesh2, mesh3, mesh4, 
--
-- >>> let s1 = SY.signal [1,2,3,4]
-- >>> let vs = vector [s1, s1, s1]
-- >>> let vv = vector [vector [1,-1,1], vector [-1,1,-1], vector [1,-1,1]]
-- >>> mesh1 (\x -> comb21 (\y z-> x*(y+z))) vv vs vs
-- <<{16,32,48,64},{8,16,24,32},{-2,-4,-6,-8}>,<{8,16,24,32},{-6,-12,-18,-24},{-3,-6,-9,-12}>,<{-2,-4,-6,-8},{-3,-6,-9,-12},{2,4,6,8}>>
--
-- <<docfiles/figs/eqs-skel-vector-mesh.png>>
--
-- <<docfiles/figs/skel-vector-mesh.png>>
-- <<docfiles/figs/skel-vector-mesh-net.png>>
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



-- map  :: (a -> b) -> Vector a -> Vector b
-- red  :: (a -> a -> a) -> Vector a -> a
-- pipe :: Vector (a -> a) -> a -> a
-- scan :: Vector (a -> a) -> a -> Vector a
-- map  = (=.=)
-- red  = (=\=)
-- pipe = (=<<=)
-- scan  ps s = map (=<<= s) (inits ps)
-- scan' ps s = map (=<<= s) (inits $ unit id <++> ps)




-- -------------
-- -- Queries --
-- -------------

-- -- | returns the number of elements in a value.
-- --
-- -- <<includes/figs/skel-length-formula.png>>
-- length Null = 0
-- length v    = red (+) . map (\_ -> 1) $ v

-- -- | returns a vetor with the indexes from another vector.
-- --
-- index = map21 (\x _ -> x) indexes

-- ----------------
-- -- Generators --
-- ----------------

-- -- | 'fanout' repeats an element. As a process network it distributes
-- -- the same value or signal to all the connected processes down the
-- -- line. Depending on the target platform and the refinement decisions
-- -- involved, it may be interpreted in the following implementations:
-- --
-- --  * global or shared memory in case of a massively parallel platform
-- --  (e.g. GPU)
-- --  * a (static) memory or cache location in memory-driven
-- --  architectures (e.g. CPU)
-- --  * a fanout in case of a HDL system
-- --  * a physical copy or a broadcast in case of a distributed system
-- --
-- -- For reasons of efficiency, this skeleton is defined recursively,
-- -- but can be proven that it is a catamorphism, because:
-- --
-- -- > first . fanout = id
fanout x = x :> fanout x

-- -- | 'fanoutn' is the same as 'fanout', but the length of the result
-- -- is also provided.
-- fanoutn n x | n <= 0    = Null
--             | otherwise = x :> fanoutn (n-1) x

-- -- | 'generate' creates a vector based on a kernel function (see also
-- -- 'recur0'). E.g.:
-- --
-- -- >>> generate 5 (+1) 1
-- -- > <2,3,4,5,6>
-- --
-- -- <<includes/figs/skel-generate-formula.png>>
-- generate n f i | n <= 0    = Null
--                | otherwise = fanoutn n f `scan` i

-- -- | 'iterate' is a version of 'generate' which keeps the initial
-- -- element as well (see also 'recur0').  E.g.:
-- --
-- -- >>> iterate 5 (+1) 1
-- -- > <1,2,3,4,5,6>
-- iterate n f i | n <= 0     = Null
--               | otherwise = fanoutn (n-1) f `scan'` i

-- ---------------
-- -- Selectors --
-- ---------------

-- first' Null = Null
-- first' xs   = first xs
-- last'  Null = Null
-- last'  xs   = last xs
-- init'  Null = Null
-- init'  xs   = init xs
-- tail'  Null = Null
-- tail'  xs   = tail xs

tail Null    = error "tail: Vector is empty"
tail (x:>xs) = xs
-- tail      = (<@!> 2) . tails

init Null      = error "init: Vector is empty"
init (_:>Null) = Null
init (v:>vs)   = v :> init vs
-- init      = (<@!> 2) . reverse . inits

-- -- | concatenates a vector of vectors.
-- --
-- -- <<includes/figs/skel-concat-formula.png>>
-- concat Null = Null
-- concat v    = red (<++>) v


-- -- | returns the last element in a vector. Implemented as a reduction
-- -- (see source code).
-- -- last Null = error "last: empty vector"
-- -- last  v   = red (\x y -> y) v

-- -- | reverses the elements in a vector.
-- --
-- -- <<includes/figs/skel-reverse-formula.png>>
-- -- <<includes/figs/skel-reverse-graph.png>>
reverse Null = Null
reverse v    = S.reduce (\x y -> y <++> x) . S.farm11 unit $ v

-- | creates a vector of all the initial segments in a vector.
--
-- <<includes/figs/skel-inits-formula.png>>
-- <<includes/figs/skel-inits-graph.png>>
inits Null = error "inits: null vector"
inits v    = S.reduce (\x y -> x <++> S.farm11 (last  x <++>) y) . S.farm11 (unit . unit) $ v

-- | creates a vector of all the final segments in a vector.
--
-- <<includes/figs/skel-tails-formula.png>>
-- <<includes/figs/skel-tails-graph.png>>
tails Null = error "inits: null vector"
tails v    = S.reduce (\x y -> S.farm11 (<++> first y) x <++> y) . S.farm11 (unit . unit) $ v

-- -- | returns the /n/-th element in a vector, or @Nothing@ if /n > l/.
-- get _ Null = Nothing
-- get n v    = reduce2' (\i x y -> if i == n then x else y) Nothing indexes . map Just $ v

-- -- | takes the first /n/ elements of a vector.
-- --
-- -- <<includes/figs/skel-take-formula.png>>
-- --
-- -- >>> take 5 $ vector [1,2,3,4,5,6,7,8,9]
-- -- > <1,2,3,4,5>
-- take _ Null = Null
-- take n v    = reduce2' (\i x y -> if i < n then x <++> y else x) Null indexes . map unit $ v

-- -- | drops the first /n/ elements of a vector.
-- --
-- -- <<includes/figs/skel-drop-formula.png>>
-- --
-- -- >>> drop 5 $ vector [1,2,3,4,5,6,7,8,9]
-- -- > <6,7,8,9>
-- drop _ Null = Null
-- drop n v    = reduce2' (\i x y -> if i > n then x <++> y else y) Null indexes . map unit $ v

-- -- | returns a vector containing only the elements of another vector
-- -- whose index satisfies a predicate.
-- --
-- -- <<includes/figs/skel-filterIdx-formula.png>>
-- -- <<includes/figs/skel-filterIdx-graph.png>>
-- filterIdx _ Null = Null
-- filterIdx f v    = reduce2' (\i x y -> if f i   then x <++> y else y) Null indexes . map unit $ v

-- -- | replaces the /n/-th element in a vector with another.
-- --
-- -- <<includes/figs/skel-replace-formula.png>>
-- --
-- -- >>> replace 5 15 $ vector [1,2,3,4,5,6,7,8,9]
-- -- > <1,2,3,4,15,6,7,8,9>
-- replace _ _ Null = Null
-- replace n r v    = reduce2' (\i x y -> if i == n then r :> y else x <++> y) Null indexes . map unit $ v

-- -- | takes the first elements in a vector until the first element that
-- -- does not fulfill a predicate.
-- --
-- -- <<includes/figs/skel-takewhile-formula.png>>
-- --
-- -- >>> takeWhile (<5) $ vector [1,2,3,4,5,6,7,8,9]
-- -- > <1,2,3,4>
-- takeWhile _ Null = Null
-- takeWhile f v    = concat . reduce1 selfunc . map (unit . unit) $ v
--   where selfunc x y = if (f . first . first) y && (not . isNull . last) x then x <++> y else x

-- -- | does a stride-selection on a vector.
-- --
-- -- <<includes/figs/skel-stride-formula.png>>
-- -- <<includes/figs/skel-stride-graph.png>>
-- stride :: Integer -- ^ first index
--        -> Integer -- ^ stride length
--        -> Vector a -> Vector a
-- stride _ _ Null = Null
-- stride f s v    = let stridef i x y | i >= f && (i-f) `mod` s == 0 = x <++> y
--                                     | otherwise                    = y
--                   in  reduce2' stridef Null indexes . map unit $ v
                 
-- -- | groups a vector into sub-vectors of /n/ elements.
-- --
-- -- <<includes/figs/skel-group-formula.png>>
-- -- <<includes/figs/skel-group-graph.png>>
-- group _ Null = unit Null
-- group n v    = let groupf i x y  | i `mod` n == 0 = x <++> y
--                                  | otherwise      = (first x <++> first' y) :> tail' y
--                in  reduce2' groupf Null indexes . map (unit . unit) $ v

-- -- | right-shifts a vector with an element.
-- --
-- -- <<includes/figs/skel-shiftr-graph.png>>
-- shiftr vs v = v :> init vs

-- -- | left-shifts a vector with an element.
-- --
-- -- <<includes/figs/skel-shiftrl-graph.png>>
-- shiftl vs v = tail vs <: v

-- -- | rotates a vector to the left.
-- --
-- -- <<includes/figs/skel-rotl-graph.png>>
-- rotl   Null = Null
-- rotl   vs   = tail vs <: first vs

-- -- | rotates a vector to the right.
-- --
-- -- <<includes/figs/skel-rotr-graph.png>>
-- rotr   Null = Null
-- rotr   vs   = last vs :> init vs


-- -- | the same as 'get' but with flipped arguments.
-- v <@  ix = get ix v

-- -- | unsafe version of '<@>'. Throws an exception if /n > l/.
-- --
-- -- <<includes/figs/skel-get-formula.png>>
-- v <@! ix | isNothing e = error "get!: index out of bounds"
--          | otherwise   = fromJust e
--   where e = get ix v

-- -- | > = filterIdx odd
-- odds      = filterIdx odd

-- -- | > = filterIdx even
-- evens     = filterIdx even

-- -- | selects the elements in a vector at the incexes contained by another vector.
-- --
-- -- The following versions of this skeleton are available, the number
-- -- suggesting how many nested vectors it is operating upon.
-- --
-- -- > gather1, gather2, gather3, gather4, gather5
-- gather1 :: Vector Integer -- ^ vector of indexes
--        -> Vector a       -- ^ input vector
--        -> Vector a
-- gather1 ix v     =  map                          (v <@!) ix
-- gather2 ix vv    = ((=.=).(=.=))                   (vv <@!) ix
-- gather3 ix vvv   = ((=.=).(=.=).(=.=))             (vvv <@!) ix
-- gather4 ix vvvv  = ((=.=).(=.=).(=.=).(=.=))       (vvvv <@!) ix
-- gather5 ix vvvvv = ((=.=).(=.=).(=.=).(=.=).(=.=)) (vvvvv <@!) ix

-- -- | the same as 'gather1' but with flipped arguments
-- --
-- -- <<includes/figs/skel-gather-formula.png>>
-- -- <<includes/figs/skel-gather-graph.png>>
-- --
-- -- The following versions of this skeleton are available, the number
-- -- suggesting how many nested vectors it is operating upon.
-- --
-- -- > (<@!>), (<<@!>>), (<<<@!>>>), (<<<<@!>>>>), (<<<<<@!>>>>>),
-- (<@!>) :: Vector a        -- ^ input vector
--        -> Vector Integer  -- ^ vector of indexes
--        -> Vector a
-- v     <@!>     ix = gather1 ix v
-- v    <<@!>>    ix = gather2 ix v    
-- v   <<<@!>>>   ix = gather3 ix v   
-- v  <<<<@!>>>>  ix = gather4 ix v  
-- v <<<<<@!>>>>> ix = gather5 ix v 


-- -- | scatters the elements in a vector based on the indexes contained by another vector.
-- --
-- -- <<includes/figs/skel-scatter-formula.png>>
-- -- <<includes/figs/skel-scatter-graph.png>>
-- --
-- -- >>> scatter (vector [2,4,5]) (vector [0,0,0,0,0,0,0,0]) (vector [1,1,1])
-- -- > <0,1,0,1,1,0,0,0>
-- scatter Null hv _    = hv
-- scatter _    hv Null = hv
-- scatter ix   hv v    = reduce2' (\i r h -> replace i (first r) h) hv ix . map unit $ v

-- -- | performs a bit-reverse permutation.
-- --
-- -- <<includes/figs/skel-bitrev-graph.png>>
-- --
-- -- >>> bitrev $ vector ["000","001","010","011","100","101","110","111"]
-- -- >                   <"111","011","101","001","110","010","100","000">
-- bitrev (x:>Null) = unit x
-- bitrev xs        = bitrev (evens xs) <++> bitrev (odds xs)

-- -- | splits a vector in two equal parts.
-- --
-- -- <<includes/figs/skel-duals-graph.png>>
-- duals   v = let k = length v `div` 2
--             in  map22 (,) (take k v) (drop k v)

-- -- | concatenates a previously split vector. See also 'duals'
-- --
-- -- <<includes/figs/skel-unduals-graph.png>>
-- unduals = (<++>)



-- -- filter f = reduce1' (\x y -> if f (first x) then x <++> y else y) Null . map unit

-- -- group n v = map (take n) $ prefix1 dropseries v
-- --   where dropseries = unit id <++> fanoutn nstages (drop n)
-- --         nstages    = ceiling $ fromIntegral (length v) / fromIntegral n - 1

