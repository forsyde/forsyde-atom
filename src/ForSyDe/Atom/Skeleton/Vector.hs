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

import Data.Maybe
import ForSyDe.Atom.MoC
import ForSyDe.Atom.Signal
import ForSyDe.Atom.Skeleton
import ForSyDe.Atom.Utility
import ForSyDe.Atom.Behavior


import qualified ForSyDe.Atom.MoC.SY as SY

import Prelude hiding (last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)

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
  (=\=) = foldr1

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

isNull Null = True
isNull _    = False

-- "Constructors"

unit a = a :> Null

Null    <++> ys = ys
(x:>xs) <++> ys = x :> (xs <++> ys) 

xs <: x = xs <++> unit x         

-- Skeletons

map  :: (a -> b) -> Vector a -> Vector b
red  :: (a -> a -> a) -> Vector a -> a
pipe :: Vector (a -> a) -> a -> a
scan :: Vector (a -> a) -> a -> Vector a
map  = (=$=)
red  = (=\=)
pipe = (=<<=)
scan  ps s = map (=<<= s) (inits ps)
scan' ps s = map (=<<= s) (inits $ unit id <++> ps)

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

pref1 p                   = map11 (red1 p) . inits
pref2 p v1 v2             = map21 (red2 p) (unit v1) (inits v2)
pref3 p v1 v2 v3          = map31 (red3 p) (unit v1) (unit v2) (inits v3)
pref4 p v1 v2 v3 v4       = map41 (red4 p) (unit v1) (unit v2) (unit v3) (inits v4)
pref5 p v1 v2 v3 v4 v5    = map51 (red5 p) (unit v1) (unit v2) (unit v3) (unit v4) (inits v5)
pref6 p v1 v2 v3 v4 v5 v6 = map61 (red6 p) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (inits v6)

pref1' p i                   = map11 (red1' p i) . inits
pref2' p i v1 v2             = map21 (red2' p i) (unit v1) (inits v2)
pref3' p i v1 v2 v3          = map31 (red3' p i) (unit v1) (unit v2) (inits v3)
pref4' p i v1 v2 v3 v4       = map41 (red4' p i) (unit v1) (unit v2) (unit v3) (inits v4)
pref5' p i v1 v2 v3 v4 v5    = map51 (red5' p i) (unit v1) (unit v2) (unit v3) (unit v4) (inits v5)
pref6' p i v1 v2 v3 v4 v5 v6 = map61 (red6' p i) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (inits v6)

suf1 p                   = map11 (red1 p) . tails
suf2 p v1 v2             = map21 (red2 p) (unit v1) (tails v2)
suf3 p v1 v2 v3          = map31 (red3 p) (unit v1) (unit v2) (tails v3)
suf4 p v1 v2 v3 v4       = map41 (red4 p) (unit v1) (unit v2) (unit v3) (tails v4)
suf5 p v1 v2 v3 v4 v5    = map51 (red5 p) (unit v1) (unit v2) (unit v3) (unit v4) (tails v5)
suf6 p v1 v2 v3 v4 v5 v6 = map61 (red6 p) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (tails v6)

suf1' p i                   = map11 (red1' p i) . tails
suf2' p i v1 v2             = map21 (red2' p i) (unit v1) (tails v2)
suf3' p i v1 v2 v3          = map31 (red3' p i) (unit v1) (unit v2) (tails v3)
suf4' p i v1 v2 v3 v4       = map41 (red4' p i) (unit v1) (unit v2) (unit v3) (tails v4)
suf5' p i v1 v2 v3 v4 v5    = map51 (red5' p i) (unit v1) (unit v2) (unit v3) (unit v4) (tails v5)
suf6' p i v1 v2 v3 v4 v5 v6 = map61 (red6' p i) (unit v1) (unit v2) (unit v3) (unit v4) (unit v5) (tails v6)

pipe1 p v1 s                      = map11 p v1 `pipe` s
pipe2 p v1 v2 s                   = map21 p v1 v2 `pipe` s
pipe3 p v1 v2 v3 s                = map31 p v1 v2 v3 `pipe` s
pipe4 p v1 v2 v3 v4 s             = map41 p v1 v2 v3 v4 `pipe` s
pipe5 p v1 v2 v3 v4 v5 s          = map51 p v1 v2 v3 v4 v5 `pipe` s
pipe6 p v1 v2 v3 v4 v5 v6 s       = map61 p v1 v2 v3 v4 v5 v6 `pipe` s
pipe7 p v1 v2 v3 v4 v5 v6 v7 s    = map71 p v1 v2 v3 v4 v5 v6 v7 `pipe` s
pipe8 p v1 v2 v3 v4 v5 v6 v7 v8 s = map81 p v1 v2 v3 v4 v5 v6 v7 v8 `pipe` s

systolic                              = scan
systolic1 p v1 s                      = map11 p v1 `scan` s
systolic2 p v1 v2 s                   = map21 p v1 v2 `scan` s
systolic3 p v1 v2 v3 s                = map31 p v1 v2 v3 `scan` s
systolic4 p v1 v2 v3 v4 s             = map41 p v1 v2 v3 v4 `scan` s
systolic5 p v1 v2 v3 v4 v5 s          = map51 p v1 v2 v3 v4 v5 `scan` s
systolic6 p v1 v2 v3 v4 v5 v6 s       = map61 p v1 v2 v3 v4 v5 v6 `scan` s
systolic7 p v1 v2 v3 v4 v5 v6 v7 s    = map71 p v1 v2 v3 v4 v5 v6 v7 `scan` s
systolic8 p v1 v2 v3 v4 v5 v6 v7 v8 s = map81 p v1 v2 v3 v4 v5 v6 v7 v8 `scan` s

cascade  p                 vs1 vs2 = map11 (\            s2 s1 -> map11 p             s1 `scan` s2)                 vs2 `pipe` vs1
cascade1 p vv1             vs1 vs2 = map21 (\v1          s2 s1 -> map21 p v1          s1 `scan` s2) vv1             vs2 `pipe` vs1
cascade2 p vv1 vv2         vs1 vs2 = map31 (\v1 v2       s2 s1 -> map31 p v1 v2       s1 `scan` s2) vv1 vv2         vs2 `pipe` vs1
cascade3 p vv1 vv2 vv3     vs1 vs2 = map41 (\v1 v2 v3    s2 s1 -> map41 p v1 v2 v3    s1 `scan` s2) vv1 vv2 vv3     vs2 `pipe` vs1
cascade4 p vv1 vv2 vv3 vv4 vs1 vs2 = map51 (\v1 v2 v3 v4 s2 s1 -> map51 p v1 v2 v3 v4 s1 `scan` s2) vv1 vv2 vv3 vv4 vs2 `pipe` vs1

mesh  p                 vs1 vs2 = map11 (\            s2 s1 -> map11 p             s1 `scan` s2)                 vs2 `scan` vs1
mesh1 p vv1             vs1 vs2 = map21 (\v1          s2 s1 -> map21 p v1          s1 `scan` s2) vv1             vs2 `scan` vs1
mesh2 p vv1 vv2         vs1 vs2 = map31 (\v1 v2       s2 s1 -> map31 p v1 v2       s1 `scan` s2) vv1 vv2         vs2 `scan` vs1
mesh3 p vv1 vv2 vv3     vs1 vs2 = map41 (\v1 v2 v3    s2 s1 -> map41 p v1 v2 v3    s1 `scan` s2) vv1 vv2 vv3     vs2 `scan` vs1
mesh4 p vv1 vv2 vv3 vv4 vs1 vs2 = map51 (\v1 v2 v3 v4 s2 s1 -> map51 p v1 v2 v3 v4 s1 `scan` s2) vv1 vv2 vv3 vv4 vs2 `scan` vs1

-- Skeletons proven by injectivity (equivalent factorized forms exist)

tail Null    = error "tail: Vector is empty"
tail (x:>xs) = xs
-- tail      = (<@!> 2) . tails

init Null      = error "init: Vector is empty"
init (_:>Null) = Null
init (v:>vs)   = v :> init vs
-- init      = (<@!> 2) . reverse . inits

shiftr vs v = v :> init vs
shiftl vs v = tail vs <: v
rotl   Null = Null
rotl   vs   = tail vs <: first vs
rotr   Null = Null
rotr   vs   = last vs :> init vs

-- Permutators

last    :: Vector a -> a
first   :: Vector a -> a
reverse :: Vector a -> Vector a
inits   :: Vector a -> Vector (Vector a)
tails   :: Vector a -> Vector (Vector a)
length  :: Vector a -> Int

length   = red (+) . map (\_ -> 1)
concat   = red (<++>)
first    = red (\x y -> x)
last     = red (\x y -> y)
reverse  = red (\x y -> y <++> x)                    . map unit
inits    = red (\x y -> x <++> map (last  x <++>) y) . map (unit . unit)
tails    = red (\x y -> map (<++> first y) x <++> y) . map (unit . unit)
-- filter f = red1' (\x y -> if f (first x) then x <++> y else y) Null . map unit
takeWhile f = concat . red1 selfunc . map (unit . unit)
  where selfunc x y = if f (first (first y)) && (not . isNull . last) x then x <++> y else x

        
first' Null = Null
first' xs   = first xs
last'  Null = Null
last'  xs   = last xs
init'  Null = Null
init'  xs   = init xs
tail'  Null = Null
tail'  xs   = tail xs

-- Index-based selectors

get     ix  = red2' (\i x y -> if i == ix then Just x else y) Nothing indexes 
take    n   = red2' (\i x y -> if i < n then x <++> y else x) Null indexes . map unit
drop    n   = red2' (\i x y -> if i > n then x <++> y else y) Null indexes . map unit
filterIdx f = red2' (\i x y -> if f i   then x <++> y else y) Null indexes . map unit
replace n r = red2' (\i x y -> if i == n then r :> y else x <++> y) Null indexes . map unit
stride  f s = let stridef i x y | i `mod` s == f = x <++> y
                                | otherwise      = y
              in  red2' stridef Null indexes . map unit
group   n   = let groupf i x y  | i `mod` n == 0 = x <++> y
                                | otherwise      = (first x <++> first' y) :> tail' y
              in  red2' groupf Null indexes . map (unit . unit)


v <@>  ix = get ix v
v <@!> ix = fromJust $ get ix v
odds      = filterIdx odd
evens     = filterIdx even

bitrev (x:>Null) = unit x
bitrev xs        = bitrev (evens xs) <++> bitrev (odds xs)
duals   v = let k = length v `div` 2
            in  map21 (,) (take k v) (drop k v)
unduals v = let (x,y) = (v |<) 
            in  x <++> y

generate n f i = fanoutn n f `scan` i
iterate  n f i = fanoutn (n-1) f `scan'` i

gather  ix v     =  (=$=)                          (v <@!>) ix
gather2 ix vv    = ((=$=).(=$=))                   (vv <@!>) ix
gather3 ix vvv   = ((=$=).(=$=).(=$=))             (vvv <@!>) ix
gather4 ix vvvv  = ((=$=).(=$=).(=$=).(=$=))       (vvvv <@!>) ix
gather5 ix vvvvv = ((=$=).(=$=).(=$=).(=$=).(=$=)) (vvvvv <@!>) ix

scatter ix hv = red2' (\i h r -> replace i (first r) h) hv ix . map unit


-- TODO: Not fully correct, since <{1,2},{3,_}> would become {<1,3>, _}.
-- This is because the MoC class explicitly asks for `Value`s. This might
-- be fixable once the Behavior Layer is described as a type class
zipx   w21 w11 = red catEv . map unitEv
  where catEv  = (comb21 . w21 . psi21) (<++>)
        unitEv = (comb11 . w11 . psi11) unit

-- TODO: not fully correct. See zipx. Extremely unsafe even!!!
unzipx w1 w2 sv = (map getFst . scan' selFun) sv
  where getFst = (comb11 . w1 . psi11) first
        selFun = fanoutn n ((comb11 . w2 . psi11) tail)
        n      = (length . unsafeFromValue . head . sniff . headS) sv - 1 


-- group n v = map (take n) $ pref1 dropseries v
--   where dropseries = unit id <++> fanoutn nstages (drop n)
--         nstages    = ceiling $ fromIntegral (length v) / fromIntegral n - 1












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
