{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Atom.Skel.FastVector.Lib where

import ForSyDe.Atom 
import Data.Maybe
import Control.Applicative
import Data.List.Split
import qualified Data.List as L
import Prelude hiding (take, drop, length, zip, unzip)


-- | In this library 'Vector' is just a wrapper around a list.
newtype Vector a = Vector { fromVector :: [a] } deriving (Eq)

vector = Vector

instance Functor Vector where
  fmap f (Vector a) = Vector (fmap f a)

instance Applicative Vector where
  pure a = Vector [a]
  (Vector fs) <*> (Vector as) = Vector $ getZipList (ZipList fs <*> ZipList as) 

instance (Show a) => Show (Vector a) where
  showsPrec p (Vector []) = showParen (p > 9) (showString "<>")
  showsPrec p (Vector xs) = showParen (p > 9) (showChar '<' . showVector1 xs)
    where
      showVector1 [] = showChar '>'            
      showVector1 (y:[]) = shows y . showChar '>'
      showVector1 (y:ys) = shows y . showChar ',' 
        . showVector1 ys

instance Foldable Vector where
  foldr f x (Vector a) = foldr f x a

farm11 f = fmap f
farm21 f a b = f <$> a <*> b
farm31 f a b c = f <$> a <*> b <*> c
farm41 f a b c d = f <$> a <*> b <*> c <*> d
farm51 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
farm12 f = (|<) . fmap f
farm22 f a = (|<) . farm21 f a

infixr 5 <++>
(Vector a) <++> (Vector b) = Vector (a ++ b)

unsafeApply f (Vector a) = f a
unsafeLift  f (Vector a) = Vector (f a)

-- | See 'ForSyDe.Atom.Skel.Vector.reduce'.
reduce f = unsafeApply (L.foldr1 f)

-- | See 'ForSyDe.Atom.Skel.Vector.length'.
length = unsafeApply (L.length)

-- | See 'ForSyDe.Atom.Skel.Vector.drop'.
drop n = unsafeLift (L.drop n)

-- | See 'ForSyDe.Atom.Skel.Vector.take'.
take n = unsafeLift (L.take n)

-- | See 'ForSyDe.Atom.Skel.Vector.first'.
first = unsafeApply L.head

-- | See 'ForSyDe.Atom.Skel.Vector.group'.
group :: Int -> Vector a -> Vector (Vector a)
group n (Vector a) = vector $ map vector $ chunksOf n a

-- | See 'ForSyDe.Atom.Skel.Vector.fanout'.
fanout    = vector . L.repeat

-- | See 'ForSyDe.Atom.Skel.Vector.fanoutn'.
fanoutn n = vector . L.replicate n

-- | See 'ForSyDe.Atom.Skel.Vector.stencil'.
stencil n v = farm11 (take n) $ dropFromEnd n $ tails v
  where dropFromEnd n = take (length v - n + 1)

-- | See 'ForSyDe.Atom.Skel.Vector.tails'.
tails = unsafeLift (L.init . map vector . L.tails)

-- | See 'ForSyDe.Atom.Skel.Vector.concat'.
concat = unsafeLift (L.concat . map fromVector)

-- | See 'ForSyDe.Atom.Skel.Vector.iterate'.
iterate n f i = vector $ L.take n $ L.iterate f i 

-- | See 'ForSyDe.Atom.Skel.Vector.pipe'.
pipe (Vector []) i = i
pipe v i = unsafeApply (L.foldr1 (.)) v i

-- | See 'ForSyDe.Atom.Skel.Vector.pipe1'.
pipe1 f v i = unsafeApply (L.foldr f i) v

-- | See 'ForSyDe.Atom.Skel.Vector.reverse'.
reverse = unsafeLift L.reverse

-- | See 'ForSyDe.Atom.Skel.Vector.recuri'.
recuri ps s = farm11 (`pipe` s) (unsafeLift (L.map vector . L.tails) ps)



-- reducei1 p i v1 vs       = S.farm21 p v1 vs =<<= i
-- tail'  Null = Null
-- tail'  xs   = V.tail xs
-- first' Null = Null
-- first' xs   = V.first xs

-- -- | The function 'rotate' rotates a vector based on an index offset.
-- --
-- -- * @(> 0)@ : rotates the vector left with the corresponding number
-- -- of positions.
-- --
-- -- * @(= 0)@ : does not modify the vector.
-- --
-- -- * @(< 0)@ : rotates the vector right with the corresponding number
-- -- of positions.
-- rotate :: Int -> Vector a -> Vector a
-- rotate n
--   | n > 0     = V.pipe (V.fanoutn (abs n) V.rotl)
--   | n < 0     = V.pipe (V.fanoutn (abs n) V.rotr)
--   | otherwise = id


-- -- | takes the first /n/ elements of a vector.
-- --
-- -- >>> take 5 $ vector [1,2,3,4,5,6,7,8,9]
-- -- <1,2,3,4,5>
-- --
-- -- <<fig/eqs-skel-vector-take.png>>
-- take _ Null = Null
-- take n v    = reducei1 sel Null indexes . S.farm11 unit $ v
--   where sel i x y = if i < n then x <++> y else x

-- -- | drops the first /n/ elements of a vector.
-- --
-- -- >>> drop 5 $ vector [1,2,3,4,5,6,7,8,9]
-- -- <6,7,8,9>
-- --
-- -- <<fig/eqs-skel-vector-drop.png>>
-- drop _ Null = Null
-- drop n v    = reducei1 sel Null indexes . S.farm11 unit $ v
--   where sel i x y = if i > n then x <++> y else y

-- -- | groups a vector into sub-vectors of /n/ elements.
-- --
-- -- >>> group 3 $ vector [1,2,3,4,5,6,7,8]
-- -- <<1,2,3>,<4,5,6>,<7,8>>
-- --
-- -- <<fig/eqs-skel-vector-group.png>>
-- --
-- -- <<fig/skel-vector-comm-group.png>>
-- -- <<fig/skel-vector-comm-group-net.png>>
-- group :: Int -> Vector a -> Vector (Vector a)
-- group _ Null = unit Null
-- group n v = reducei1 sel Null indexes . S.farm11 (unit . unit) $ v
--   where sel i x y
--           | i `mod` n == 0 = x <++> y
--           | otherwise      = (S.first x <++> first' y) :> tail' y

-- | See 'ForSyDe.Atom.Skel.Vector.get'.
get :: Int -> Vector a -> Maybe a 
get _ (Vector []) = Nothing
get n v | n >= length v = Nothing
        | otherwise = Just $ fromVector v !! n

-- -- | the same as 'get' but with flipped arguments.
-- v <@  ix = get ix v

-- -- | unsafe version of '<@>'. Throws an exception if /n > l/.
-- v <@! ix | isNothing e = error "get!: index out of bounds"
--          | otherwise   = fromJust e
--   where e = get ix v


-- indexes = V.vector [1..] :: Vector Int

-- indexesTo n = take n $ indexes


-- -- | Returns a stencil of @n@ neighboring elements for each possible
-- -- element in a vector.
-- --
-- -- >>> stencilV 3 $ vector [1..5]
-- -- <<1,2,3>,<2,3,4>,<3,4,5>>
-- stencil :: Int               -- ^ stencil size @= n@
--         -> Vector a          -- ^ /length/ = @la@ 
--         -> Vector (Vector a) -- ^ /length/ = @la - n + 1@ 
-- stencil n v = V.farm11 (take n) $ dropFromEnd n $ V.tails v
--   where dropFromEnd n = take (length v - n + 1)


-- zip = farm21 (,)

-- unzip = farm12 id

evensF [] = []
evensF [x] = [x]
evensF (x:_:xs) = x:evensF xs
oddsF  [] = []
oddsF  [_] = []
oddsF  (_:y:xs) = y:oddsF xs

-- | See 'ForSyDe.Atom.Skel.Vector.DSP.evens'.
evens = unsafeLift evensF

-- | See 'ForSyDe.Atom.Skel.Vector.DSP.odds'.
odds = unsafeLift oddsF
