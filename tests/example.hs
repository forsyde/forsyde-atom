{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Test where

import qualified Data.Param.FSVec as V
import Data.TypeLevel hiding ((==))

import GHC.Exts

class OldA e where
  f :: (Maybe a -> b) -> [e (Maybe a)] -> [e b]

data Y a = Y a deriving Show
instance Functor Y where
  fmap f (Y a) = Y (f a)

data X a = X a deriving Show
fromX (X a) = a

instance OldA Y where
  f = fmap . fmap

type family F a b  ::  *
data W a b         =   W {unW :: F a b}
data T a           =   T {unT :: forall b. W a b}

-- r :: forall a b. T a -> F a b                                             
-- r =  unW . (unT :: T a -> W a b) 

-- data Vec a where
--   V :: (Nat s) => V.FSVec s a -> Vec a
--   Q :: a -> Vec a

-- unV (V a) = a
-- unQ (Q a) = a

data Vec a = V {unV :: forall rate . Nat rate => V.FSVec rate a}

class NewA e where
  type Arg e a
  type Ctx e c
  g :: (CTx e c) => (Arg e c a -> b) -> [e (Maybe a)] -> [e b]

instance NewA X where
  type Arg X a = Vec (Maybe a)
  g f xs = let x'  = (V.reallyUnsafeVector . take c . fmap fromX) xs
               xs' = drop c xs
               c   = V.length x'
           in if c == length (V.fromVector x') then X ((unV . f) $ V x') : g f xs' else []



-- instance NewA Y where
--   type Arg Y c a = Maybe a
--   type Typ Y c = ()
--   g = fmap . fmap


-- instance NewA X where
--   type Arg X rate a = V.FSVec rate (Maybe a)
--   type Typ X rate = Nat rate
--   g f xs = let x'  = (V.reallyUnsafeVector . take c . fmap fromX) xs
--                xs' = drop c xs
--                c   = V.length x'
--            in if c == length (V.fromVector x') then X (f x') : g f xs' else []


-- gIndependent :: (Nat size) => (V.FSVec size (Maybe a) -> b) -> [X (Maybe a)] -> [X b]
-- gIndependent _ [] = []
-- gIndependent f xs = let x'  = (V.reallyUnsafeVector . take c . fmap fromX) xs
--                         xs' = drop c xs
--                         c   = V.length x'
--                     in if c == length (V.fromVector x') then X (f x') : gIndependent f xs' else []

-- test :: V.FSVec D2 x -> Int
-- test a = V.length a

-- newtype Args a = Args (forall rate.Nat rate => V.FSVec rate (Maybe a))

-- -- instance NewA X where
-- --   type Arg X a = Args a
-- --   g f xs = let x'  = (V.reallyUnsafeVector . take c . fmap fromX) xs
-- --                xs' = drop c xs
-- --                c   = V.length x'
-- --            in if c == length (V.fromVector x') then X (f $ Args x') : g f xs' else []

-- data C a = C Int a deriving Show
-- fromC (C _ a) = a

-- instance Functor C where
--   fmap f (C c a) = C c (f a)
  
-- q :: ([Maybe a] -> b) -> [C (Maybe a)] -> [C b]
-- q _ []           = []
-- q f s@(C c x:xs) = let x'  = (take c . map fromC) s 
--                        xs' = drop c s
--                    in if length x' == c then C c (f x') : q f xs' else []

