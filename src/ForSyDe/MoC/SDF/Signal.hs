{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSDFDe.MoC.SDF.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF.Signal where

import ForSyDe.Core


data Stream a = a :- Stream a | NullS deriving (Eq)

type Signal a = Stream a

infixr 3 :-

instance Functor Stream where
  fmap _ NullS   = NullS
  fmap f (x:-xs) = f x :- fmap f xs

instance Applicative Stream where
  pure a  = a :- pure a
  _         <*> NullS     = NullS
  NullS     <*> _         = NullS
  (f :- fs) <*> (x :- xs) = f x :- fs <*> xs

instance Foldable Stream where
  foldr f z NullS     = z
  foldr f z (x :- xs) = f x (foldr f z xs)

infixl 4 §-, -§-
-- | operator for functional application on signals
(§-) :: ([a] -> b) -> (Int, Signal a) -> Signal b
f §- (_, NullS) = NullS
f §- (c, xs)    = let x  = fromSignal $ takeS c xs 
                      xs'= dropS c xs
                  in if length x == c then f x :- f §- (c,xs') else NullS

(-§-) :: Signal ([a] -> b) -> (Int, Signal a) -> Signal b
NullS   -§- _          = NullS
_       -§- (_, NullS) = NullS
(f:-fs) -§- (c, xs)    = let x  = fromSignal $ takeS c xs 
                             xs'= dropS c xs
                         in if length x == c then f x :- fs -§- (c,xs') else NullS

-- | instance defining the SDFnchronous MoC behavior
instance MoC Stream where
  -- | To abide the SDFnchronicicy law, a data-filtered signal must replace missing tokens with 
  --   absent values, thus the filtered type is 'AbstExt'
  type TokenType Stream a = a 
  type Padded Stream a = AbstExt a
  --------
  xs -#- p = foldr (\a as -> if p a then a :- as else as)  NullS xs
  --------
  --safe = ((<$>) . (§>)) Prst
  --------
  a ->- b = b :- a


signal :: [a] -> Signal a 
signal []     = NullS
signal (x:xs) = x :- (signal xs)

-- | The function 'fromSignal' converts a signal into a list.
fromSignal :: Signal a -> [a]
fromSignal NullS   = []
fromSignal (x:-xs) = x : fromSignal xs

tokenize :: Signal [a] -> Signal a
tokenize = signal . concat . fromSignal

zipx :: Vector (Signal a) -> Signal (Vector (AbstExt a))
zipx = takeWhileS (allV isAbsent) . zippx
  where zippx NullV         = pure NullV
        zippx (x:>xs)       = (:>) <$> padS Abst (fmap pure x) <*> zippx xs

unzipx :: Signal (Vector (AbstExt a)) -> Vector (Signal a)
unzipx NullS     = pure NullS
unzipx (x :- xs) = fromPadded §> x <§> unzipx xs
  where fromPadded Abst as     = as
        fromPadded (Prst a) as = a :- as  


takeS 0 _      = NullS
takeS _ NullS  = NullS
takeS n (x:-xs) 
  | n <= 0    = NullS
  | otherwise = x :- takeS (n-1) xs

dropS 0 NullS = NullS
dropS _ NullS = NullS 
dropS n (x:-xs) 
  | n <= 0    = x:-xs
  | otherwise = dropS (n-1) xs

takeWhileS               :: (a -> Bool) -> Stream a -> Stream a
takeWhileS _ NullS      =  NullS
takeWhileS p (x:-xs)
  | p x       =  x :- takeWhileS p xs
  | otherwise =  NullS

padS :: a -> Stream a -> Stream a
padS y NullS   = y :- padS y NullS
padS y (x:-xs) = x :- (padS y xs)

anyS :: (a -> Bool) -> Stream a -> Bool
anyS _ NullS = False
anyS c (x :- xs) = c x || anyS c xs


allV c = all c . fromVector 
