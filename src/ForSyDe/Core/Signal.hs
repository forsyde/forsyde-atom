-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Signal' datatype and
-- functions operating on it.
-----------------------------------------------------------------------------
module ForSyDe.Core.Signal (
  Signal (..), lengthS,
  signal, fromSignal, headS, tailS, isNull, takeS, dropS, repeatS, takeWhileS, padS, (+-+), anyS,
) where

-- | A  Signal is defined as a list of events. An event has a tag and a value. 
--   The tag of an event is defined by the position in the list. 
--
--   This is the base data type for the 'ForSyDe.MoC.MoC'-bound signals, which are further described
--   by becoming instances of the 'Signal' type class.
data Signal a = a :- Signal a | NullS deriving (Eq)

infixr 3 :-

instance Functor Signal where
  fmap _ NullS   = NullS
  fmap f (x:-xs) = f x :- fmap f xs

instance Applicative Signal where
  pure a  = a :- pure a
  _         <*> NullS     = NullS
  NullS     <*> _         = NullS
  (f :- fs) <*> (x :- xs) = f x :- fs <*> xs

instance Foldable Signal where
  foldr f z NullS     = z
  foldr f z (x :- xs) = f x (foldr f z xs)
  
-- | 'Show' instance for a SY signal. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (Signal a) where
  showsPrec p = showParen (p>1) . showSignal
    where
      showSignal (x :- xs)  = showChar '{' . showEvent x . showSignal' xs
      showSignal (NullS)    = showChar '{' . showChar '}'
      showSignal' (x :- xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' (NullS)   = showChar '}'
      showEvent x           = shows x

-- | 'Read' instance for a SY signal. The signal 1 :- 2 :- NullS is read using the string \"\{1,2\}\".
instance (Read a) => Read (Signal a) where
  readsPrec d = readParen (d>1) readSignalStart
    where
      readSignalStart = (\ a -> [(xs,c) | ("{",b) <- lex a , (xs,c) <- readSignal (',' : b) ++ readNull b])
      readSignal r    = readEvent r ++ readNull r
      readEvent a     = [(x :- xs,d) | (",",b) <- lex a , (x,c) <- reads b , (xs,d) <- readSignal c]
      readNull a      = [(NullS,b) | ("}",b) <- lex a]

-- | The function 'signal' converts a list into a signal.
signal :: [a] -> Signal a 
signal []     = NullS
signal (x:xs) = x :- (signal xs)

-- | The function 'fromSignal' converts a signal into a list.
fromSignal :: Signal a -> [a]
fromSignal NullS   = []
fromSignal (x:-xs) = x : fromSignal xs

lengthS :: Signal a -> Int
lengthS NullS     = 0
lengthS (x :- xs) = 1 + lengthS xs

headS :: Signal a -> a
headS NullS    = error "Empty signal"
headS (x :- _) = x

tailS NullS  = NullS
tailS (_ :- a) = a

isNull NullS = True
isNull _ = False

repeatS :: a -> Signal a
repeatS a = a :- repeatS a

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

takeWhileS               :: (a -> Bool) -> Signal a -> Signal a
takeWhileS _ NullS      =  NullS
takeWhileS p (x:-xs)
  | p x       =  x :- takeWhileS p xs
  | otherwise =  NullS

(+-+) NullS   ys = ys
(+-+) (x:-xs) ys = x :- (xs +-+ ys)

padS :: a -> Signal a -> Signal a
padS y NullS   = y :- padS y NullS
padS y (x:-xs) = x :- (padS y xs)

anyS :: (a -> Bool) -> Signal a -> Bool
anyS _ NullS = False
anyS c (x :- xs) = c x || anyS c xs
