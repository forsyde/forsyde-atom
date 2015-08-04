{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY.Signal where

import ForSyDe.Core hiding (Stream, NullS, (:-), anyS)


data Stream a = a :- Stream a | NullS deriving (Eq)

type Signal a = Stream (AbstExt a)

infixr 3 :-

ssignal :: [a] -> Signal a 
ssignal = signal . map Prst

signal :: [AbstExt a] -> Signal a 
signal []     = NullS
signal (x:xs) = x :- (signal xs)

-- | The function 'fromSignal' converts a signal into a list.
fromSignal :: Signal a -> [AbstExt a]
fromSignal NullS   = []
fromSignal (x:-xs) = x : fromSignal xs


instance Functor Stream where
  fmap _ NullS   = NullS
  fmap f (x:-xs) = f x :- fmap f xs

instance Applicative Stream where
  pure a  = a :- pure a
  _         <*> NullS     = NullS
  NullS     <*> _         = NullS
  (f :- fs) <*> (x :- xs) = f x :- fs <*> xs

infixl 4 §-, -§-, §§-, -§§-,  §§!-, -§§!-
-- | operator for functional application on signals
(§-) :: (AbstExt a -> b) -> Signal a -> Stream b
(§-) = (<$>)


(-§-) :: Stream (AbstExt a -> b) -> Signal a -> Stream b
(-§-) = (<*>)

-- | operator for functional application on signals
(§§-) :: (a -> b) -> Signal a -> Signal b
(§§-) = ((<$>).(<$>))

-- | operator for zipping signals
(-§§-) :: Signal (a -> b) -> Signal a -> Signal b
fs -§§- xs = (\f x -> f <*> x) <$> fs <*> xs

(§§!-) :: (a -> b) -> Signal a -> Signal b
f §§!- xs | anyS isAbsent xs = error "Unexpected absent value in szip"
          | otherwise         = f §§- xs

-- | operator for zipping signals
(-§§!-) :: Signal (a -> b) -> Signal a -> Signal b
fs -§§!- xs | anyS isAbsent xs = error "Unexpected absent value in szip"
            | otherwise         = fs -§§- xs

-- | instance defining the Synchronous MoC behavior
instance MoC Stream where
  -- | To abide the synchronicicy law, a data-filtered signal must replace missing tokens with 
  --   absent values, thus the filtered type is 'AbstExt'
  type Padded Stream a = a 
  type TokenType Stream a = AbstExt a
  --------
  xs -#- p = (\x -> if (p <$> x) == Prst True then x else Abst) §- xs
  xs ->- i = i :- xs
  --------
  safe xs = xs
  --------
  zipx NullV = (pure . pure) NullV
  zipx (x:>xs) =  (:>) §§- x -§§- zipx xs
  --------
  unzipx NullS            = pure NullS
  unzipx (Abst :- xs)     = (:-) §> pure Abst <§> unzipx xs
  unzipx ((Prst x) :- xs) = ((:-) . Prst) §> x <§> unzipx xs
  --------



anyS :: (a -> Bool) -> Stream a -> Bool
anyS _ NullS = False
anyS c (x :- xs) = c x || anyS c xs


  
-- | 'Show' instance for a SY signal. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (Stream a) where
  showsPrec p = showParen (p>1) . showSignal
    where
      showSignal (x :- xs)  = showChar '{' . showEvent x . showSignal' xs
      showSignal (NullS)     = showChar '{' . showChar '}'
      showSignal' (x :- xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' (NullS)    = showChar '}'
      showEvent x           = shows x


