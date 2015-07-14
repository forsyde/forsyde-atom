{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.SY.Signal (
         SignalSY(..),
       ) where

import ForSyDe.Shallow.Core

-- | A synchronous (SY) signal is defined as a list of events. An event has a tag and a value. 
--   The tag of an event is defined by the position in the list.  
data SignalSY a = a :- SignalSY a | NullS

infixr 3 :-
  
-- | 'Show' instance for a SY signal. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (SignalSY a) where
  showsPrec p = showParen (p>1) . showSignal
    where
      showSignal (x :- xs)  = showChar '{' . showEvent x . showSignal' xs
      showSignal (NullS)     = showChar '{' . showChar '}'
      showSignal' (x :- xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' (NullS)    = showChar '}'
      showEvent x           = shows x

-- | 'Read' instance for a SY signal. The signal 1 :- 2 :- NullS is read using the string \"\{1,2\}\".
instance (Read a) => Read (SignalSY a) where
  readsPrec d = readParen (d>1) readSignalStart
    where
      readSignalStart = (\ a -> [(xs,c) | ("{",b) <- lex a , (xs,c) <- readSignal (',' : b) ++ readNull b])
      readSignal r    = readEvent r ++ readNull r
      readEvent a     = [(x :- xs,d) | (",",b) <- lex a , (x,c) <- reads b , (xs,d) <- readSignal c]
      readNull a      = [(NullS,b) | ("}",b) <- lex a]

-- | provides 'fmap'
instance Functor SignalSY where
  fmap _ NullS    = NullS
  fmap f (x:-xs) = f x :- fmap f xs

-- | provides 'pure', '<*>', '<$>'
instance Applicative SignalSY where
  pure a = a :- NullS
  _         <*> NullS      = NullS
  NullS      <*> _         = NullS
  (f :- fs) <*> (x :- xs) = f x :- fs <*> xs

-- | 'Signals' instance for a  signal
instance Signals SignalSY where
  type Filtered SignalSY a = AbstExt a 
  --------
  signal []     = NullS
  signal (x:xs) = x :- signal xs 
  --------
  fromSignal NullS  =  []
  fromSignal (x:-xs) =  x : fromSignal xs
  --------
  nullS = NullS
  (-:-) = (:-) 
  --------
  NullS    -++- ys = ys
  (x:-xs) -++- ys = x :- (xs -++- ys)
  --------
  xs -#- p = fmap (\x -> if p x then Prst x else Abst) xs
  --------
  applyPattern NullS a _ = a
  applyPattern _ _ b = b

