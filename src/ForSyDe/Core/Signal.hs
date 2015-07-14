{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
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
module ForSyDe.Core.Signal where

-- | A  signal is defined as a list of events. An event has a tag and a value. 
--   The tag of an event is defined by the position in the list.  
data Signal a = a :- Signal a | NullS


infixr 3 :-
  
-- | 'Show' instance for a SY signal. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (Signal a) where
  showsPrec p = showParen (p>1) . showSignal
    where
      showSignal (x :- xs)  = showChar '{' . showEvent x . showSignal' xs
      showSignal (NullS)     = showChar '{' . showChar '}'
      showSignal' (x :- xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' (NullS)    = showChar '}'
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

-- | Test signal
class (Applicative s) => Signals s where
    -- The data type associated with a filtered output.
  type Filtered s a

  toS :: s a -> Signal a

  fromS :: Signal a -> s a

  -- | operator for functional application on signals
  (§-) :: (a -> b) -> s a -> s b

  -- | operator for zipping signals
  (-§-) :: s (a -> b) -> s a -> s b

  -- | operator for the default delay function
  (->-) :: s a -> a -> s a 

  -- | filtering operator
  (-#-) :: s a -> (a -> Bool) -> s (Filtered s a) 

  ----------------------------------------
  xs ->- x  = fromS $ x :- toS xs
  (§-)  = (<$>)
  (-§-) = (<*>)

infixl 4 ->-
infixl 4 -#-
infixl 4 §-, -§-
