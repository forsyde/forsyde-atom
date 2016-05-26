-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Signal' datatype and
-- functions operating on it.
-----------------------------------------------------------------------------
module ForSyDe.Core.Signal  where


infixr 3 :-
-- | As a data type, 'Signal' is defined only as a stream of events,
-- encapsulating them in a structure similar to an infinite list. By
-- doing so, we benefit from the
-- <https://www.cs.ox.ac.uk/files/3378/PRG56.pdf Bird-Merteens formalism>,
-- thus all properties of lists may be applied to 'Signal's also.
-- 
-- A signal's order is determined by the order of application of its
-- constructors. Thus the finite signal @{e0, e1, e2, e3}@ is created
-- using the operations
--
-- > e0 :- e1 :- e2 :- e3 :- NullS
--
-- As can be seen, no further constraints are posed on the type of
-- @e@ in order to avoid language extensions as much as
-- possible. This imposes a convention that throughout the library,
-- @e@ will be of event type.
data Signal e = NullS         -- ^ terminates a signal
              | e :- Signal e -- ^ the default constructor appends an
                              -- event to the head of the stream
              deriving (Eq)

-- | 'Functor' instance, allows for the mapping of a function @(a ->b)@
-- upon all the events of a @'Signal' a@. See the Processes section
-- in "ForSyDe.Core#processes".
instance Functor Signal where
  fmap _ NullS   = NullS
  fmap f (x:-xs) = f x :- fmap f xs
  
-- | 'Show' instance. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (Signal a) where
  showsPrec p = showParen (p>1) . showSignal
    where
      showSignal (x :- xs)  = showChar '{' . showEvent x . showSignal' xs
      showSignal (NullS)    = showChar '{' . showChar '}'
      showSignal' (x :- xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' (NullS)   = showChar '}'
      showEvent x           = shows x

-- | 'Read' instance. The signal 1 :- 2 :- NullS is read using the string \"\{1,2\}\".
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
