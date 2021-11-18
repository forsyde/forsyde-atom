{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Stream
-- Copyright   :  (c) George Ungureanu, 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Stream' data type and
-- utility functions operating on it. In ForSyDe a signal is
-- represented as a (partially or totally) /ordered/ sequence of
-- events that enables processes to communicate and synchronize. In
-- ForSyDe-Atom signals are the main operating type for the MoC
-- layer. The 'Stream' type is only the base (potentially infinite)
-- structure which can encapsulate events in order to describe
-- signals.
-----------------------------------------------------------------------------
module ForSyDe.Atom.MoC.Stream  where


infixr 3 :-
-- | Defines a stream of events, encapsulating them in a structure
-- isomorphic to an infinite list <ForSyDe-Atom.html#bird87 [Bird87]>,
-- thus all properties of lists may also be applied to
-- 'Stream's. While, in combination with lazy evaluation, it is
-- possible to create and simulate infinite signals, we need to ensure
-- that the first/previous event is always fully evaluated, which is
-- equivalent to ensuring the the monotonicity property in dataflow
-- systems. This translates in the following composition rule:
--
-- [non-causal feedback is forbidden] also called "zero-delay"
-- feedback loops, are caused by un-evaluated self-referential
-- calls. In a feedback loop, there always has to be enough events to
-- ensure the data flow.
--
-- This rule imposes that the stream of data is uninterrupted in order
-- to have an evaluatable kernel every time a new event is produced
-- (i.e. to avoid deadlocks), which is ther equivalent to ensuring
-- continuity in dataflow systems. This translates in the following
-- rule:
--
-- [cleaning of signals in a feedback is forbidden] in other words,
-- whenever a feedback composition occurs, for each new input at any
-- instant in time, a process must react with /at least/ one output
-- event.
data Stream e = NullS         -- ^ terminates a signal
              | e :- Stream e -- ^ the default constructor appends an
                              -- event to the head of the stream

-- | allows for the mapping of an arbitrary function @(a -> b)@ upon
-- all the events of a @('Stream' a)@.
instance Functor Stream where
  fmap _ NullS   = NullS
  fmap f (x:-xs) = f x :- fmap f xs

-- | enables the 'Stream' to behave like a 'Control.Applicative.ZipList'
instance Applicative Stream where
  pure x = x :- NullS
  _ <*> NullS = NullS
  NullS <*> _ = NullS
  (f:-fs) <*> (x:-xs) = f x :- fs <*> xs 

-- | provides folding functions useful for implementing utilities, such as 'length'.
instance Foldable Stream where
  foldr k z = go
    where
      go NullS   = z
      go (y:-ys) = y `k` go ys


-- | signal @(1 :- 2 :- NullS)@ is represented as @{1,2}@.
instance (Show a) => Show (Stream a) where
  showsPrec p = showParen (p>1) . showStream
    where
      showStream (x :- xs)  = showChar '{' . showEvent x . showStream' xs
      showStream (NullS)    = showChar '{' . showChar '}'
      showStream' (x :- xs) = showChar ',' . showEvent x . showStream' xs
      showStream' (NullS)   = showChar '}'
      showEvent x           = shows x

-- | signal @(1 :- 2 :- NullS)@ is read using the string @"{1,2}"@.
instance (Read a) => Read (Stream a) where
  readsPrec d = readParen (d>1) readStreamStart
    where
      readStreamStart = (\ a -> [(xs,c) | ("{",b) <- lex a , (xs,c) <- readStream (',' : b) ++ readNull b])
      readStream r    = readEvent r ++ readNull r
      readEvent a     = [(x :- xs,d) | (",",b) <- lex a , (x,c) <- reads b , (xs,d) <- readStream c]
      readNull a      = [(NullS,b) | ("}",b) <- lex a]

-- | The function 'stream' converts a list into a stream.
stream :: [a] -> Stream a 
stream []     = NullS
stream (x:xs) = x :- (stream xs)

-- | The function 'fromStream' converts a signal into a list.
fromStream :: Stream a -> [a]
fromStream NullS   = []
fromStream (x:-xs) = x : fromStream xs

-- | Returns the head of a signal.
headS :: Stream a -> a
headS NullS    = error "Empty signal"
headS (x :- _) = x

-- | Returns the tail of a signal
tailS NullS  = NullS
tailS (_ :- a) = a

-- | Returns the last event in a signal.
lastS NullS = error "Empty signal"
lastS (x:-NullS) = x
lastS (_:- xs)   = lastS xs

-- | Returns an infinite list containing the same repeated event.
repeatS :: a -> Stream a
repeatS a = a :- repeatS a

-- | Returns the first @n@ events in a signal.
takeS 0 _      = NullS
takeS _ NullS  = NullS
takeS n (x:-xs) 
  | n <= 0    = NullS
  | otherwise = x :- takeS (n-1) xs

-- | Drops the first @n@ events in a signal.
dropS 0 NullS = NullS
dropS _ NullS = NullS 
dropS n (x:-xs) 
  | n <= 0    = x:-xs
  | otherwise = dropS (n-1) xs

-- | Returns the first events of a signal which comply to a condition.
takeWhileS               :: (a -> Bool) -> Stream a -> Stream a
takeWhileS _ NullS      =  NullS
takeWhileS p (x:-xs)
  | p x       =  x :- takeWhileS p xs
  | otherwise =  NullS

-- | Concatenates two signals.
(+-+) NullS   ys = ys
(+-+) (x:-xs) ys = x :- (xs +-+ ys)

(-$-) :: Functor e => (a -> b) -> Stream (e a) -> Stream (e b)
(-$-) = fmap . fmap


-- padS :: a -> Stream a -> Stream a
-- padS y NullS   = y :- padS y NullS
-- padS y (x:-xs) = x :- (padS y xs)

-- anyS :: (a -> Bool) -> Stream a -> Bool
-- anyS _ NullS = False
-- anyS c (x :- xs) = c x || anyS c xs
