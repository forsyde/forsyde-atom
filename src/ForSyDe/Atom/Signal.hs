{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Signal' datatype and
-- utility functions operating on it. In ForSyDe a signal is
-- represented as a (partially or totally) /ordered/ sequence of
-- events that enables processes to communicate and synchronize.  The
-- 'Signal' type is but an ordered structure to encapsulate events as
-- infinite streams.
--
-- __/OBS:/__ although there is no constraint on the type of @e@ to
-- avoid unnecessary language extensions or cyclic dependencies, the
-- convention throughout the library is that @e@ is an instance of
-- 'ForSyDe.Atom.MoC.MoC'.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Signal  where


infixr 3 :-
-- | As a data type, 'Signal' is defined only as a stream of events,
-- encapsulating them in a structure similar to an infinite list. By
-- doing so, it adheres fully to the
-- <https://www.cs.ox.ac.uk/files/3378/PRG56.pdf Bird-Merteens formalism>
-- <#bird87 [3]>, thus all properties of lists may also be applied to
-- 'Signal's. This has deep repercussions in the execution and
-- evaluation mechanisms of the simulator, also propagated in the
-- library design. For example, lazy evaluation mechanism permits us
-- to create and simulate infinite signals, but on the other hand
-- imposes that the first/previous event is always fully
-- evaluated. Not respecting this condition immediately emerges as a
-- deadlock in self-referential calls (i.e. zero-delay feedback
-- loops). This can be translated into:
--
-- [design rule #1] all process network paths associated with a
-- feedback loop must contain (at least) one delay process to ensure
-- an initial event.
--
-- The above rule, although limiting the design options and denying
-- the simulation of network structures that otherwise might be sane
-- or synthesizable, is often featured in the execution model for many
-- systems based on the data flow assumption (e.g. Kahn Process
-- Networks <#kahn76 [4]>). The data flow model inferred from the
-- Bird-Merteens formalism also imposes an uninterrupted stream of
-- data in order to have an evaluatable kernel every time a new token
-- is produced (i.e. to avoid deadlocks). Thus we can enunciate:
--
-- [design rule #2] all processes are forbidden to "clean up" any
-- produced tokens. In other words, for each new input at any instant
-- in time, a process must produce /at least/ one output event.
--
-- Coming back to the implementation at hand, the order of a signal is
-- determined by the order of application of its constructors. So the
-- finite signal @{e0, e1, e2, e3}@ is created using the operations
--
-- > e0 :- e1 :- e2 :- e3 :- NullS
--
data Signal e = NullS         -- ^ terminates a signal
              | e :- Signal e -- ^ the default constructor appends an
                              -- event to the head of the stream

-- | allows for the mapping of an arbitrary function @(a -> b)@ upon
-- all the events of a @('Signal' a)@.
instance Functor Signal where
  fmap _ NullS   = NullS
  fmap f (x:-xs) = f x :- fmap f xs

-- | enables the 'Signal' to behave like a 'Control.Applicative.ZipList'
instance Applicative Signal where
  pure x = x :- NullS
  _ <*> NullS = NullS
  NullS <*> _ = NullS
  (f:-fs) <*> (x:-xs) = f x :- fs <*> xs 

-- | provides folding functions useful for implementing utilities, such as 'length'.
instance Foldable Signal where
  foldr k z = go
    where
      go NullS   = z
      go (y:-ys) = y `k` go ys


-- | signal @(1 :- 2 :- NullS)@ is represented as @{1,2}@.
instance (Show a) => Show (Signal a) where
  showsPrec p = showParen (p>1) . showSignal
    where
      showSignal (x :- xs)  = showChar '{' . showEvent x . showSignal' xs
      showSignal (NullS)    = showChar '{' . showChar '}'
      showSignal' (x :- xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' (NullS)   = showChar '}'
      showEvent x           = shows x

-- | signal @(1 :- 2 :- NullS)@ is read using the string @"{1,2}"@.
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

-- | Returns the head of a signal.
headS :: Signal a -> a
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
repeatS :: a -> Signal a
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
takeWhileS               :: (a -> Bool) -> Signal a -> Signal a
takeWhileS _ NullS      =  NullS
takeWhileS p (x:-xs)
  | p x       =  x :- takeWhileS p xs
  | otherwise =  NullS

-- | Concatenates two signals.
(+-+) NullS   ys = ys
(+-+) (x:-xs) ys = x :- (xs +-+ ys)

-- padS :: a -> Signal a -> Signal a
-- padS y NullS   = y :- padS y NullS
-- padS y (x:-xs) = x :- (padS y xs)

-- anyS :: (a -> Bool) -> Signal a -> Bool
-- anyS _ NullS = False
-- anyS c (x :- xs) = c x || anyS c xs
