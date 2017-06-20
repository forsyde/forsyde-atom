{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.CT.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the CT MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.CT.Core where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.Utility

import Numeric
import Data.Ratio

-- | Type alias for the tag type to represent metric time. Underneath
-- we use 'Rational' that is able to represent any /t/ between
-- /t&#8321;/ < /t&#8322;/ &#8712; /T/.
type Time = Rational

-- | Type synonym for a CT signal, i.e. "a signal of CT events"
type Signal a = Stream (CT a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data CT a  = CT { tag :: Time, phase :: Time, func :: Time -> a }

-- | Implenents the execution and synchronization semantics for the CT
-- MoC through its atoms.
instance MoC CT where
  ---------------------
  type Fun CT a b = a -> b
  type Ret CT b   = b 
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (f:-fs) -*- (x:-xs)           =      f  <*> x  :- comb f  x  fs xs
    where
      comb pf px s1@(f :- fs) s2@(x :- xs)
        | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
        | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
        | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
      comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f px fs NullS
      comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x NullS xs
      comb _ _ NullS NullS      = NullS
  ---------------------
  (-*) = id
  ---------------------
  (CT _ p v :- _) -<- xs = (CT 0 p v) :- xs
  ---------------------
  (CT 0 _ _ :- _) -&- xs = xs
  (CT d _ _ :- _) -&- xs = (\(CT t p v) -> CT (t + d) (p - d) v) <$> xs
  ---------------------
    
-- | Allows for mapping of functions on a CT event.
instance Functor CT where
  fmap f (CT t p g) = CT t p (f . g)

-- | Allows for lifting functions on a pair of CT events.
instance Applicative CT where
  pure x = CT 0 0 (\_ -> x)
  (CT t p f) <*> (CT _ _ g) = CT t p (\x -> (f x) (g x))

-----------------------------------------------------------------------------
-- These functions are not exported and are used for testing purpose only.

infixl 7 %>
(CT t p _) %> (CT _ _ x) = CT t p x

-- end of testbench functions
-----------------------------------------------------------------------------

eval (CT t p f) = f (t+p)

-- | Wraps a tuple @(tag, value)@ into a CT event
event  :: (Time, Time -> a) -> CT a 
event (t,f) = CT t 0 f


-- | Wraps a (tuple of) pair(s) @(tag, function)@ into the equivalent
-- event container(s).
--
-- "ForSyDe.Atom.MoC.CT" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > event, event2, event3, event4,
event2 = ($$) (event,event)
event3 = ($$$) (event,event,event)
event4 = ($$$$) (event,event,event,event)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a CT signal
signal   :: [(Time, Time -> a)] -> Signal a
signal l = stream (event <$> l)

    
-- -- | Shows the event starting from tag @t@ with value @v = f t@  @ v \@t@. It hides the partition (the singleton list constructor).
-- instance Show a => Show (CT [a]) where
--   showsPrec _ (CT t f) = (++) ( " " ++ show (head $ f t) ++ " @" ++ (showFFloat Nothing $ fromRat t) "" )
  
-- -- | A more efficient instatiation since we /know/ that the partition
-- -- size is always 1.
-- instance Eq a => Eq (CT a) where
--   (CT t1 a) == (CT t2 b) = a t1 == b t2

-- -- | Defines the equality operator between two CT signals. __TODO!__ incomplete definition.
-- instance Eq a => Eq (Signal (CT [a])) where
--   a == b = flatten a == flatten b
--     where flatten = concat . map (\(CT t f) -> f t) . fromSignal

