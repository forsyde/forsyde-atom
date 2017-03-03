{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE.Core where

import ForSyDe.Atom.MoC.Timed
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.Utility (($$),($$$),($$$$))
import Numeric.Natural

-- | Type alias for timestamps. They are natural numbers to ensure /t/ &#8805; 0.
type Tag = Natural

-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Signal a   = Stream (DE a)

-- | The DE event. It identifies a discrete event signal.
data DE a  = DE { tag :: Tag,  -- ^ timestamp
                  val :: a     -- ^ the value
                } deriving (Eq)

-- | Implenents the execution and synchronization semantics for the DE
-- MoC through its atoms.
instance Timed DE where
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (f:-fs) -*- (x:-xs)               =      f  <*> x  :- comb f  x  fs xs
    where comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f  px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x  NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (DE _ v) -<- xs = pure v :- xs
  ---------------------
  (DE d _) -&- xs = (\(DE t v) -> DE (t + d) v) <$> xs
  ---------------------

-- | Shows the event with tag @t@ and value @v@ as @ v \@t@.
instance Show a => Show (DE a) where
  showsPrec _ (DE t x) = (++) ( " " ++ show x ++ " @" ++ show t )

-- | Reads the string of type @v\@t@ as an event @DE t v@.
instance (Read a) => Read (DE a) where
  readsPrec _ x = [ (DE tg val, r2)
                  | (val,r1) <- reads $ takeWhile (/='@') x
                  , (tg, r2) <- reads $ tail $ dropWhile (/='@') x ]

-- | Allows for mapping of functions on a DE event.
instance Functor DE where
  fmap f (DE t a) = DE t (f a)

-- | Allows for lifting functions on a pair of DE events.
instance Applicative DE where
  pure = DE 0
  (DE tf f) <*> (DE _ x) = DE tf (f x)

-----------------------------------------------------------------------------

-- | Wraps a tuple @(tag, value)@ into a DE event of extended values
event  :: (Tag, a) -> DE a 
event (t,v) = DE t v

-- | Wraps a (tuple of) pair(s) @(tag, value)@ into the equivalent
-- event container(s).
--
-- "ForSyDe.Atom.MoC.DE" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > event, event2, event3, event4,
event2 = ($$) (event,event)
event3 = ($$$) (event,event,event)
event4 = ($$$$) (event,event,event,event)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a DE signal
signal :: [(Tag, a)] -> Signal a
signal = checkSignal . stream . fmap event

-- | Reads a signal from a string and checks if it is well-formed. Like
-- with the @read@ function from @Prelude@, you must specify the tipe
-- of the signal.
--
-- >>> readSignal "{ 1@0, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- > { 1 @0, 2 @2, 3 @5, 4 @7, 5 @10}
-- >>> readSignal "{ 1@0, 2@2, 3@5, 4@10, 5@7 }" :: Signal Int
-- > { 1 @0, 2 @2, 3 @5*** Exception: DE: malformed signal
-- >>> readSignal "{ 1@1, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- > *** Exception: DE: input signal does not start from global 0
readSignal :: Read a => String -> Signal a
readSignal s = checkSignal $ read s

-- | Checks if a signal is well-formed or not, according to the DE MoC
-- interpretation in @ForSyDe-Atom@.
checkSignal NullS = NullS
checkSignal s@(x:-_)
  | tag x == 0 = checkOrder s
  | otherwise  = error "DE: input signal does not start from global 0"
  where
    checkOrder NullS      = NullS
    checkOrder (x:-NullS) = (x:-NullS)
    checkOrder (x:-y:-xs) | tag x < tag y = x :-checkOrder (y:-xs)
                          | otherwise = error "DE: malformed signal"

----------------------------------------------------------------------------- 
-- These functions are not exported and are used for testing purpose only.

infixl 7 %>
(DE t _) %> (DE _ x) = DE t x

-- end of testbench functions
-----------------------------------------------------------------------------
