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

import ForSyDe.Atom.MoC hiding (Sig)
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility

import Numeric
import Data.Ratio

-- | Type alias for the tag type to represent metric time. Underneath
-- we use 'Rational' that is able to represent any /t/ between
-- /t&#8321;/ < /t&#8322;/ &#8712; /T/.
type Time = Rational

-- | Type synonym for a CT event, i.e. "a constructed event of
-- partitioned function on 'Time' returning extended values, where the
-- partition size is always 1"
type Event a = CT ([Value a])

-- | Type synonym for a CT signal, i.e. "a signal of CT events"
type Sig   a = S.Signal (Event a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data CT a  = CT { tag :: Time, func :: Time -> a }

-- | Implenents the execution and synchronization semantics for the CT
-- MoC through its atoms.
instance MoC CT where
  type Context CT = ()
  ---------------------
  (-$-) (_,f) = (fmap . fmap) f
  ---------------------
  sf -*- sx = init ue (extractFunction <$> sf) sx
    where init px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> f  <*> ue :- comb f  x  s1 xs
          init _ _ NullS            = NullS
          init _ NullS _            = NullS
          comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (CT _ v) ->- xs =  (CT 0 v) :- xs
  ---------------------
  (CT t _) -&- xs = (\(CT t1 v) -> CT (t1 + t) v) <$> xs
  ---------------------
  -- sniff (CT t a) = a t
    
-- | Shows the event starting from tag @t@ with value @v = f t@  @ v \@t@. It hides the partition (the singleton list constructor).
instance Show a => Show (CT [a]) where
  showsPrec _ (CT t f) = (++) ( " " ++ show (head $ f t) ++ " @" ++ (showFFloat Nothing $ fromRat t) "" )
  
-- -- | A more efficient instatiation since we /know/ that the partition
-- -- size is always 1.
-- instance Eq a => Eq (CT a) where
--   (CT t1 a) == (CT t2 b) = a t1 == b t2

-- | Defines the equality operator between two CT signals. __TODO!__ incomplete definition.
instance Eq a => Eq (Signal (CT [a])) where
  a == b = flatten a == flatten b
    where flatten = concat . map (\(CT t f) -> f t) . fromSignal

-- | Allows for mapping of functions on a CT event.
instance Functor (CT) where
  fmap f (CT t a) = CT t (f <$> a)

-- | Allows for lifting functions on a pair of CT events.
instance Applicative (CT) where
  pure a = CT 0.0 (\_ -> a)
  (CT t f) <*> (CT _ x) = CT t (\a -> (f a) (x a))

-----------------------------------------------------------------------------
-- These functions are not exported and are used for testing purpose only.

part :: (Time, Time -> a) -> CT [a]
part (t,f) = CT t (pure . f)

stream :: [(Time, Time -> a)] -> Signal (CT [a])
stream l = S.signal (part <$> l)

infixl 7 %>
(CT t _) %> (CT _ x) = CT t x
ue = CT 0.0 (\_ -> [Undef]) :: Event x

extractFunction (CT t f) = CT t (snd . f)

-- end of testbench functions
-----------------------------------------------------------------------------

-- -- | Wraps a tuple @(tag, value)@ into a CT event of extended values
event  :: (Time, Time -> a) -> Event a 
event (t,f) = CT t (pure . Value . f)

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
signal   :: [(Time, Time -> a)] -> Sig a
signal l = S.signal (event <$> l)

-- | Discretizes a signal by splitting the events into multiple events
-- of constant duration. The return signal has an infinite number of
-- events.
split :: Time -- ^ resolution
         -> Sig a -> Sig a 
split _    NullS     = NullS
split sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t < nt    = CT t f :- chunk (CT (t+sample) f) s
          | otherwise = CT nt nf :- chunk (CT nt nf) fs
        chunk _ NullS = NullS


-- | Discretizes a signal by splitting the events into multiple events
-- of constant duration, until a given time.
splitUntil :: Time -- ^ end time
           -> Time -- ^ resolution
           -> Signal (CT a) -> Signal (CT a) 
splitUntil _ _ NullS = NullS
splitUntil until sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t <= until && t < nt  = CT t f :- chunk (CT (t+sample) f) s
          | t <= until && t >= nt = CT nt nf :- chunk (CT nt nf) fs
          | otherwise = NullS
        chunk (CT t f) NullS
          | t <= until = CT t f :- chunk (CT (t+sample) f) NullS
          | otherwise  = NullS

eval s = (\(CT t f) -> f t) <$> s

----------------------------------------------------------------------------- 

-- | Wraps a function on extended values into the format needed by the
-- MoC atoms.
--
-- <<includes/figs/timed-wrapper-formula1.png>>
--
-- "ForSyDe.Atom.MoC.DE" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > wrap11, wrap21, wrap31, wrap41, wrap51, wrap61, wrap71, wrap81, 
-- > wrap12, wrap22, wrap32, wrap42, wrap52, wrap62, wrap72, wrap82, 
-- > wrap13, wrap23, wrap33, wrap43, wrap53, wrap63, wrap73, wrap83, 
-- > wrap14, wrap24, wrap34, wrap44, wrap54, wrap64, wrap74, wrap84,
wrap22 :: (Value a1 -> Value a2 -> (Value b1, Value b2))
       -> ((), [Value a1] -> ((), [Value a2] -> ([Value b1], [Value b2])))


wrap f = ((), \x -> f x)

wrap11 f = wrap $ (map f)
wrap21 f = wrap $ wrap11 . f . head 
wrap31 f = wrap $ wrap21 . f . head
wrap41 f = wrap $ wrap31 . f . head
wrap51 f = wrap $ wrap41 . f . head
wrap61 f = wrap $ wrap51 . f . head
wrap71 f = wrap $ wrap61 . f . head
wrap81 f = wrap $ wrap71 . f . head

wrap12 f = wrap $ ((|<) . map f)
wrap22 f = wrap $ wrap12 . f . head 
wrap32 f = wrap $ wrap22 . f . head
wrap42 f = wrap $ wrap32 . f . head
wrap52 f = wrap $ wrap42 . f . head
wrap62 f = wrap $ wrap52 . f . head
wrap72 f = wrap $ wrap62 . f . head
wrap82 f = wrap $ wrap72 . f . head

wrap13 f = wrap $ ((|<<) . map f)
wrap23 f = wrap $ wrap13 . f . head 
wrap33 f = wrap $ wrap23 . f . head
wrap43 f = wrap $ wrap33 . f . head
wrap53 f = wrap $ wrap43 . f . head
wrap63 f = wrap $ wrap53 . f . head
wrap73 f = wrap $ wrap63 . f . head
wrap83 f = wrap $ wrap73 . f . head

wrap14 f = wrap $ ((|<<<) . map f)
wrap24 f = wrap $ wrap14 . f . head 
wrap34 f = wrap $ wrap24 . f . head
wrap44 f = wrap $ wrap34 . f . head
wrap54 f = wrap $ wrap44 . f . head
wrap64 f = wrap $ wrap54 . f . head
wrap74 f = wrap $ wrap64 . f . head
wrap84 f = wrap $ wrap74 . f . head



-- s = ForSyDe.Atom.MoC.CT.Core.signal [(0, \_ -> 1), (2, \_ -> 0)]
-- i = event (1, \t -> t)
-- q = i ->- (i -&- s)

-- w = wrap21 (psi21 (+)) -$- s -*- q
