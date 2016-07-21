{-# LANGUAGE TypeFamilies #-}
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

import ForSyDe.Atom.MoC.Atom
import ForSyDe.Atom.MoC.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility (($$),($$$),($$$$))

import Numeric
import Data.Ratio

type Time = Rational

-- | Type alias for a CT signal
type Sig   a = S.Signal (Event a)
type Event a = CT (Value a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data CT a = CT Time (Time -> a) 


instance Partitioned CT where
  type Arg CT c a = CTArg c a
  type Context CT c = ()
  o = (fmap . fmap) unArg

-- | Implenents the CT semantics for the MoC atoms
instance MoC CT where
  _ -$- NullS = NullS
  f -$- (x:-xs) = f >$< x :- f -$- xs
  ---------------------
  sf -*- sx = init (CT 0.0 (\_ -> Undef)) sf sx
    where init px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  >*< x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  >*< px :- comb f  px fs s2
            | tag f >  tag x        = x %> f  >*< ue :- init    x  s1 xs
          comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  >*< x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  >*< px :- comb f  px fs s2
            | tag f >  tag x        = x %> pf >*< x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  >*< px :- comb f px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf >*< x  :- comb pf x NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (CT _ av) ->- xs = CT 0 (unArg <$> av) :- xs
  ---------------------
  (CT t _) -&- xs = (\(CT t1 v) -> CT (t1 + t) v) <$> xs
    

-- | Shows the event with tag @t@ and value @v@ as @(\@t:v)@
instance Show a => Show (CT a) where
  showsPrec _ (CT t x) = (++) ( show (x t) ++ "@" ++ (showFFloat Nothing $ fromRat t) "" )


-- | Needed to implement the @unzip@ utilities
instance Functor (CT) where
  fmap f (CT t a) = CT t (f <$> a)

-- | Needed to implement the @unzip@ utilities
instance Applicative (CT) where
  pure a = CT 0.0 (\_ -> a)
  (CT t f) <*> (CT _ x) = CT t (\a -> (f a) (x a))

-----------------------------------------------------------------------------

newtype CTArg c a = CTArg {unArg :: a }

instance Functor (CTArg c) where
  fmap f (CTArg a) = CTArg (f a)

instance Applicative (CTArg c) where
  pure = CTArg
  (CTArg f) <*> (CTArg a) = CTArg (f a)

infixl 4 >$<, >*<
(>$<) = fmap . (\f a -> f $ CTArg a)
fs >*< gs = fs <*> (fmap CTArg gs)

infixl 7 %>
(CT t _) %> (CT _ x) = CT t x
tag (CT t _) = t 
ue = CT 0.0 (\_ -> Undef) :: Event x

-----------------------------------------------------------------------------


-- | Wraps a tuple @(tag, value)@ into a DE event of extended values
event  :: (Rational, Rational -> a) -> CT (CTArg c (Value a))
event (t,f) = CT t (pure . Value . f)
event2  = ($$) (event,event)
event3  = ($$$) (event,event,event)
event4  = ($$$$) (event,event,event,event)


-- -- | Wraps a tuple @(tag, value)@ into a CT argument of extended values
-- argument  :: (Rational, Rational -> a) -> CT (CTArg c a)
-- argument (t,f) = CT t (pure . f)
-- argument2  = ($$) (argument,argument)
-- argument3  = ($$$) (argument,argument,argument)
-- argument4  = ($$$$) (argument,argument,argument,argument)


-- -- | Wraps a tuple @(tag, value)@ into a DE event of extended values
-- event       :: (Rational, Rational -> a) -> Event a
-- event (t,f) = CT t (Value . f)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a DE signal
signal   :: [(Rational, Rational -> a)] -> Sig a
signal l = S.signal $ (\(t,f) -> CT t (Value . f)) <$> l

partition :: Rational -> Sig a -> Sig a 
partition _    NullS     = NullS
partition sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t < nt    = CT t f :- chunk (CT (t+sample) f) s
          | otherwise = CT nt nf :- chunk (CT nt nf) fs
        chunk _ NullS = NullS

partitionUntil _ _ NullS = NullS
partitionUntil until sample (x:-xs) = chunk x xs
  where chunk (CT t f) s@(CT nt nf :- fs)
          | t <= until && t < nt  = CT t f :- chunk (CT (t+sample) f) s
          | t <= until && t >= nt = CT nt nf :- chunk (CT nt nf) fs
          | otherwise = NullS
        chunk (CT t f) NullS
          | t <= until = CT t f :- chunk (CT (t+sample) f) NullS
          | otherwise  = NullS



----------------------------------------------------------------------------- 


-- s = ForSyDe.Atom.MoC.CT.Core.signal [(0, \_ -> 1), (2, \_ -> 0)]
-- i = argument (1, \t -> t)
-- q = i ->- (i -&- s)

-- w = o $ psi21 (+) -$- s -*- q
