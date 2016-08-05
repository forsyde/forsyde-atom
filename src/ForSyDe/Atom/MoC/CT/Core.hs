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

import ForSyDe.Atom.MoC.AtomLib
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility

import Numeric
import Data.Ratio

type Time = Rational

-- | Type alias for a CT signal
type Sig   a = S.Signal (Event a)
type Event a = CT ([Value a])

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data CT a  = CT { tag :: Time, func :: Time -> a }

-- | Implenents the CT semantics for the MoC atoms
instance MoC CT where
  type Param CT = ()
  ---------------------
  (-$-) (_,f) = (,) () . (fmap . fmap) f
  ---------------------
  (_,sf) -*- sx = ((), init ue sf sx)
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
-- These functions are not exported and are used for testing purpose only.

part :: (Time, Time -> a) -> CT [a]
part (t,f) = CT t (pure . f)

stream :: [(Time, Time -> a)] -> Signal (CT [a])
stream l = S.signal (part <$> l)

infixl 7 %>
(CT t _) %> (CT _ x) = CT t x
ue = CT 0.0 (\_ -> [Undef]) :: Event x

-- end of testbench functions
-----------------------------------------------------------------------------

-- | Wraps a tuple @(tag, value)@ into a CT event of extended values
event  :: (Time, Time -> a) -> Event a 
event (t,f) = CT t (pure . Value . f)
event2 = ($$) (event,event)
event3 = ($$$) (event,event,event)
event4 = ($$$$) (event,event,event,event)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a CT signal
signal   :: [(Time, Time -> a)] -> Sig a
signal l = S.signal (event <$> l)

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


eval s = (\(CT t f) -> f t) <$> s

----------------------------------------------------------------------------- 

wrap11 :: (a1->b1)                             -> ((), [a1]->[b1])
wrap21 :: (a1->a2->b1)                         -> ((), [a1]->[a2]->[b1])
wrap31 :: (a1->a2->a3->b1)                     -> ((), [a1]->[a2]->[a3]->[b1])
wrap41 :: (a1->a2->a3->a4->b1)                 -> ((), [a1]->[a2]->[a3]->[a4]->[b1])
wrap51 :: (a1->a2->a3->a4->a5->b1)             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[b1])
wrap61 :: (a1->a2->a3->a4->a5->a6->b1)         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[b1])
wrap71 :: (a1->a2->a3->a4->a5->a6->a7->b1)     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[b1])
wrap81 :: (a1->a2->a3->a4->a5->a6->a7->a8->b1) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->[b1])

wrap12 :: (a1->(b1,b2))                             -> ((), [a1]->([b1],[b2]))
wrap22 :: (a1->a2->(b1,b2))                         -> ((), [a1]->[a2]->([b1],[b2]))
wrap32 :: (a1->a2->a3->(b1,b2))                     -> ((), [a1]->[a2]->[a3]->([b1],[b2]))
wrap42 :: (a1->a2->a3->a4->(b1,b2))                 -> ((), [a1]->[a2]->[a3]->[a4]->([b1],[b2]))
wrap52 :: (a1->a2->a3->a4->a5->(b1,b2))             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->([b1],[b2]))
wrap62 :: (a1->a2->a3->a4->a5->a6->(b1,b2))         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->([b1],[b2]))
wrap72 :: (a1->a2->a3->a4->a5->a6->a7->(b1,b2))     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->([b1],[b2]))
wrap82 :: (a1->a2->a3->a4->a5->a6->a7->a8->(b1,b2)) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->([b1],[b2]))

wrap13 :: (a1->(b1,b2,b3))                             -> ((), [a1]->([b1],[b2],[b3]))
wrap23 :: (a1->a2->(b1,b2,b3))                         -> ((), [a1]->[a2]->([b1],[b2],[b3]))
wrap33 :: (a1->a2->a3->(b1,b2,b3))                     -> ((), [a1]->[a2]->[a3]->([b1],[b2],[b3]))
wrap43 :: (a1->a2->a3->a4->(b1,b2,b3))                 -> ((), [a1]->[a2]->[a3]->[a4]->([b1],[b2],[b3]))
wrap53 :: (a1->a2->a3->a4->a5->(b1,b2,b3))             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->([b1],[b2],[b3]))
wrap63 :: (a1->a2->a3->a4->a5->a6->(b1,b2,b3))         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->([b1],[b2],[b3]))
wrap73 :: (a1->a2->a3->a4->a5->a6->a7->(b1,b2,b3))     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->([b1],[b2],[b3]))
wrap83 :: (a1->a2->a3->a4->a5->a6->a7->a8->(b1,b2,b3)) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->([b1],[b2],[b3]))

wrap14 :: (a1->(b1,b2,b3,b4))                             -> ((), [a1]->([b1],[b2],[b3],[b4]))
wrap24 :: (a1->a2->(b1,b2,b3,b4))                         -> ((), [a1]->[a2]->([b1],[b2],[b3],[b4]))
wrap34 :: (a1->a2->a3->(b1,b2,b3,b4))                     -> ((), [a1]->[a2]->[a3]->([b1],[b2],[b3],[b4]))
wrap44 :: (a1->a2->a3->a4->(b1,b2,b3,b4))                 -> ((), [a1]->[a2]->[a3]->[a4]->([b1],[b2],[b3],[b4]))
wrap54 :: (a1->a2->a3->a4->a5->(b1,b2,b3,b4))             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->([b1],[b2],[b3],[b4]))
wrap64 :: (a1->a2->a3->a4->a5->a6->(b1,b2,b3,b4))         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->([b1],[b2],[b3],[b4]))
wrap74 :: (a1->a2->a3->a4->a5->a6->a7->(b1,b2,b3,b4))     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->([b1],[b2],[b3],[b4]))
wrap84 :: (a1->a2->a3->a4->a5->a6->a7->a8->(b1,b2,b3,b4)) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->([b1],[b2],[b3],[b4]))

wrap11 = (,) () . psi11
wrap21 = (,) () . psi21
wrap31 = (,) () . psi31
wrap41 = (,) () . psi41
wrap51 = (,) () . psi51
wrap61 = (,) () . psi61
wrap71 = (,) () . psi71
wrap81 = (,) () . psi81
wrap12 = (,) () . psi12
wrap22 = (,) () . psi22
wrap32 = (,) () . psi32
wrap42 = (,) () . psi42
wrap52 = (,) () . psi52
wrap62 = (,) () . psi62
wrap72 = (,) () . psi72
wrap82 = (,) () . psi82
wrap13 = (,) () . psi13
wrap23 = (,) () . psi23
wrap33 = (,) () . psi33
wrap43 = (,) () . psi43
wrap53 = (,) () . psi53
wrap63 = (,) () . psi63
wrap73 = (,) () . psi73
wrap83 = (,) () . psi83
wrap14 = (,) () . psi14
wrap24 = (,) () . psi24
wrap34 = (,) () . psi34
wrap44 = (,) () . psi44
wrap54 = (,) () . psi54
wrap64 = (,) () . psi64
wrap74 = (,) () . psi74
wrap84 = (,) () . psi84




-- s = ForSyDe.Atom.MoC.CT.Core.signal [(0, \_ -> 1), (2, \_ -> 0)]
-- i = event (1, \t -> t)
-- q = i ->- (i -&- s)

-- w = wrap21 (psi21 (+)) -$- s -*- q
