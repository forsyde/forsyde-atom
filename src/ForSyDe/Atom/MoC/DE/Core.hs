{-# LANGUAGE TypeFamilies #-}
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

import ForSyDe.Atom.MoC.AtomLib
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility


type Tag = Int

-- | Type alias for a DE signal
type Event a = DE ([Value a])
type Sig a   = S.Signal (Event a)

-- | The DE type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data DE a  = DE { tag :: Tag, val :: a }

-- | Implenents the DE semantics for the MoC atoms
instance MoC DE where
  type Param DE = ()
  ---------------------
  (-$-) (_,f) = (,) () . (fmap . fmap) f
  ---------------------
  (_,sf) -*- sx = ((), init ue sf sx)
    where init px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> f  <*> ue :- init    x  s1 xs
          comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (DE _ v) ->- xs = pure v :- xs
  ---------------------
  (DE t _) -&- xs = (\(DE t1 v) -> DE (t1 + t) v) <$> xs


-- | Shows the event with tag @t@ and value @v@ as @v\@t@
instance Show a => Show (DE a) where
  showsPrec _ (DE t x) = (++) ( show x ++ "@" ++ show t )

-- | Reads the string for a normal tuple @(Tag,Value a)@ as an event @DE a@
instance (Read a) => Read (DE a) where
  readsPrec _ x     = [(DE t v, x) | ((t,v), x) <- reads x]

-- | Needed to implement the @unzip@ utilities
instance Functor (DE) where
  fmap f (DE t a) = DE t (f a)

-- | Needed to implement the @unzip@ utilities
instance Applicative (DE) where
  pure = DE 0
  (DE tf f) <*> (DE _ x) = DE tf (f x)

-----------------------------------------------------------------------------

-- newtype DEArg c a = DEArg {unArg :: a }

-- instance Functor (DEArg c) where
--   fmap f (DEArg a) = DEArg (f a)

-- instance Applicative (DEArg c) where
--   pure = DEArg
--   (DEArg f) <*> (DEArg a) = DEArg (f a)

-- infixl 4 >$<, >*<
-- (>$<) = fmap . (\f a -> f $ DEArg a)
-- fs >*< gs = fs <*> (fmap DEArg gs)

infixl 7 %>
(DE t _) %> (DE _ x) = DE t x
--tag (DE t _) = t 
ue = DE 0 [Undef]


-----------------------------------------------------------------------------

-- | Wraps a tuple @(tag, value)@ into a DE event of extended values
event  :: (Tag, a) -> Event a 
event (t,v) = DE t $ (pure . pure) v
event2 = ($$) (event,event)
event3 = ($$$) (event,event,event)
event4 = ($$$$) (event,event,event,event)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a DE signal
signal   :: [(Tag, a)] -> Sig a
signal l = S.signal (event <$> l)

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



    -- where init (DE ptx px) s1@(DE tf f :- fs) s2@(DE tx x :- xs)
    --         | tf == tx = DE tf (f  x) :- comb (DE tf f) (DE  tx  x) fs xs
    --         | tf <  tx = DE tf (f px) :- comb (DE tf f) (DE ptx px) fs s2
    --         | tf >  tx = DE tx (f  Undef) :- init (DE tx x) s1 xs
    --       comb (DE ptf pf) (DE ptx px) s1@(DE tf f :- fs) s2@(DE tx x :- xs)
    --         | tf == tx = DE tf ( f  x) :- comb (DE  tf  f) (DE  tx  x) fs xs
    --         | tf <  tx = DE tf ( f px) :- comb (DE  tf  f) (DE ptx px) fs s2
    --         | tf >  tx = DE tx (pf  x) :- comb (DE ptf pf) (DE  tx  x) s1 xs
    --       comb _ (DE ptx px) (DE tf f :- fs) NullS
    --         = DE tf (f px) :- comb (DE tf f) (DE ptx px) fs NullS
    --       comb (DE ptf pf) _ NullS (DE tx x :- xs)
    --         = DE tx (pf x) :- comb (DE ptf pf) (DE tx x) NullS xs
    --       comb _ _ NullS NullS = NullS
