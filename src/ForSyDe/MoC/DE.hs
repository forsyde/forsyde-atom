{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.DE
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronuous library defines process constructors, processes and a signal conduit
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process. 
-----------------------------------------------------------------------------

module ForSyDe.MoC.DE where

import ForSyDe.Core
import ForSyDe.Core.Utilities
import ForSyDe.Core.AbsentExt

-----------------------------------------------------------------------------

data Ev a = Ev Int (UExt a) deriving (Show)

instance Functor Ev where
  fmap f (Ev t a) = Ev t (f <$> a)

instance Filtered Ev where
  c # (Ev t a) = if (c <$> a) == (D True) then Ev t a else Ev t U
          
-----------------------------------------------------------------------------

infixl 5 -$-
(-$-) :: (a -> b) -> Signal (Ev a) -> Signal (Ev b)
_ -$- NullS = NullS
f -$- (Ev t x:-xs) = Ev t (f <$> x) :- f -$- xs

infixl 5 -*-
(-*-) :: Signal (Ev (a -> b)) -> Signal (Ev a) -> Signal (Ev b)
sf -*- sx = zpw (Ev 0 U) (Ev 0 U) sf sx
  where zpw (Ev ptf pf) (Ev ptx px) s1@(Ev tf f :- fs) s2@(Ev tx x :- xs)
          | tf == tx = Ev tf ( f <*>  x) :- zpw (Ev  tf  f) (Ev  tx  x) fs xs
          | tf <  tx = Ev tf ( f <*> px) :- zpw (Ev  tf  f) (Ev ptx px) fs s2
          | tf >  tx = Ev tx (pf <*>  x) :- zpw (Ev ptf pf) (Ev  tx  x) s1 xs
        zpw _ (Ev ptx px) (Ev tf f :- fs) NullS
          = Ev tf (f <*> px) :- zpw (Ev tf f) (Ev ptx px) fs NullS
        zpw (Ev ptf pf) _ NullS (Ev tx x :- xs)
          = Ev tx (pf <*> x) :- zpw (Ev ptf pf) (Ev tx x) NullS xs
        zpw _ _ NullS NullS = NullS

infixl 5 ->-
(->-) :: Ev a -> Signal (Ev a) -> Signal (Ev a)
i@(Ev t v) ->- xs = (Ev 0 v) :- (\(Ev t1 g) -> Ev (t1 + t) g) <$> xs

infixl 3 -<, -<<, -<<<
(-<)    s = funzip2 (funzip2 <$> s)
(-<<)   s = funzip3 (funzip3 <$> s)
(-<<<)  s = funzip4 (funzip4 <$> s)

-----------------------------------------------------------------------------
-- ARGUMENT DATA TYPE - ABSENT EXTENDED
-----------------------------------------------------------------------------

data UExt a =  U | D a deriving (Eq)


instance Show a => Show (UExt a) where
  showsPrec _ = showsAE
    where showsAE U     = (++) "?"       
          showsAE (D x) = (++) (show x)

instance Read a => Read (UExt a) where
  readsPrec _       =  readsAE 
    where readsAE s = [(U, r1) | ("?", r1) <- lex s] ++ [(D x, r2) | (x, r2) <- reads s]

instance Functor UExt where
  fmap _ U     = U
  fmap f (D x) = D (f x)

instance Applicative UExt where
  pure  = D
  _      <*> U   = U
  U   <*> _      = U
  (D x) <*> (D y) = D (x y)


----------------------
---- CONSTRUCTORS ----
----------------------

comb11 f s1          = (f -$- s1)
comb12 f s1          = (f -$- s1 -<)
comb13 f s1          = (f -$- s1 -<<)
comb14 f s1          = (f -$- s1 -<<<)
comb21 f s1 s2       = (f -$- s1 -*- s2)
comb22 f s1 s2       = (f -$- s1 -*- s2 -<)
comb23 f s1 s2       = (f -$- s1 -*- s2 -<<)
comb24 f s1 s2       = (f -$- s1 -*- s2 -<<<)
comb31 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3)
comb32 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<)
comb33 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<<)
comb34 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<<<)
comb41 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4)
comb42 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<)
comb43 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
comb44 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)

delay s1 xs = s1 ->- xs

moore11 ns od i s1          = comb11 od st
  where st                  = i ->- comb21 ns st s1
moore12 ns od i s1          = comb12 od st
  where st                  = i ->- comb21 ns st s1
moore13 ns od i s1          = (od -$- st -<<)
  where st                  = i ->- comb21 ns st s1
moore14 ns od i s1          = (od -$- st -<<<)
  where st                  = i ->- comb21 ns st s1
moore21 ns od i s1 s2       = (od -$- st)
  where st                  = i ->- comb31 ns st s1 s2
moore22 ns od i s1 s2       = (od -$- st -<)
  where st                  = i ->- comb31 ns st s1 s2
moore23 ns od i s1 s2       = (od -$- st -<<)
  where st                  = i ->- comb31 ns st s1 s2
moore24 ns od i s1 s2       = (od -$- st -<<<)
  where st                  = i ->- comb31 ns st s1 s2
moore31 ns od i s1 s2 s3    = (od -$- st)
  where st                  = i ->- comb41 ns st s1 s2 s3
moore32 ns od i s1 s2 s3    = (od -$- st -<)
  where st                  = i ->- comb41 ns st s1 s2 s3
moore33 ns od i s1 s2 s3    = (od -$- st -<<)
  where st                  = i ->- comb41 ns st s1 s2 s3
moore34 ns od i s1 s2 s3    = (od -$- st -<<<)
  where st                  = i ->- comb41 ns st s1 s2 s3
moore41 ns od i s1 s2 s3 s4 = (od -$- st)
  where st                  = i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)
moore42 ns od i s1 s2 s3 s4 = (od -$- st -<)
  where st                  = i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)
moore43 ns od i s1 s2 s3 s4 = (od -$- st -<<)
  where st                  = i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)
moore44 ns od i s1 s2 s3 s4 = (od -$- st -<<<)
  where st                  = i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)

mealy11 ns od i s1          = comb21 od st s1
  where st                  = i ->- comb21 ns st s1
mealy12 ns od i s1          = comb22 od st s1
  where st                  = i ->- comb21 ns st s1
mealy13 ns od i s1          = comb23 od st s1
  where st                  = i ->- comb21 ns st s1
mealy14 ns od i s1          = comb24 od st s1
  where st                  = i ->- comb21 ns st s1
mealy21 ns od i s1 s2       = comb31 od st s1 s2
  where st                  = i ->- comb31 ns st s1 s2
mealy22 ns od i s1 s2       = comb32 od st s1 s2
  where st                  = i ->- comb31 ns st s1 s2
mealy23 ns od i s1 s2       = comb33 od st s1 s2
  where st                  = i ->- comb31 ns st s1 s2
mealy24 ns od i s1 s2       = comb34 od st s1 s2
  where st                  = i ->- comb31 ns st s1 s2
mealy31 ns od i s1 s2 s3    = comb41 od st s1 s2 s3
  where st                  = i ->- comb41 ns st s1 s2 s3
mealy32 ns od i s1 s2 s3    = comb42 od st s1 s2 s3
  where st                  = i ->- comb41 ns st s1 s2 s3
mealy33 ns od i s1 s2 s3    = comb43 od st s1 s2 s3
  where st                  = i ->- comb41 ns st s1 s2 s3
mealy34 ns od i s1 s2 s3    = comb44 od st s1 s2 s3
  where st                  = i ->- comb41 ns st s1 s2 s3
mealy41 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4)
  where st                  =  i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)
mealy42 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<)
  where st                  =  i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)
mealy43 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<)
  where st                  =  i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)
mealy44 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<<)
  where st                  =  i ->- (ns -$- st -*- s1 -*- s2 -*- s3 -*- s4)

------------------------------------------------------------------




-- funzip2 x = (fat21 x, fat22 x)
        
------------------------------------------------------------------

data State = V | F deriving (Eq, Show)

nsSplit F _ _ = F
nsSplit _ F _ = F
nsSplit _ _ _ = V

odSplit F _ _    = (U, U)
odSplit _ _ U = (U, U)
odSplit V _ (D b) | b `mod` 2 == 0 = (D b, U)
                | otherwise      = (U, D b)

pv (D b) | b >= 0    = D V
            | otherwise = D F
pv U = U

merge (D F) _ = F
merge _ (D F) = F
merge _ _        = V

--split = mealy22 nsSplit odSplit (Ev 2 V)

--so b = comb2 merge (comb pv $ s1 b) (comb pv $ s2 s3 s4 b)
--s1 b = fst $ split (sf b) b
--s2 b = snd $ split (sf b) b
--sf b = delay (Ev 15 V) $ so b

--a = signal [Ev 10 U, Ev (Tag 40) (D 4), Ev (Tag 60) (D 8), Ev Infty (D (-3))]
--dummy = signal [Ev Infty V]


w = signal [(2,2), (5,1), (12,3)]

--q' = signal [Ev (Tag 1) 1, Ev (Tag 2) 0.5, Ev Infty 1.5]
--q'' = signal [Ev (Tag 1) 1, Ev (Tag 2) 0.5, Ev (Tag 3) 0.5, Ev (Tag 4) 0.5, Ev (Tag 100) 1.5]


q1 = signal [Ev 0 (D 2), Ev 2 (D 1), Ev 6 (D 3)]
q2 = signal [Ev 1 (D 2), Ev 2 (D 1), Ev 6 (D 3)]
i1 = Ev 1 (D 1)
i2 = Ev 3 (D 1)
i3 = Ev 0 (D 1)

osc1 = (+1) -$- (i1 ->- osc1)
osc2 = (+1) -$- (i2 ->- osc2)
osc3 = (+1) -$- (i3 ->- osc3)

cosc1 = (+) -$- (i1 ->- cosc1) -*- q1
cosc2 = (+) -$- (i2 ->- cosc2) -*- q1
cosc3 = (+) -$- (i1 ->- cosc3) -*- q2
cosc4 = (+) -$- (i2 ->- cosc4) -*- q2
cosc5 = (+) -$- (i3 ->- cosc5) -*- q2



--   /===< JFK <===\
--   ||           ||
--   ||           ||
--   V             A
--  ORD >=======> LAX

airport plane1 plane2 = comb22 (\p1 p2->(p1 + p2, p1 - p2)) plane1 plane2 

jfk = airport pOrdJfk pLaxJfk
ord = airport pJfkOrd pLaxOrd
lax = airport pOrdLax pJfkLax

pJfkOrd = i1 ->- at21 jfk
pJfkLax = i2 ->- at22 jfk
pOrdJfk = i1 ->- at21 ord
pOrdLax = i3 ->- at22 ord
pLaxJfk = i1 ->- at21 lax
pLaxOrd = i2 ->- at22 lax

-- Concorde example
data Plane = Concorde | Boeing deriving (Eq, Show)

selPlane = comb12 selfunc
  where selfunc Concorde = (Prst Concorde, Abst)
        selfunc Boeing   = (Abst, Prst Boeing)

mergePlane = comb21 mux
  where mux (Prst Concorde) _ = Prst Concorde
        mux _ (Prst Boeing)   = Prst Boeing
        mux _ _               = Abst

concorde sched = (Ev 1 (D Abst)) ->- fat21 selPlane sched
boeing   sched = (Ev 10 (D Abst)) ->- fat22 selPlane sched

planeSched1 = signal [Ev 0 (D Boeing), Ev 7 (D Concorde), Ev 8 (D Boeing)]

scenario1 = mergePlane (concorde planeSched1) (boeing planeSched1)

-- deComb  = (+) -$- qi -*- q'
-- deOsc   = (+1) -$- (delay i deOsc)

-- deCOsc = (+) -$- q' -*- (delay i deCOsc)

