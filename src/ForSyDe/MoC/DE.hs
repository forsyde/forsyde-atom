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

data Tok a = Tok Int (AbstExt a) deriving (Show)

instance Functor Tok where
  fmap f (Tok t a) = Tok t (f <$> a)
          
-----------------------------------------------------------------------------

infixl 5 -§
(-§) :: (Eq a) => (a -> b) -> Signal (Tok a) -> Signal (Tok b)
_ -§ NullS = NullS
f -§ (Tok t x:-xs) = Tok t (f <$> x) :- f -§ xs

infixl 5 §§
(§§) :: (Eq a) => Signal (Tok (a -> b)) -> Signal (Tok a) -> Signal (Tok b)
sf §§ sx = zpw (Tok 0 Abst) (Tok 0 Abst) sf sx
  where zpw (Tok ptf pf) (Tok ptx px) s1@(Tok tf f :- fs) s2@(Tok tx x :- xs)
          | tf == tx = Tok tf ( f <*>  x) :- zpw (Tok  tf  f) (Tok  tx  x) fs xs
          | tf <  tx = Tok tf ( f <*> px) :- zpw (Tok  tf  f) (Tok ptx px) fs s2
          | tf >  tx = Tok tx (pf <*>  x) :- zpw (Tok ptf pf) (Tok  tx  x) s1 xs
        zpw _ (Tok ptx px) (Tok tf f :- fs) NullS
          = Tok tf (f <*> px) :- zpw (Tok tf f) (Tok ptx px) fs NullS
        zpw (Tok ptf pf) _ NullS (Tok tx x :- xs)
          = Tok tx (pf <*> x) :- zpw (Tok ptf pf) (Tok tx x) NullS xs
        zpw _ _ NullS NullS = NullS



infixl 5 ->-
(->-) :: Tok a -> Signal (Tok a) -> Signal (Tok a)
i@(Tok t v) ->- xs = (Tok 0 v) :- (\(Tok t1 g) -> Tok (t1 + t) g) <$> xs

-----------------------------------------------------------------------------
-- ARGUMENT DATA TYPE - ABSENT EXTENDED
-----------------------------------------------------------------------------

data AbstExt a =  Abst | Prst a deriving (Eq)


instance Show a => Show (AbstExt a) where
  showsPrec _ x = showsAE x
    where showsAE Abst     = (++) "_"       
          showsAE (Prst x) = (++) (show x)

instance Read a => Read (AbstExt a) where
  readsPrec _ x       =  readsAE x 
    where readsAE s = [(Abst, r1) | ("_", r1) <- lex s] ++ [(Prst x, r2) | (x, r2) <- reads s]

instance Functor AbstExt where
  fmap _ Abst     = Abst
  fmap f (Prst x) = Prst (f x)

instance Applicative AbstExt where
  pure a  = Prst a
  _      <*> Abst   = Abst
  Abst   <*> _      = Abst
  (Prst x) <*> (Prst y) = Prst (x y)

----------------------
---- CONSTRUCTORS ----
----------------------

{--- | The `comb` take a combinatorial function as argument and returns a process with one input signals and one output signal.
comb :: (a -> b) -- ^ combinatorial function
       -> Signal (Subsig a) -- ^ input signal
       -> Signal (Subsig b) -- ^ output signal

-- | Behaves like 'comb', but the process takes 2 input signals.
comb2 :: (a -> b -> c) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c)

-- | Behaves like 'comb', but the process takes 3 input signals.
comb3 :: (a -> b -> c -> d) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d)

-- | Behaves like 'comb', but the process takes 4 input signals.
comb4 :: (a -> b -> c -> d -> e) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d) -> Signal (Subsig e)-}

-- comb  f x       = (f -§ x §-)
-- comb2 f x y     = (f -§ x §§ y §-)
-- comb3 f x y z   = (f -§ x §§ y §§ z §-)
-- comb4 f x y z q = (f -§ x §§ y §§ z §§ q §-)


-- DELAY

-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delay :: (Tok a) -- ^Initial state
        -> Signal (Tok a) -- ^Input signal
        -> Signal (Tok a) -- ^Output signal

delay x xs = x ->- xs

---------------------------------------------------------



--comb12 :: (a -> (b,b)) -> Signal(Tok a) -> (Signal (Tok b), Signal(Tok b))
--comb12 f x = uzf2 (unzip2 <$> (f -§ x)) (¤)

-- comb12 f x = ((fat21 funzip2<$>s §-),(fat22 funzip2<$>s §-))
--   where s = f -§ x

-- comb22 f x y = ((fat21 funzip2<$>s ),(fat22 funzip2<$>s ))
--   where s = f -§ x §§ y

-- comb32 f x y z = ((fat21 funzip2<$>s ),(fat22 funzip2<$>s ))
--   where s = f -§ x §§ y §§ z

-- mealy ns od mem s1 = comb2 od s s1
--   where s = (comb2 ns s s1) ->- mem

-- mealy' ns od mem s1 = (od -§ s §§ s1 §-)
--   where s = (ns -§ s §§ s1 §-) ->- mem

-- mealy22 ns od mem s1 s2 = comb32 od s s1 s2
--   where s = (comb3 ns s s1 s2) ->- mem

------------------------------------------------------------------

data State = V | F deriving (Eq, Show)

nsSplit F _ _ = F
nsSplit _ F _ = F
nsSplit _ _ _ = V

odSplit F _ _    = (Abst, Abst)
odSplit _ _ Abst = (Abst, Abst)
odSplit V _ (Prst b) | b `mod` 2 == 0 = (Prst b, Abst)
                | otherwise      = (Abst, Prst b)

pv (Prst b) | b >= 0    = Prst V
            | otherwise = Prst F
pv Abst = Abst

merge (Prst F) _ = F
merge _ (Prst F) = F
merge _ _        = V

--split = mealy22 nsSplit odSplit (Tok 2 V)

--so b = comb2 merge (comb pv $ s1 b) (comb pv $ s2 b)
--s1 b = fst $ split (sf b) b
--s2 b = snd $ split (sf b) b
--sf b = delay (Tok 15 V) $ so b

--a = signal [Tok 10 Abst, Tok (Tag 40) (Prst 4), Tok (Tag 60) (Prst 8), Tok Infty (Prst (-3))]
--dummy = signal [Tok Infty V]


w = signal [(2,2), (5,1), (12,3)]

--q' = signal [Tok (Tag 1) 1, Tok (Tag 2) 0.5, Tok Infty 1.5]
--q'' = signal [Tok (Tag 1) 1, Tok (Tag 2) 0.5, Tok (Tag 3) 0.5, Tok (Tag 4) 0.5, Tok (Tag 100) 1.5]


q1 = signal [Tok 0 (Prst 2), Tok 2 (Prst 1), Tok 6 (Prst 3)]
q2 = signal [Tok 1 (Prst 2), Tok 2 (Prst 1), Tok 6 (Prst 3)]
i1 = Tok 1 (Prst 1)
i2 = Tok 3 (Prst 1)
i3 = Tok 0 (Prst 1)

osc1 = (+1) -§ (i1 ->- osc1)
osc2 = (+1) -§ (i2 ->- osc2)
osc3 = (+1) -§ (i3 ->- osc3)

cosc1 = (+) -§ (i1 ->- cosc1) §§ q1
cosc2 = (+) -§ (i2 ->- cosc2) §§ q1
cosc3 = (+) -§ (i1 ->- cosc3) §§ q2
cosc4 = (+) -§ (i2 ->- cosc4) §§ q2
cosc5 = (+) -§ (i3 ->- cosc5) §§ q2


comb21 f x y = f -§ x §§ y

comb12 f x   = ((fat21 funzip2<$>s ),(fat22 funzip2<$>s ))
  where s = f -§ x
        
comb22 f x y = ((fat21 funzip2<$>s ),(fat22 funzip2<$>s ))
  where s = f -§ x §§ y

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

concorde sched = (Tok 1 (Prst Abst)) ->- fat21 selPlane sched
boeing   sched = (Tok 10 (Prst Abst)) ->- fat22 selPlane sched

planeSched1 = signal [Tok 0 (Prst Boeing), Tok 7 (Prst Concorde), Tok 8 (Prst Boeing)]

scenario1 = mergePlane (concorde planeSched1) (boeing planeSched1)

-- deComb  = (+) -§ qi §§ q'
-- deOsc   = (+1) -§ (delay i deOsc)

-- deCOsc = (+) -§ q' §§ (delay i deCOsc)

