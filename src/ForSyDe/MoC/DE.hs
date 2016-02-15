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

data Tok a = Tok Tag (AbstExt a)       deriving (Show)

data Tag   = Tag Int | Infty deriving (Eq, Ord)

instance Show Tag where
  showsPrec _ x = showsTag x
    where showsTag Infty   = (++) "\8734"       
          showsTag (Tag x) = (++) (show x)

instance Num Tag where
  Infty + _ = Infty
  _ + Infty = Infty
  (Tag a) + (Tag b) = Tag (a + b)
  -------------------
  Infty * _ = Infty
  _ * Infty = Infty
  (Tag a) * (Tag b) = Tag (a * b)
  -------------------
  abs Infty = Infty
  abs (Tag a) = Tag a
  -------------------
  signum (Tag a) = Tag (signum a)
  signum Infty   = Tag 1
  -------------------
  fromInteger a = Tag (fromIntegral a)
  -------------------
  negate Infty = -1 * Infty
  negate (Tag a) = Tag (negate a)

  
tag (Tok a _) = a
val (Tok _ b) = b



instance Functor Tok where
  fmap f (Tok t a) = Tok t (f <$> a)
          
-----------------------------------------------------------------------------
{-
infixl 5 §-
(§-) :: (a -> b) -> Signal (Tok a) -> Signal (Tok b)
_ §- NullS           = NullS
f §- (Tok t x :- xs) = Tok t (f x) :- f §- xs
f §- (Tok t x :- xs) = clean (Tok t (f x)) (f §- xs)
  where clean   (Tok t v) NullS = Tok t v :- NullS 
        clean e@(Tok t v) es@(Tok t1 v1 :- vs)
          | t >  t1   = error "Tags not ordered"
          | v == v1   = es
          | otherwise = e :- es
  
infixl 5 -§-
(-§-) :: Signal (Tok (a -> b)) -> Signal (Tok a) -> Signal (Tok b)
NullS -§- _     = NullS
_     -§- NullS = NullS
s1@(Tok t1 f :- fs) -§- s2@(Tok t2 x :- xs)
  | t1 == t2 = Tok t1 (f x) :- (fs -§- xs)
  | t1 <  t2 = Tok t1 (f x) :- (fs -§- s2)
  | t1 >  t2 = Tok t2 (f x) :- (s1 -§- xs)
-}

infixl 5 -§
(-§) :: (Eq a) => (a -> b) -> Signal (Tok a) -> Signal (Tok b)
_ -§ NullS = NullS
f -§ s@(Tok t x:-xs) = Tok t (f <$> x) :- map' t x f xs
  where map' _  _  _ NullS = NullS
        map' pt px f (Tok t x:-xs)
          | pt >  t   = error "Tags not ordered"
          | px == x   = map' pt px f xs
          | otherwise = Tok t (f <$> x) :- map' t x f xs

        
infixl 5 §§
(§§) :: (Eq a) => Signal (Tok (a -> b)) -> Signal (Tok a) -> Signal (Tok b)
_ §§ NullS  = NullS
NullS §§ _  = NullS
sf §§ sx@(Tok _ x:-_) = advance x sf sx
  where advance i s1@(Tok t1 f :- fs) s2@(Tok t2 x :- xs)
          | t1 == t2 = Tok t1 (f <*> x) :- zpw' t1 i fs xs
          | t1 <  t2 = Tok t1 (f <*> x) :- zpw' t1 i fs s2
          | t1 >  t2 = Tok t2 (f <*> x) :- zpw' t2 i s1 xs
        zpw' _  _  NullS _ = NullS
        zpw' _  _  _ NullS = NullS
        zpw' pt px sf@(Tok tf f:-fs) sx@(Tok tx x:-xs)
          | pt >  tx  = error "Tags not ordered"                         
          | px == x   = zpw' pt px sf xs
          | otherwise = advance x sf sx

infixl 3 §-
(§-) :: Eq a => Signal (Tok a) -> Signal (Tok a)
(§-) NullS = NullS
(§-) (Tok t x:-xs)  =  Tok t x :- clean t x xs 
  where clean _  _  NullS = NullS
        clean pt px (Tok t x :- xs)
          | pt >  t   = error "Tags not ordered"
          | px == x   = clean pt px xs
          | otherwise = Tok t x :- clean t x xs
           
infixl 5 ->-
(->-) :: Signal (Tok a) -> Tok a -> Signal (Tok a)
xs ->- i@(Tok t v) = (Tok 0 v) :- (\(Tok t1 g) -> Tok (t1 + t) g) <$> xs

sig2de :: Signal (Int, a) -> Signal (Tok a)
sig2de NullS = NullS
sig2de s@((t, x):-xs) | t == 0    = period (Prst x) xs
                      | otherwise = period Abst     s
  where period px NullS        = Tok Infty px :- NullS
        period px ((t, x):-xs) = Tok (Tag t) px :- period (Prst x) xs
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

comb  f x       = (f -§ x §-)
comb2 f x y     = (f -§ x §§ y §-)
comb3 f x y z   = (f -§ x §§ y §§ z §-)
comb4 f x y z q = (f -§ x §§ y §§ z §§ q §-)


-- DELAY

-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delay :: (Tok a) -- ^Initial state
        -> Signal (Tok a) -- ^Input signal
        -> Signal (Tok a) -- ^Output signal

delay x xs = xs ->- x

---------------------------------------------------------



--comb12 :: (a -> (b,b)) -> Signal(Tok a) -> (Signal (Tok b), Signal(Tok b))
--comb12 f x = uzf2 (unzip2 <$> (f -§ x)) (¤)

comb12 f x = ((fat21 funzip2<$>s §-),(fat22 funzip2<$>s §-))
  where s = f -§ x

comb22 f x y = ((fat21 funzip2<$>s ),(fat22 funzip2<$>s ))
  where s = f -§ x §§ y

comb32 f x y z = ((fat21 funzip2<$>s ),(fat22 funzip2<$>s ))
  where s = f -§ x §§ y §§ z

mealy ns od mem s1 = comb2 od s s1
  where s = (comb2 ns s s1) ->- mem

mealy' ns od mem s1 = (od -§ s §§ s1 §-)
  where s = (ns -§ s §§ s1 §-) ->- mem

mealy22 ns od mem s1 s2 = comb32 od s s1 s2
  where s = (comb3 ns s s1 s2) ->- mem

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

q = signal [(3,2), (5,3), (20, 4)]
q' = signal [Tok 1 (Prst 2), Tok 2 (Prst 1), Tok 3 (Prst 1), Tok 4 (Prst 1), Tok 5 (Prst 1), Tok 20 (Prst 3)]
w = signal [(2,2), (5,1), (12,3)]

--q' = signal [Tok (Tag 1) 1, Tok (Tag 2) 0.5, Tok Infty 1.5]
--q'' = signal [Tok (Tag 1) 1, Tok (Tag 2) 0.5, Tok (Tag 3) 0.5, Tok (Tag 4) 0.5, Tok (Tag 100) 1.5]

i = Tok 1 (Prst 1)
qi = delay i $ sig2de q

deComb  = comb2 (+) qi $ sig2de q
deOsc   = comb (+1) (delay i deOsc)

