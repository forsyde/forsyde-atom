{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.CT
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

module ForSyDe.MoC.CT where

import ForSyDe.Core
import ForSyDe.Core.Utilities
import ForSyDe.Core.UndefinedExt

plot :: (Num a, Show a) => Time -> SignalCT a -> Signal (Time, UExt a)
plot step (Event t f :- fs) = plot' t f fs 
  where plot' ct cf NullS = (ct, cf ct) :- plot' (ct + step) cf NullS
        plot' ct cf (Event t f :- fs)
          | ct < t    = (ct, cf ct) :- plot' (ct + step) cf (Event t f :- fs)
          | otherwise = ( t,  f  t) :- plot' ( t + step)  f fs

-----------------------------------------------------------------------------

type Time = Rational
type SignalCT a = Signal (Event a)

data Event a = Event Time (Time -> UExt a)

instance Functor Event where
  fmap f (Event t a) = Event t (fmap f <$> a)

-- instance Applicative Event where
--   pure a = Event 0.0 (pure a)
--   (D x) <*> (D y) = D (x y)
  
--   fmap f (Event t a) = Event t (fmap f <$> a)

-- instance Filter Event where
--   c # (Event t a) = if (c <$> a) == True
--                     then Event t a
--                     else Event t (\x -> U)
          
-----------------------------------------------------------------------------

infixl 5 -$-
(-$-) :: (a -> b) -> Signal (Event a) -> Signal (Event b)
_ -$- NullS = NullS
f -$- (e :- es) = f <$> e  :- f -$- es

infixl 5 -*-
(-*-) :: Signal (Event (a -> b)) -> Signal (Event a) -> Signal (Event b)
sf -*- sx = zpw (Event 0.0 (\x -> U)) (Event 0.0 (\x -> U)) sf sx
  where zpw (Event pt1 px1) (Event pt2 px2) s1@(Event t1 x1 :- xs1) s2@(Event t2 x2 :- xs2)
          | t1 == t2 = Event t1 (\a -> (x1 a) <*> (x2 a))  :- zpw (Event t1 x1) (Event  t2 x2)  xs1 xs2
          | t1 <  t2 = Event t1 (\a -> (x1 a) <*> (px2 a)) :- zpw (Event t1 x1) (Event pt2 px2) xs1 s2
          | t1 >  t2 = Event t2 (\a -> (px1 a) <*> (x2 a)) :- zpw (Event pt1 px1) (Event t2 x2) s1  xs2
        zpw _ (Event ptx px) (Event tf f :- fs) NullS
          = Event t1 (\a -> (x1 a) <*> (px2 a)) :- zpw (Event tf f) (Event ptx px) fs NullS
        zpw (Event ptf pf) _ NullS (Event tx x :- xs)
          = Event t2 (\a -> (px1 a) <*> x) :- zpw (Event ptf pf) (Event tx x) NullS xs
        zpw _ _ NullS NullS = NullS

-- infixl 5 -§-
-- (-§-) :: Signal (Subsig (a -> b)) -> Signal (Subsig a) -> Signal (Subsig b)
-- NullS -§- _     = NullS
-- _     -§- NullS = NullS
-- s1@(Subsig (t1, g) :- gs) -§- s2@(Subsig (t2, f) :- fs) 
--     | t1 == t2 = Subsig (t1, \x -> (g x) (f x)) :- (gs -§- fs)
--     | t1 <  t2 = Subsig (t1, \x -> (g x) (f x)) :- (gs -§- s2)
--     | t1 >  t2 = Subsig (t2, \x -> (g x) (f x)) :- (s1 -§- fs)


-- infixl 5 ->-
-- (->-) :: Signal (Subsig a) -> Subsig a -> Signal (Subsig a)
-- xs ->- i@(Subsig (t, _)) = i :- (\(Subsig (t1, g)) -> Subsig (t1+t, g)) <$> xs

-- infixl 5 -*-
-- (-*-) :: Signal (Event (a -> b)) -> Signal (Event a) -> Signal (Event b)
-- sf -*- sx = zpw (Event 0 U) (Event 0 U) sf sx
--   where zpw (Event ptf pf) (Event ptx px) s1@(Event tf f :- fs) s2@(Event tx x :- xs)
--           | tf == tx = Event tf ( f <*>  x) :- zpw (Event  tf  f) (Event  tx  x) fs xs
--           | tf <  tx = Event tf ( f <*> px) :- zpw (Event  tf  f) (Event ptx px) fs s2
--           | tf >  tx = Event tx (pf <*>  x) :- zpw (Event ptf pf) (Event  tx  x) s1 xs
--         zpw _ (Event ptx px) (Event tf f :- fs) NullS
--           = Event tf (f <*> px) :- zpw (Event tf f) (Event ptx px) fs NullS
--         zpw (Event ptf pf) _ NullS (Event tx x :- xs)
--           = Event tx (pf <*> x) :- zpw (Event ptf pf) (Event tx x) NullS xs
--         zpw _ _ NullS NullS = NullS

-- infixl 4 ->-
-- (->-) :: Event a -> Signal (Event a) -> Signal (Event a)
-- i@(Event t v) ->- xs = (Event 0 v) :- (\(Event t1 g) -> Event (t1 + t) g) <$> xs

-- infixl 3 -<, -<<, -<<<, -<<<<, -<<<<<, -<<<<<<, -<<<<<<<
-- (-<)       s = funzip2 (funzip2 <$> s)
-- (-<<)      s = funzip3 (funzip3 <$> s)
-- (-<<<)     s = funzip4 (funzip4 <$> s)
-- (-<<<<)    s = funzip5 (funzip5 <$> s)
-- (-<<<<<)   s = funzip6 (funzip6 <$> s)
-- (-<<<<<<)  s = funzip7 (funzip7 <$> s)
-- (-<<<<<<<) s = funzip8 (funzip8 <$> s)

-- -----------------------------------------------------------------------------


-- ----------------------
-- ---- CONSTRUCTORS ----
-- ----------------------

-- -- | The `comb` take a combinatorial function as argument and returns a process with one input signals and one output signal.
-- comb :: (a -> b) -- ^ combinatorial function
--        -> Signal (Subsig a) -- ^ input signal
--        -> Signal (Subsig b) -- ^ output signal

-- -- | Behaves like 'comb', but the process takes 2 input signals.
-- comb2 :: (a -> b -> c) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c)

-- -- | Behaves like 'comb', but the process takes 3 input signals.
-- comb3 :: (a -> b -> c -> d) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d)

-- -- | Behaves like 'comb', but the process takes 4 input signals.
-- comb4 :: (a -> b -> c -> d -> e) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d) -> Signal (Subsig e)

-- comb  f x       = f §- x
-- comb2 f x y     = f §- x -§- y
-- comb3 f x y z   = f §- x -§- y -§- z
-- comb4 f x y z q = f §- x -§- y -§- z -§- q


-- -- DELAY

-- -- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
-- delay :: (Subsig a) -- ^Initial state
--         -> Signal (Subsig a) -- ^Input signal
--         -> Signal (Subsig a) -- ^Output signal

-- delay x xs = xs ->- x

