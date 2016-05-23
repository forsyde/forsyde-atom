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
        zpw _ (Event pt2 px2) (Event t1 x1 :- xs1) NullS
          = Event t1 (\a -> (x1 a) <*> (px2 a)) :- zpw (Event t1 x1) (Event pt2 px2) xs1 NullS
        zpw (Event pt1 px1) _ NullS (Event t2 x2 :- xs2)
          = Event t2 (\a -> (px1 a) <*> (x2 a)) :- zpw (Event pt1 px1) (Event t2 x2) NullS xs2
        zpw _ _ NullS NullS = NullS

infixl 4 ->-
(->-) :: Event a -> Signal (Event a) -> Signal (Event a)
i@(Event t v) ->- xs = (Event 0 v) :- (\(Event t1 g) -> Event (t1 + t) g) <$> xs

infixl 3 -<, -<<, -<<<, -<<<<, -<<<<<, -<<<<<<, -<<<<<<<
(-<)       s = funzip2 (funzip2 <$> s)
(-<<)      s = funzip3 (funzip3 <$> s)
(-<<<)     s = funzip4 (funzip4 <$> s)
(-<<<<)    s = funzip5 (funzip5 <$> s)
(-<<<<<)   s = funzip6 (funzip6 <$> s)
(-<<<<<<)  s = funzip7 (funzip7 <$> s)
(-<<<<<<<) s = funzip8 (funzip8 <$> s)

-----------------------------------------------------------------------------


----------------------
---- CONSTRUCTORS ----
----------------------

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

-- -- DELAY

-- -- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
-- delay :: (Subsig a) -- ^Initial state
--         -> Signal (Subsig a) -- ^Input signal
--         -> Signal (Subsig a) -- ^Output signal


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
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore42 ns od i s1 s2 s3 s4 = (od -$- st -<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore43 ns od i s1 s2 s3 s4 = (od -$- st -<<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore44 ns od i s1 s2 s3 s4 = (od -$- st -<<<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

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
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy42 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy43 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy44 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
