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

-----------------------------------------------------------------------------

data Event a = Event Int a deriving (Show)

instance Functor Event where
  fmap f (Event t a) = Event t (f a)

-----------------------------------------------------------------------------

infixl 5 -$-
(-$-) :: (UExt a -> b) -> Signal (Event (UExt a)) -> Signal (Event  b)
_ -$- NullS = NullS
f -$- (x:-xs) = fmap f x :- f -$- xs

infixl 5 -*-
(-*-) :: Signal (Event (UExt a -> b)) -> Signal (Event (UExt a)) -> Signal (Event b)
sf -*- sx = zpwh (Event 0 U) sf sx
  where zpwh (Event ptx px) s1@(Event tf f :- fs) s2@(Event tx x :- xs)
          | tf == tx = Event tf (f  x) :- zpwt (Event  tf  f) (Event  tx  x) fs xs
          | tf <  tx = Event tf (f px) :- zpwt (Event  tf  f) (Event ptx px) fs s2
          | tf >  tx = Event tx (f  U) :- zpwh (Event  tx  x) s1 xs
        -- zpwh pxe (Event tf f :- fs) NullS = Event tf (f U) :- zpwh pxe fs NullS
        -- zpwh pxe NullS (Event tx x :- xs) = Event tx (id U) :- zpwh pxe NullS xs
        zpwh _ NullS _ = NullS
        zpwh _ _ NullS = NullS    
        zpwt (Event ptf pf) (Event ptx px) s1@(Event tf f :- fs) s2@(Event tx x :- xs)
          | tf == tx = Event tf ( f  x) :- zpwt (Event  tf  f) (Event  tx  x) fs xs
          | tf <  tx = Event tf ( f px) :- zpwt (Event  tf  f) (Event ptx px) fs s2
          | tf >  tx = Event tx (pf  x) :- zpwt (Event ptf pf) (Event  tx  x) s1 xs
        zpwt _ (Event ptx px) (Event tf f :- fs) NullS
          = Event tf (f px) :- zpwt (Event tf f) (Event ptx px) fs NullS
        zpwt (Event ptf pf) _ NullS (Event tx x :- xs)
          = Event tx (pf x) :- zpwt (Event ptf pf) (Event tx x) NullS xs
        zpwt _ _ NullS NullS = NullS

infixl 4 ->-
(->-) :: Event a -> Signal (Event a) -> Signal (Event a)
(Event t v) ->- xs = (Event 0 v) :- (\(Event t1 g) -> Event (t1 + t) g) <$> xs

infixl 3 -<, -<<, -<<<, -<<<<, -<<<<<, -<<<<<<, -<<<<<<<
(-<)       s = funzip2 (funzip2 <$> s)
(-<<)      s = funzip3 (funzip3 <$> s)
(-<<<)     s = funzip4 (funzip4 <$> s)
(-<<<<)    s = funzip5 (funzip5 <$> s)
(-<<<<<)   s = funzip6 (funzip6 <$> s)
(-<<<<<<)  s = funzip7 (funzip7 <$> s)
(-<<<<<<<) s = funzip8 (funzip8 <$> s)

infixl 4 -|>-
(-|>-) :: Event a -> Signal (Event a) -> Signal (Event a)
_ -|>- NullS = NullS 
(Event t _) -|>- xs = queue t 0 xs
  where queue dt pt (Event tx x :-  xs) | pt < tx   = Event tx x :- queue dt tx xs
                                        | otherwise = Event (pt + dt) x :- queue dt (pt + dt) xs

-- infixl 3 #
-- (#) :: Signal (Event Bool) -> Signal (Event (a -> a)) 
-- (#) s = fmap (\(Event t x) -> if x == D True then Event t (D id) else Event t U) s

-----------------------------------------------------------------------------

----------------------
---- CONSTRUCTORS ----
----------------------

comb11 f s1          = (psi11 f -$- s1)
comb12 f s1          = (psi12 f -$- s1 -<)
comb13 f s1          = (psi13 f -$- s1 -<<)
comb14 f s1          = (psi14 f -$- s1 -<<<)
comb21 f s1 s2       = (psi21 f -$- s1 -*- s2)
comb22 f s1 s2       = (psi22 f -$- s1 -*- s2 -<)
comb23 f s1 s2       = (psi23 f -$- s1 -*- s2 -<<)
comb24 f s1 s2       = (psi24 f -$- s1 -*- s2 -<<<)
comb31 f s1 s2 s3    = (psi31 f -$- s1 -*- s2 -*- s3)
comb32 f s1 s2 s3    = (psi32 f -$- s1 -*- s2 -*- s3 -<)
comb33 f s1 s2 s3    = (psi33 f -$- s1 -*- s2 -*- s3 -<<)
comb34 f s1 s2 s3    = (psi34 f -$- s1 -*- s2 -*- s3 -<<<)
comb41 f s1 s2 s3 s4 = (psi41 f -$- s1 -*- s2 -*- s3 -*- s4)
comb42 f s1 s2 s3 s4 = (psi42 f -$- s1 -*- s2 -*- s3 -*- s4 -<)
comb43 f s1 s2 s3 s4 = (psi43 f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
comb44 f s1 s2 s3 s4 = (psi44 f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)

delay s1 xs = s1 ->- xs

moore11 ns od i s1          = comb11 od st
  where st                  = i ->- comb21 ns st s1
moore12 ns od i s1          = comb12 od st
  where st                  = i ->- comb21 ns st s1
moore13 ns od i s1          = comb13 od st
  where st                  = i ->- comb21 ns st s1
moore14 ns od i s1          = comb14 od st
  where st                  = i ->- comb21 ns st s1
moore21 ns od i s1 s2       = comb11 od st
  where st                  = i ->- comb31 ns st s1 s2
moore22 ns od i s1 s2       = comb12 od st
  where st                  = i ->- comb31 ns st s1 s2
moore23 ns od i s1 s2       = comb13 od st
  where st                  = i ->- comb31 ns st s1 s2
moore24 ns od i s1 s2       = comb14 od st
  where st                  = i ->- comb31 ns st s1 s2
moore31 ns od i s1 s2 s3    = comb11 od st
  where st                  = i ->- comb41 ns st s1 s2 s3
moore32 ns od i s1 s2 s3    = comb12 od st
  where st                  = i ->- comb41 ns st s1 s2 s3
moore33 ns od i s1 s2 s3    = comb13 od st
  where st                  = i ->- comb41 ns st s1 s2 s3
moore34 ns od i s1 s2 s3    = comb14 od st
  where st                  = i ->- comb41 ns st s1 s2 s3
moore41 ns od i s1 s2 s3 s4 = comb11 od st
  where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore42 ns od i s1 s2 s3 s4 = comb12 od st
  where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore43 ns od i s1 s2 s3 s4 = comb13 od st
  where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore44 ns od i s1 s2 s3 s4 = comb14 od st
  where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

mealy11 ns od i s1          = comb21 od st s1
  where st            = i ->- comb21 ns st s1
mealy12 ns od i s1          = comb22 od st s1
  where st            = i ->- comb21 ns st s1
mealy13 ns od i s1          = comb23 od st s1
  where st            = i ->- comb21 ns st s1
mealy14 ns od i s1          = comb24 od st s1
  where st            = i ->- comb21 ns st s1
mealy21 ns od i s1 s2       = comb31 od st s1 s2
  where st            = i ->- comb31 ns st s1 s2
mealy22 ns od i s1 s2       = comb32 od st s1 s2
  where st            = i ->- comb31 ns st s1 s2
mealy23 ns od i s1 s2       = comb33 od st s1 s2
  where st            = i ->- comb31 ns st s1 s2
mealy24 ns od i s1 s2       = comb34 od st s1 s2
  where st            = i ->- comb31 ns st s1 s2
mealy31 ns od i s1 s2 s3    = comb41 od st s1 s2 s3
  where st            = i ->- comb41 ns st s1 s2 s3
mealy32 ns od i s1 s2 s3    = comb42 od st s1 s2 s3
  where st            = i ->- comb41 ns st s1 s2 s3
mealy33 ns od i s1 s2 s3    = comb43 od st s1 s2 s3
  where st            = i ->- comb41 ns st s1 s2 s3
mealy34 ns od i s1 s2 s3    = comb44 od st s1 s2 s3
  where st            = i ->- comb41 ns st s1 s2 s3
mealy41 ns od i s1 s2 s3 s4 = (psi51 od -$- st -*- s1 -*- s2 -*- s3 -*- s4)
  where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy42 ns od i s1 s2 s3 s4 = (psi52 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<)
  where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy43 ns od i s1 s2 s3 s4 = (psi53 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<)
  where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy44 ns od i s1 s2 s3 s4 = (psi54 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<<)
  where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

filt sels s = (#) -$- sels -*- s
