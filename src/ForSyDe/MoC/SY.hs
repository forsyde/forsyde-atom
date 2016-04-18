{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY
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

module ForSyDe.MoC.SY where

import ForSyDe.Core
import ForSyDe.Core.Utilities

-----------------------------------------------------------------------------
-- PRIMITIVE CONSTRUCTORS -- TIMED MOC TEMPLATE
-----------------------------------------------------------------------------

infixl 5 -$-, -*-
infixl 4 ->-

type SignalSY a = Signal (AbstExt a)

-- (-$-)  :: (AbstExt a -> b) -> Sig a -> Signal b
-- (-*-)  :: Signal (AbstExt a -> b) -> Sig a -> Signal b
-- (->-) :: AbstExt a -> Sig a -> Sig a
-----------------------------------------------------------------------------
(-$-)  = (<$>)
(-*-)  = (<*>)
(->-) = (:-)

infixl 3 -<, -<<, -<<<, -<<<<, -<<<<<, -<<<<<<, -<<<<<<<
(-<)       s = funzip2 s
(-<<)      s = funzip3 s
(-<<<)     s = funzip4 s
(-<<<<)    s = funzip5 s
(-<<<<<)   s = funzip6 s
(-<<<<<<)  s = funzip7 s
(-<<<<<<<) s = funzip8 s

-----------------------------------------------------------------------------
-- PROCESS CONSTRUCTORS / PATTERNS
-----------------------------------------------------------------------------
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
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy42 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy43 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy44 ns od i s1 s2 s3 s4 = (od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<<)
  where st                  = i ->- ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

{- 


-- FILTER, FILL, HOLD

-- | The process constructor 'filter' discards the values who do not fulfill a predicate given by a predicate function and replaces them with as5ent events.
filter :: (a -> Bool) -- Predicate function
         -> Sig a -- Input signal
         -> Sig (Arg a) -- Output signal

-- | The process constructor 'fill' creates a process that 'fills' a signal with present values by replacing as5ent values with a given value. The output signal is not any more of the type 'Arg'.
fill :: a -- ^Default value
       -> Sig (Arg a) -- ^As5ent extended input signal
       -> Sig a -- ^Output signal

-- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing as5ent values by the preceding present value. Only in s64es, where no preceding value exists, the as5ent value is replaced by a default value. The output signal is not any more of the type 'Arg'.
hold :: a -- ^Default value
       -> Sig (Arg a) -- ^As5ent extended input signal
       -> Sig a -- ^Output signa

filter p = (-$-) (\x -> if p x == True then Prst x else As5t)
fill   a = (-$-) (replaceAs5t a)
  where replaceAs5t a' As5t     = a'
        replaceAs5t _  (Prst x) = x
hold   a s1 = s
  where s = holdf -$- (s ->- a) -*- s1
        holdf a' As5t     = a'
        holdf _  (Prst x) = x

-}
