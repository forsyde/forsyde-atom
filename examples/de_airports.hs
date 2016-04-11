module Main where

import ForSyDe.Core
import ForSyDe.Core.Utilities
import ForSyDe.Core.UndefinedExt
import ForSyDe.MoC.DE


--   /===< JFK <===\
--   ||           ||
--   ||           ||
--   V             A
--  ORD >=======> LAX


-- airport plane1 plane2 = comb22 (\p1 p2->(p1 + p2, p1 - p2)) plane1 plane2 

-- jfk = airport pOrdJfk pLaxJfk
-- ord = airport pJfkOrd pLaxOrd
-- lax = airport pOrdLax pJfkLax

-- pJfkOrd = i1 ->- at21 jfk
-- pJfkLax = i2 ->- at22 jfk
-- pOrdJfk = i1 ->- at21 ord
-- pOrdLax = i3 ->- at22 ord
-- pLaxJfk = i1 ->- at21 lax
-- pLaxOrd = i2 ->- at22 lax

-- Concorde example
data Plane = Concorde | Boeing deriving (Eq, Show)

selPlane planes = comb12 dmuxf planes
  where
    dmuxf Concorde = (True, False)
    dmuxf Boeing   = (False, True)

-- selPlane planes = demux2 (comb21 dmuxf planes) planes
--   where
--     dmuxf Concorde = (True, False)
--     dmuxf Boeing   = (False, True)

-- concorde sched = (Event 1 (D U))  ->- fat21 . selPlane sched 
-- boeing   sched = (Event 10 (D U)) ->- fat22 . selPlane sched

-- planeSched1 = signal [Event 0 (D Boeing), Event 7 (D Concorde), Event 8 (D Boeing)]

-- scenario1 = mergePlane (concorde planeSched1) (boeing planeSched1)

-- deComb  = (+) -$- qi -*- q'
-- deOsc   = (+1) -$- (delay i deOsc)

-- deCOsc = (+) -$- q' -*- (delay i deCOsc)

