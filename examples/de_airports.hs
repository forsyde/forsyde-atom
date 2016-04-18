module Main where

import ForSyDe.Core
import ForSyDe.Core.Utilities
import ForSyDe.MoC.DE
import ForSyDe.PatternsTemp



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

isConcorde Concorde = True
isConcorde _        = False
isBoeing   Boeing   = True
isBoeing   _        = False

concordeSpeed = Event 1 U
boeingSpeed   = Event 10 U
landingDelay  = Event 2 (D NullV)


flight planes = (\dmuxf speed -> speed ->- filt (comb11 dmuxf planes) planes)
                 §> vector [isConcorde, isBoeing]
                <§> vector [concordeSpeed, boeingSpeed]

queue s1 = st
  where st = (Event 1 (D NullV)) ->- store st s1

q1 = signal [Event 0 (D 2), Event 2 (D 1), Event 6 (D 3)] :: Signal (Event (UExt Int))
q2 = signal [Event 1 (D 2), Event 2 (D 1), Event 6 (D 3)] :: Signal (Event (UExt Int))
i1 = Event 1 (D 1)
i2 = Event 3 (D 1)
i3 = Event 0 (D 1)

osc1 = psi11 (+1) -$- (i1 ->- osc1)
osc2 = psi11 (+1) -$- (i2 ->- osc2)
osc3 = psi11 (+1) -$- (i3 ->- osc3)

cosc1 = psi21 (+) -$- (i1 ->- cosc1) -*- q1
cosc2 = psi21 (+) -$- (i2 ->- cosc2) -*- q1
cosc3 = psi21 (+) -$- (i1 ->- cosc3) -*- q2
cosc4 = psi21 (+) -$- (i2 ->- cosc4) -*- q2
cosc5 = psi21 (+) -$- (i3 ->- cosc5) -*- q2
                


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

schedule1 = signal [Event 0 (D Boeing), Event 7 (D Concorde), Event 8 (D Boeing)]
