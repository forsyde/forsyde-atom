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
landingDelay  = Event 5 (D 0)


flight planes = (\dmuxf speed -> speed ->- filt (comb11 dmuxf planes) planes)
                 §> vector [isConcorde, isBoeing]
                <§> vector [concordeSpeed, boeingSpeed]


-- queue sc s1 = syr2de1 (0:-tags) (storefunc ctl inp)
--   where (tags, (ctl, inp)) = de2syr2 sc s1
--         storefunc   = mooreSyr21 ns id (D NullV)
--         ns bf c a  | (nullV <$> bf) == D True = bf >¤ a
--                    | otherwise = if c == D True then (tailV <$> bf) >¤ a else bf >¤ a


queue = phi wait 


  -- storefunc   = mooreSyr21 ns at21 (U, D NullV)
        -- ns (_, buf) c a | (nullV <$> buf) == D True = (U, buf >¤ a)
        --                 | otherwise = if c then (headV <$> buf ,(tailV <$> buf) >¤ a)

-- q1 = signal [Event 0 (D 2), Event 2 (D 1), Event 6 (D 3)] :: Signal (Event (UExt Int))
-- q2 = signal [Event 1 (D 2), Event 2 (D 1), Event 6 (D 3)] :: Signal (Event (UExt Int))
-- i1 = Event 1 (D 1)
-- i2 = Event 3 (D 1)
-- i3 = Event 0 (D 1)

-- osc1 = psi11 (+1) -$- (i1 ->- osc1)
-- osc2 = psi11 (+1) -$- (i2 ->- osc2)
-- osc3 = psi11 (+1) -$- (i3 ->- osc3)

-- cosc1 = psi21 (+) -$- (i1 ->- cosc1) -*- q1
-- cosc2 = psi21 (+) -$- (i2 ->- cosc2) -*- q1
-- cosc3 = psi21 (+) -$- (i1 ->- cosc3) -*- q2
-- cosc4 = psi21 (+) -$- (i2 ->- cosc4) -*- q2
-- cosc5 = psi21 (+) -$- (i3 ->- cosc5) -*- q2
                


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
control1  = signal [Event 4 (D True), Event 5 (D False), Event 10 (D True), Event 11 (D False)]



mooreSyr11 ns od i s1 = od <$> st
  where st          = i :- ns <$> st <*> s1

mooreSyr21 ns od i s1 s2 = od <$> st
  where st               = i :- ns <$> st <*> s1 <*> s2

mooreSyr41 ns od i s1 s2 s3 s4 = od <$> st
  where st                     = i :- ns <$> st <*> s1 <*> s2 <*> s3 <*> s4
