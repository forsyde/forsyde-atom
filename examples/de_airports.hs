        
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

--split = mealy22 nsSplit odSplit (Event 2 V)

--so b = comb2 merge (comb pv $ s1 b) (comb pv $ s2 s3 s4 b)
--s1 b = fst $ split (sf b) b
--s2 b = snd $ split (sf b) b
--sf b = delay (Event 15 V) $ so b

--a = signal [Event 10 U, Event (Tag 40) (D 4), Event (Tag 60) (D 8), Event Infty (D (-3))]
--dummy = signal [Event Infty V]


w = signal [(2,2), (5,1), (12,3)]

--q' = signal [Event (Tag 1) 1, Event (Tag 2) 0.5, Event Infty 1.5]
--q'' = signal [Event (Tag 1) 1, Event (Tag 2) 0.5, Event (Tag 3) 0.5, Event (Tag 4) 0.5, Event (Tag 100) 1.5]


q1 = signal [Event 0 (D 2), Event 2 (D 1), Event 6 (D 3)]
q2 = signal [Event 1 (D 2), Event 2 (D 1), Event 6 (D 3)]
i1 = Event 1 (D 1)
i2 = Event 3 (D 1)
i3 = Event 0 (D 1)

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

concorde sched = (Event 1 (D Abst)) ->- fat21 selPlane sched
boeing   sched = (Event 10 (D Abst)) ->- fat22 selPlane sched

planeSched1 = signal [Event 0 (D Boeing), Event 7 (D Concorde), Event 8 (D Boeing)]

scenario1 = mergePlane (concorde planeSched1) (boeing planeSched1)

-- deComb  = (+) -$- qi -*- q'
-- deOsc   = (+1) -$- (delay i deOsc)

-- deCOsc = (+) -$- q' -*- (delay i deCOsc)

