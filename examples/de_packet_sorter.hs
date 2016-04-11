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


