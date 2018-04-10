module ForSyDe.Atom.MoC.SDF.SADF where

import qualified ForSyDe.Atom.MoC.SAXX as SA
import ForSyDe.Atom.MoC.SDF.Core
import ForSyDe.Atom.MoC

smap = fmap . fmap

kernel11 :: Signal (Cons, Prod, [a1] -> [b1]) 
         -> Signal a1
         -> Signal b1
kernel21 :: Signal ((Cons, Cons), Prod, [a1] -> [a2] -> [b1])
         -> Signal a1 -> Signal a2
         -> Signal b1
kernel31 :: Signal ((Cons, Cons, Cons), Prod,
                    [a1] -> [a2] -> [a3] -> [b1])
         -> Signal a1 -> Signal a2 -> Signal a3
         -> Signal b1
kernel41 :: Signal ((Cons, Cons, Cons, Cons), Prod,
                    [a1] -> [a2] -> [a3] -> [a4] -> [b1])
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> Signal b1
kernel12 :: Signal (Cons, (Prod, Prod), [a1] -> ([b1], [b2]))
         -> Signal a1
         -> (Signal b1, Signal b2)
kernel22 :: Signal ((Cons, Cons), (Prod, Prod),
                    [a1] -> [a2] -> ([b1], [b2]))
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2)
kernel32 :: Signal ((Cons, Cons, Cons), (Prod, Prod),
                    [a1] -> [a2] -> [a3] -> ([b1], [b2]))
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2)
kernel42 :: Signal ((Cons, Cons, Cons, Cons), (Prod, Prod),
                    [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2)
kernel13 :: Signal (Cons, (Prod, Prod, Prod), [a1] -> ([b1], [b2], [b3]))
         -> Signal a1
         -> (Signal b1, Signal b2, Signal b3)
kernel23 :: Signal ((Cons, Cons), (Prod, Prod, Prod),
                    [a1] -> [a2] -> ([b1], [b2], [b3]))
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3)
kernel33 :: Signal ((Cons, Cons, Cons), (Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3)
kernel43 :: Signal ((Cons, Cons, Cons, Cons), (Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3)
kernel14 :: Signal (Cons, (Prod, Prod, Prod, Prod),
                    [a1] -> ([b1], [c], [d], [e]))
         -> Signal a1
         -> (Signal b1, Signal c, Signal d, Signal e)
kernel24 :: Signal ((Cons, Cons), (Prod, Prod, Prod, Prod),
                    [a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
         -> Signal a1 -> Signal a2
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
kernel34 :: Signal ((Cons, Cons, Cons), (Prod, Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
         -> Signal a1 -> Signal a2 -> Signal a3
         -> (Signal b1, Signal b2, Signal b3, Signal b4)
kernel44 :: Signal ((Cons, Cons, Cons, Cons), (Prod, Prod, Prod, Prod),
                    [a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
         -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
         -> (Signal b1, Signal b2, Signal b3, Signal b4)

kernel11 sc = SA.kernel11 (smap scen11 sc)
kernel12 sc = SA.kernel12 (smap scen12 sc)
kernel13 sc = SA.kernel13 (smap scen13 sc)
kernel14 sc = SA.kernel14 (smap scen14 sc)
kernel21 sc = SA.kernel21 (smap scen21 sc)
kernel22 sc = SA.kernel22 (smap scen22 sc)
kernel23 sc = SA.kernel23 (smap scen23 sc)
kernel24 sc = SA.kernel24 (smap scen24 sc)
kernel31 sc = SA.kernel31 (smap scen31 sc)
kernel32 sc = SA.kernel32 (smap scen32 sc)
kernel33 sc = SA.kernel33 (smap scen33 sc)
kernel34 sc = SA.kernel34 (smap scen34 sc)
kernel41 sc = SA.kernel41 (smap scen41 sc)
kernel42 sc = SA.kernel42 (smap scen42 sc)
kernel43 sc = SA.kernel43 (smap scen43 sc)
kernel44 sc = SA.kernel44 (smap scen44 sc)


detector11 :: Cons
           -> (s -> [a1] -> s)    -- ^ next state function (@f@)
           -> (s -> (Prod, [y]))  -- ^ scenario selection (@g@)
           ->  s                  -- ^ initial state (@s0@)
           -> Signal a1           -- ^ Input
           -> Signal y            -- ^ Output
detector21 :: (Cons, Cons)
           -> (s -> [a1] -> [a2] -> s) 
           -> (s -> (Prod, [y]))
           ->  s 
           -> Signal a1 -> Signal a2
           -> Signal y           
detector31 :: (Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> s)
           -> (s -> (Prod, [y]))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3
           -> Signal y
detector41 :: (Cons, Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> [a4] -> s)
           -> (s -> (Prod, [y]))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> Signal y
detector12 :: Cons
           -> (s -> [a1] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2])))
           -> s
           -> Signal a1
           -> (Signal y1, Signal y2)
detector22 :: (Cons, Cons)
           -> (s -> [a1] -> [a2] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2])))
           -> s
           -> Signal a1 -> Signal a2
           -> (Signal y1, Signal y2)
detector32 :: (Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2])))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3
           -> (Signal y1, Signal y2)
detector42 :: (Cons, Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> [a4] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2])))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> (Signal y1, Signal y2)
detector13 :: Cons
           -> (s -> [a1] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3])))
           -> s
           -> Signal a1
           -> (Signal y1, Signal y2, Signal y3)
detector23 :: (Cons, Cons)
           -> (s -> [a1] -> [a2] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3])))
           -> s
           -> Signal a1 -> Signal a2
           -> (Signal y1, Signal y2, Signal y3)
detector33 :: (Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3])))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3
           -> (Signal y1, Signal y2, Signal y3)
detector43 :: (Cons, Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> [a4] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3])))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> (Signal y1, Signal y2, Signal y3)  
detector14 :: Cons
           -> (s -> [a1] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3]), (Prod, [y4])))
           -> s
           -> Signal a1
           -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector24 :: (Cons, Cons)
           -> (s -> [a1] -> [a2] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3]), (Prod, [y4])))
           -> s
           -> Signal a1 -> Signal a2
           -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector34 :: (Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3]), (Prod, [y4])))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3
           -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector44 :: (Cons, Cons, Cons, Cons)
           -> (s -> [a1] -> [a2] -> [a3] -> [a4] -> s)
           -> (s -> ((Prod, [y1]), (Prod, [y2]), (Prod, [y3]), (Prod, [y4])))
           -> s
           -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4
           -> (Signal y1, Signal y2, Signal y3, Signal y4)
           
wrapFromSecond f wrap = \s -> wrap $ f s
detector11 c ns od s0 = SA.detector11 (wrapFromSecond ns (arg1 c)) od (SDF s0)
detector21 c ns od s0 = SA.detector21 (wrapFromSecond ns (arg2 c)) od (SDF s0)
detector31 c ns od s0 = SA.detector31 (wrapFromSecond ns (arg3 c)) od (SDF s0)
detector41 c ns od s0 = SA.detector41 (wrapFromSecond ns (arg4 c)) od (SDF s0)
detector12 c ns od s0 = SA.detector12 (wrapFromSecond ns (arg1 c)) od (SDF s0)
detector22 c ns od s0 = SA.detector22 (wrapFromSecond ns (arg2 c)) od (SDF s0)
detector32 c ns od s0 = SA.detector32 (wrapFromSecond ns (arg3 c)) od (SDF s0)
detector42 c ns od s0 = SA.detector42 (wrapFromSecond ns (arg4 c)) od (SDF s0)
detector13 c ns od s0 = SA.detector13 (wrapFromSecond ns (arg1 c)) od (SDF s0)
detector23 c ns od s0 = SA.detector23 (wrapFromSecond ns (arg2 c)) od (SDF s0)
detector33 c ns od s0 = SA.detector33 (wrapFromSecond ns (arg3 c)) od (SDF s0)
detector43 c ns od s0 = SA.detector43 (wrapFromSecond ns (arg4 c)) od (SDF s0)
detector14 c ns od s0 = SA.detector14 (wrapFromSecond ns (arg1 c)) od (SDF s0)
detector24 c ns od s0 = SA.detector24 (wrapFromSecond ns (arg2 c)) od (SDF s0)
detector34 c ns od s0 = SA.detector34 (wrapFromSecond ns (arg3 c)) od (SDF s0)
detector44 c ns od s0 = SA.detector44 (wrapFromSecond ns (arg4 c)) od (SDF s0)


