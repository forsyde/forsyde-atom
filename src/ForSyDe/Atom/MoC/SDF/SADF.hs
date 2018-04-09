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
detector11 c ns od s0
  = SA.detector11 (\s -> arg1 c $ ns s) od (SDF s0)
