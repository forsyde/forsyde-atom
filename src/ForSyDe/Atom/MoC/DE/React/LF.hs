{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Atom.MoC.DE.React.LF where

import           ForSyDe.Atom.MoC.DE.React.Core
import           ForSyDe.Atom.MoC.DE.React.Lib
import qualified ForSyDe.Atom.MoC.SY.Clocked as SYC
import           ForSyDe.Atom.MoC.TimeStamp
import           ForSyDe.Atom.Utility.Tuple ((><), (><<), (><<<))

-- CLOCKS --

-- | The equivalent of a timer set to period @0@ in LF. It triggers once at the
-- beginning of the execution and then it never triggers again.
timer0 :: Signal ()
timer0 = infinite ()

-- | A timer signal generator
timer :: TimeStamp -- ^ period
      -> Signal () -- ^ timer signal
timer period = generate1 id (period,())

-- | Like 'timer', but you can specify an initial phase.
timer' :: TimeStamp -- ^ phase
       -> TimeStamp -- ^ period
       -> Signal () -- ^ timer signal
timer' phase period = delay phase () $ timer period

-- STATES

-- | State process that can be affected by /one/ reactor.
--
-- >>> let s = signal [(0,(+1)), (3,(*2)), (7,(/2)), (10,id)]
-- >>> :t s
-- s :: Fractional a => Signal (a -> a)
-- >>> state1 0 s
-- {0.0@0s,1.0@3s,2.0@7s,1.0@10s}
state1 :: a               -- ^ initial state
       -> Signal (a -> a) -- ^ signal with events carrying state-modifying functions
       -> Signal a        -- ^ signal showing the /current state/
state1 i = embedSY11 (SYC.stated11 (\s f -> f s) i)

-- | Same as 'state1' but affected by two concurrent sources.
--
-- >>> let s1 = signal [(0,(+1)), (3,(*2)), (7,(/2)), (10,id)]
-- >>> let s2 = signal [(0,(+1)), (5,(*2)), (7,(/2))]
-- >>> state2 0 s1 s2
-- {0.0@0s,2.0@3s,4.0@5s,8.0@7s,2.0@10s}
state2 :: a               -- ^ initial state
       -> Signal (a -> a) -- ^ first modifier signal
       -> Signal (a -> a) -- ^ second modifier signal
       -> Signal a        -- ^ signal showing the /current state/
state2 i fs1 fs2
  = embedSY21 (SYC.stated21 (\s f1 f2 -> f1 $ f2 s) i)
    >< syncAndFill2 (id,id) fs1 fs2

state3 :: a -> Signal (a -> a) -> Signal (a -> a) -> Signal (a -> a)
       -> Signal a
state3 i fs1 fs2 fs3
  = embedSY31 (SYC.stated31 (\s f1 f2 f3 -> f1 $ f2 $ f3 s) i)
    ><< syncAndFill3 (id,id,id) fs1 fs2 fs3

-- state4 :: a -> Signal (a -> a) -> Signal (a -> a) -> Signal (a -> a)
--        -> Signal (a -> a) -> Signal a
-- state4 i fs1 fs2 fs3 fs4
--   = embedSY21 (SYC.stated41 (\s f1 f2 f3 f4 -> f1 $ f2 $ f3 $ f4 s) i)
--     ><<< syncAndFill4 (id,id,id,id) fs1 fs2 fs3 fs4

-- ACTIONS

-- | A delayed action is simply a delayed signal. In ForSyDe the initial value is
-- /mandatory/. If there is a notion of /unknown/, this has to be captured by the
-- domain @a@ of the event (i.e. \"one layer below\").
actionD :: TimeStamp -> Signal a -> Signal a 
actionD = unsafeDelay

-- REACTIONS

reaction11 = comb11
reaction12 = comb12
reaction13 = comb13
reaction14 = comb14
reaction21 = comb21
reaction22 = comb22
reaction23 = comb23
reaction24 = comb24
reaction31 = comb31
reaction32 = comb32
reaction33 = comb33
reaction34 = comb34
reaction41 = comb41
reaction42 = comb42
reaction43 = comb43
reaction44 = comb44
