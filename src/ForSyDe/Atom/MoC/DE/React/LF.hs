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
timer0 = instant ()

-- | A timer signal generator
timer :: TimeStamp -- ^ period
      -> Signal () -- ^ timer signal
timer period = generate1 id (period,())

-- | Like 'timer', but you can specify an initial phase.
timer' :: TimeStamp -- ^ phase
       -> TimeStamp -- ^ period
       -> Signal () -- ^ timer signal
timer' phase period = delay phase () $ timer period

-- ACTIONS

-- | A delayed action is simply a delayed signal. In ForSyDe the initial value is
-- /mandatory/. If there is a notion of /unknown/, this has to be captured by the
-- domain @a@ of the event (i.e. \"one layer below\").
actionD :: TimeStamp -> Signal a -> Signal a 
actionD = unsafeDelay

-- SYNC UTILITIES

states0inputs2 = (,)
states0inputs3 = (,,)
states0inputs4 = (,,,)
states1inputs1 i s1 i1       = syncAndObs11 i i1 s1
states2inputs1 i s1 s2 i1    = syncAndObs12 i i1 s1 s2
states3inputs1 i s1 s2 s3 i1 = syncAndObs13 i i1 s1 s2 s3
states1inputs2 i s1 i1 i2    = syncAndObs21 i i1 i2 s1
-- | Alias for 'syncAndObs11'. Denotes which actions come from states (with persistent
-- values) and which come from other actions or inputs (instantaneous).
--
-- >>> let sts = read "{1@0,2@3,3@6,4@10,5@13}" :: Signal Int
-- >>> let ins = read "{1@3,2@4,3@5,4@6,5@13}"  :: Signal Int
-- >>> states1inputs1 0 sts ins
-- ({1@3s,2@4s,3@5s,4@6s,5@13s},{1@0s,2@3s,2@4s,2@5s,3@6s,4@10s,5@13s})
states2inputs2 i s1 s2 i1 i2 = syncAndObs22 i i1 i2 s1 s2
states1inputs3 i s1 i1 i2 i3 = syncAndObs31 i i1 i2 i3 s1

-- REACTIONS

reaction11 = comb11 
reaction12 = comb12  
reaction13 = comb13 
reaction14 = comb14 
reaction21 f = (><) (comb21 f)
-- | Alias for 'comb22', tupled version, that can be chained directly from a
-- @statesXinputsY@ "utility", see 'states2inputs2'.
--
-- >>> let sts = read "{1@0,2@3,3@6,4@10,5@13}" :: Signal Int
-- >>> let ins = read "{1@3,2@4,3@5,4@6,5@13}"  :: Signal Int
-- >>> reaction21 (\a b -> [sum a + sum b]) $ states1inputs1 0 sts ins
-- {1@0s,3@3s,4@4s,5@5s,7@6s,4@10s,10@13s}
reaction22 f = (><) (comb22 f)
reaction23 f = (><) (comb23 f)
reaction24 f = (><) (comb24 f)
reaction31 f = (><<) (comb31 f)
reaction32 f = (><<) (comb32 f)
reaction33 f = (><<) (comb33 f)
reaction34 f = (><<) (comb34 f)
reaction41 f = (><<<) (comb41 f)
reaction42 f = (><<<) (comb42 f)
reaction43 f = (><<<) (comb43 f)
reaction44 f = (><<<) (comb44 f)


--------------- TEST -----------------

reactor1 :: TimeStamp -> Signal Int -> Signal Int
reactor1 period x = y
  where
    t     = timer period
    count = state11 (\s _ -> s + 1) 0 t
    a     = actionD (milisec 200) count
    y     = reaction21 (\i1 i2 -> [sum i1 + sum i2])
            $ states1inputs1 0 a x
