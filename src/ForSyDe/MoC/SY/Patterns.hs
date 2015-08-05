module ForSyDe.MoC.SY.Patterns (
  gatherAdpPN, gatherAdp2PN, gatherAdp3PN, gatherAdp4PN, gatherAdp5PN,
  ) where

import ForSyDe.Core
import ForSyDe.MoC.SY.Signal
import ForSyDe.MoC.SY.Process

-- | 'gatherAdpPN' is the adaptive version of 'gatherVecPN' where the vector of indexes for each event is carried by a signal.
gatherAdpPN  :: Signal (Vector Int) -> Vector (Signal a) -> Vector (Signal a)

-- | 'gatherAdp2PN' is the adaptive version of 'gatherVec2PN' where the indexes for each event is carried by a signal.
gatherAdp2PN :: Signal (Vector (Vector Int)) -> Vector (Signal a) -> Vector (Signal (Vector a))

-- | 'gatherAdp3PN' is the adaptive version of 'gatherVec3PN' where the indexes for each event is carried by a signal.
gatherAdp3PN :: Signal (Vector (Vector (Vector Int))) -> Vector (Signal a) -> Vector (Signal (Vector (Vector a)))

-- | 'gatherAdp4PN' is the adaptive version of 'gatherVec4PN' where the indexes for each event is carried by a signal.
gatherAdp4PN :: Signal (Vector (Vector (Vector (Vector Int)))) -> Vector (Signal a) -> Vector (Signal (Vector (Vector (Vector a))))

-- | 'gatherAdp5PN' is the adaptive version of 'gatherVec5PN' where the indexes for each event is carried by a signal.
gatherAdp5PN :: Signal (Vector (Vector (Vector (Vector (Vector Int))))) -> Vector (Signal a) -> Vector (Signal (Vector (Vector (Vector (Vector a)))))

gatherAdpPN ixs v  = unzipx $ (\ixv vec -> (§>) (atV vec) ixv) §§- ixs -§§- zipx v
gatherAdp2PN ixs v = unzipx $ (\ixv vec -> ((§>).(§>)) (atV vec) ixv) §§- ixs -§§- zipx v
gatherAdp3PN ixs v = unzipx $ (\ixv vec -> ((§>).(§>).(§>)) (atV vec) ixv) §§- ixs -§§- zipx v
gatherAdp4PN ixs v = unzipx $ (\ixv vec -> ((§>).(§>).(§>).(§>)) (atV vec) ixv) §§- ixs -§§- zipx v
gatherAdp5PN ixs v = unzipx $ (\ixv vec -> ((§>).(§>).(§>).(§>).(§>)) (atV vec) ixv) §§- ixs -§§- zipx v

