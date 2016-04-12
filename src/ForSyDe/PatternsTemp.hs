module ForSyDe.PatternsTemp where

import ForSyDe.Core

fanout a                = a :> fanout a
fanoutn n a | n > 0     = a :> fanoutn (n-1) a
            | otherwise = NullV
