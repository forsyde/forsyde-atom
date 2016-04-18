module ForSyDe.PatternsTemp where

import ForSyDe.Core

fanout a                = a :> fanout a
fanoutn n a | n > 0     = a :> fanoutn (n-1) a
            | otherwise = NullV



-- -- | The pattern constructor 'reduce' creates a reduction network from a base process
-- reduce  :: VecSig vsa 
--            => (vsa -> vsa -> vsa) -- ^ commutative process with two inputs
--            -> Vector vsa          -- ^ input vector
--            -> vsa                 -- ^ output signal

-- -- | The pattern constructor 'reduce' creates a reduction network from a base process constructor applied on a vector of parameters.
-- reduce1 :: VecSig vsa 
--            => (b -> vsa -> vsa -> vsa)  -- ^ commutative process constructor with one function parameter and two inputs 
--            -> Vector b                  -- ^ vector of function parameters
--            -> Vector vsa                -- ^ input vector of signals
--            -> vsa                       -- ^ output signal


reduce p vs           = pipeV (p §> tailV vs) (headV vs)
reduce1 p v1 vs       = pipeV (p §> v1 <§> tailV vs) (headV vs)
