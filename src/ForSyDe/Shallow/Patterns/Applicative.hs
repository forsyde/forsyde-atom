-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Patterns.Applicative
-- Copyright   :  ...
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  <ugeorge@kth.se>
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Patterns.Applicative where

import ForSyDe.Shallow.Core
import ForSyDe.Shallow.Patterns.Vector
import ForSyDe.Shallow.Patterns.Transition

-- $staticdoc
-- Static process networks do not change their structure throughout the execution, 
-- i.e. the (high-level) structure is not data dependent.

-- $dynadoc
-- Dynamic process networks change their structure throughout the execution. 
-- In order to implement dynamic process networks, transformations towards static 
-- process networks need to be defined. 

-- STATIC PROCESS NETWORK PATTERNS

-- map patterns
farmPN  :: (VSig vsa, VSig vsb) => 
         (vsa -> vsb) -- ^ process
         -> Vector vsa   -- ^ input vector of signals
         -> Vector vsb   -- ^ output vector of signals 
farm1PN :: (VSig vsa, VSig vsb) => 
         (c -> vsa -> vsb) -- ^ process constructor with one function parameter 
         -> Vector c       -- ^ vector of function parameters
         -> Vector vsa   -- ^ input vector of signals
         -> Vector vsb   -- ^ output vector of signals 
farm2PN :: (VSig vsa, VSig vsb) => (c -> d -> vsa -> vsb) -> Vector c -> Vector d -> Vector vsa -> Vector vsb 
farm3PN :: (VSig vsa, VSig vsb) => (c -> d -> e -> vsa -> vsb) -> Vector c -> Vector d -> Vector e -> Vector vsa -> Vector vsb

-- pipeV/serial composition patterns
pipePN  :: (VSig vsa) => Int                    -- ^ number of repeated compositions
        -> (vsa -> vsa) -- ^ process
        -> vsa               -- ^ input signal
        -> vsa               -- ^ output signal
pipe1PN :: (VSig vsa) => (a -> vsa -> vsa) -- ^ process constructor with one function parameter
        -> Vector a                    -- ^ vector of function parameters
        -> vsa                    -- ^ input signal
        -> vsa                    -- ^ output signal
pipe2PN :: (VSig vsa) => (a -> c -> vsa -> vsa) -> Vector a -> Vector c -> vsa -> vsa
pipe3PN :: (VSig vsa) => (a -> c -> d -> vsa -> vsa) -> Vector a -> Vector c -> Vector d -> vsa -> vsa

-- scanV/parallel prefix patterns
dim1SystolicPN  :: (VSig vsa) => Int                    -- ^ number of repeated compositions
        -> (vsa -> vsa) -- ^ process
        -> vsa               -- ^ input signal 
        -> Vector vsa      -- ^ output vector of signals
dim1Systolic1PN :: (VSig vsa) => (a -> vsa -> vsa) -- ^ process constructor with one function parameter
        -> Vector a                    -- ^ vector of function parameters
        -> vsa                    -- ^ input signal 
        -> Vector vsa           -- ^ output vector of signals
dim1Systolic2PN :: (VSig vsa) => (a -> c -> vsa -> vsa) -> Vector a -> Vector c -> vsa -> Vector vsa 
dim1Systolic3PN :: (VSig vsa) => (a -> c -> d -> vsa -> vsa) -> Vector a -> Vector c -> Vector d -> vsa -> Vector vsa 

-- reduce/fold patterns
reducePN  :: (VSig vsa) => (vsa -> vsa -> vsa) -- ^ commutative process with two inputs
        -> Vector vsa                  -- ^ input vector
        -> vsa                           -- ^ output signal
reduce1PN :: (VSig vsa) => (b -> vsa -> vsa -> vsa)  -- ^ commutative process constructor with one function parameter and two inputs 
        -> Vector b                                 -- ^ vector of function parameters
        -> Vector vsa                        -- ^ input vector of signals
        -> vsa                                 -- ^ output signal
reduce2PN :: (VSig vsa) => (b -> c -> vsa -> vsa -> vsa) -> Vector b -> Vector c -> Vector vsa -> vsa 
reduce3PN :: (VSig vsa) => (b -> c -> d -> vsa -> vsa -> vsa) -> Vector b -> Vector c -> Vector d -> Vector vsa -> vsa 

-- filter patterns
-- | the 'maskv' pattern filters out signal valuevsbased on a boolean condition. It ivsa static process network, thus the output vector length will be fixed and equal to the input vector length.
--
-- /OBS/: this pattern disregardvsany MoC bound to the signals, and forces the boolean function avsa functor on the token values. This is desirable since a mask should always (statically) work on the input streams regardless of their production and consumption rates.
maskPN  :: (Signals s) => (a -> Bool)                 -- ^ condition (function)
        -> Vector (s a)           -- ^ input vector of signals
        -> Vector (s (Filtered s a)) -- ^ output vector of absent extended signals

mask1PN :: (Signals s) => Vector (a -> Bool)         -- ^ vector of conditions function
        -> Vector (s a)           -- ^ input vector of signals
        -> Vector (s (Filtered s a)) -- ^ output vector of absent extended signals


farmPN                = (§>)
farm1PN p v1 s1       = p §> v1 <§> s1
farm2PN p v1 v2 s1    = p §> v1 <§> v2 <§> s1
farm3PN p v1 v2 v3 s1 = p §> v1 <§> v2 <§> v3 <§> s1

pipePN n p         = pipeV (copyV n p)
pipe1PN p v1       = pipeV (p §> v1)
pipe2PN p v1 v2    = pipeV (p §> v1 <§> v2)
pipe3PN p v1 v2 v3 = pipeV (p §> v1 <§> v2 <§> v3) 

dim1SystolicPN n p s         = scanV (copyV n p) s <+> unitV s
dim1Systolic1PN p v1 s       = scanV (p §> v1) s <+> unitV s 
dim1Systolic2PN p v1 v2 s    = scanV (p §> v1 <§> v2) s <+> unitV s
dim1Systolic3PN p v1 v2 v3 s = scanV (p §> v1 <§> v2 <§> v3) s <+> unitV s

reducePN p vs           = pipeV (p §> tailV vs) (headV vs)
reduce1PN p v1 vs       = pipeV (p §> v1 <§> tailV vs) (headV vs)
reduce2PN p v1 v2 vs    = pipeV (p §> v1 <§> v2 <§> tailV vs) (headV vs)
reduce3PN p v1 v2 v3 vs = pipeV (p §> v1 <§> v2 <§> v3 <§> tailV vs) (headV vs)

maskPN c vs   = (-#- c) §> vs
mask1PN vc vs = (-#-) §> vs <§> vc

