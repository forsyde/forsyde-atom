{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Patterns.Applicative
-- Copyright   :  ...
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  <ugeorge@kth.se>
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------
module ForSyDe.Patterns.Applicative (
  -- ** map patterns
  farmPN, farm1PN, farm2PN, farm3PN,
  -- ** pipe patterns
  pipePN, pipe1PN, pipe2PN, pipe3PN, pipevPN,
  -- ** systolic array patterns
  dim1SystolicPN, dim1Systolic1PN, dim1Systolic2PN, dim1Systolic3PN,
  -- ** reduce patterns
  reducePN, reduce1PN, reduce2PN, reduce3PN,
  -- ** filter patterns
  maskPN, mask1PN,
) where

import ForSyDe.Core

-- $staticdoc
-- Static process networks do not change their structure throughout the execution, 
-- i.e. the (high-level) structure is not data dependent.

-- $dynadoc
-- Dynamic process networks change their structure throughout the execution. 
-- In order to implement dynamic process networks, transformations towards static 
-- process networks need to be defined. 

-- | The pattern constructor 'farmPN' takes a process and applies it on a vector of 'VecSig's
farmPN  :: (VecSig vsa, VecSig vsb) 
         => (vsa -> vsb) -- ^ process
         -> Vector vsa   -- ^ input vector of signals
         -> Vector vsb   -- ^ output vector of signals 

-- | The pattern constructor 'farm1PN' takes a process constructors which takes one parameter and applies it on a vector of 'VecSig's
farm1PN :: (VecSig vsa, VecSig vsb) 
         => (c -> vsa -> vsb) -- ^ process constructor with one function parameter 
         -> Vector c          -- ^ vector of function parameters
         -> Vector vsa        -- ^ input vector of signals
         -> Vector vsb        -- ^ output vector of signals 

-- | The pattern constructor 'farm2PN' is similar to 'farm1PN', but the process constructor takes two parameters
farm2PN :: (VecSig vsa, VecSig vsb) => (c -> d -> vsa -> vsb) -> Vector c -> Vector d -> Vector vsa -> Vector vsb

-- | The pattern constructor 'farm3PN' is similar to 'farm1PN', but the process constructor takes three parameters 
farm3PN :: (VecSig vsa, VecSig vsb) => (c -> d -> e -> vsa -> vsb) -> Vector c -> Vector d -> Vector e -> Vector vsa -> Vector vsb



-- | The pattern constructor 'pipePN' creates a serial composition by piping a process through /n/ stages.
pipePN  :: VecSig vsa 
         => Int          -- ^ number of repeated compositions
         -> (vsa -> vsa) -- ^ process
         -> vsa          -- ^ input signal
         -> vsa          -- ^ output signal

-- | The pattern constructor 'pipe1PN' creates a serial composition by piping a process constructor and applying a vector of parameters against it.
pipe1PN :: VecSig vsa 
         => (a -> vsa -> vsa) -- ^ process constructor with one function parameter
         -> Vector a          -- ^ vector of function parameters
         -> vsa               -- ^ input signal
         -> vsa               -- ^ output signal

-- | The pattern constructor 'pipe2PN' is similar to 'pipe1PN', but the process constructor takes two parameters 
pipe2PN :: VecSig vsa => (a -> c -> vsa -> vsa) -> Vector a -> Vector c -> vsa -> vsa

-- | The pattern constructor 'pipe3PN' is similar to 'pipe1PN', but the process constructor takes three parameters 
pipe3PN :: VecSig vsa => (a -> c -> d -> vsa -> vsa) -> Vector a -> Vector c -> Vector d -> vsa -> vsa

-- | The pattern constructor 'pipevPN' creates a serial composition by folding a vector of processes.
pipevPN :: VecSig vsa => Vector (vsa -> vsa) -> vsa -> vsa


-- | The pattern constructor 'dim1SystolicPN' creates a 1D systolic array (for example a FIR filter) by applying parallel prefix on a  /n/-stage pipe of processes.
dim1SystolicPN  :: VecSig vsa 
                 => Int          -- ^ number of stages
                 -> (vsa -> vsa) -- ^ base process
                 -> vsa          -- ^ input signal 
                 -> Vector vsa   -- ^ output vector of signals

-- | The pattern constructor 'dim1Systolic1PN' creates a 1D systolic array (for example a FIR filter) by applying parallel prefix on a pipe of processes created from a base process constructor and a vector of parameters.
dim1Systolic1PN :: VecSig vsa 
        => (a -> vsa -> vsa) -- ^ process constructor with one function parameter
        -> Vector a          -- ^ vector of function parameters
        -> vsa               -- ^ input signal 
        -> Vector vsa        -- ^ output vector of signals

-- | The pattern constructor 'dim1Systolic2PN' is similar to 'dim1Systolic1PN', but the process constructor takes two parameters 
dim1Systolic2PN :: VecSig vsa => (a -> c -> vsa -> vsa) -> Vector a -> Vector c -> vsa -> Vector vsa 

-- | The pattern constructor 'dim1Systolic3PN' is similar to 'dim1Systolic1PN', but the process constructor takes three parameters 
dim1Systolic3PN :: VecSig vsa => (a -> c -> d -> vsa -> vsa) -> Vector a -> Vector c -> Vector d -> vsa -> Vector vsa 


-- | The pattern constructor 'reducePN' creates a reduction network from a base process
reducePN  :: VecSig vsa 
           => (vsa -> vsa -> vsa) -- ^ commutative process with two inputs
           -> Vector vsa          -- ^ input vector
           -> vsa                 -- ^ output signal

-- | The pattern constructor 'reducePN' creates a reduction network from a base process constructor applied on a vector of parameters.
reduce1PN :: VecSig vsa 
           => (b -> vsa -> vsa -> vsa)  -- ^ commutative process constructor with one function parameter and two inputs 
           -> Vector b                  -- ^ vector of function parameters
           -> Vector vsa                -- ^ input vector of signals
           -> vsa                       -- ^ output signal

-- | The pattern constructor 'reduce2PN' is similar to 'reduce1PN', but the process constructor takes two parameters 
reduce2PN :: VecSig vsa => (b -> c -> vsa -> vsa -> vsa) -> Vector b -> Vector c -> Vector vsa -> vsa 

-- | The pattern constructor 'reduce3PN' is similar to 'reduce1PN', but the process constructor takes three parameters 
reduce3PN :: VecSig vsa => (b -> c -> d -> vsa -> vsa -> vsa) -> Vector b -> Vector c -> Vector d -> Vector vsa -> vsa 


-- | The 'maskPN' pattern filters out signal values based on a boolean condition. It is a static process network, thus the output vector length will be fixed and equal to the input vector length.
maskPN  :: (Signals s) 
         => (a -> Bool)                -- ^ condition (function)
         -> Vector (s a) -- ^ input vector of signals
         -> Vector (s a) -- ^ output vector of absent extended signals

-- | the 'mask1PN' pattern filters out signal values based on a vector of boolean conditions. It is a static process network, thus the output vector length will be fixed and equal to the input vector length.
mask1PN :: (Signals s) => Vector (a -> Bool)-- ^ vector of conditions function
        -> Vector (s a)   -- ^ input vector of signals
        -> Vector (s a)   -- ^ output vector of absent extended signals


farmPN                = (§>)
farm1PN p v1 s1       = p §> v1 <§> s1
farm2PN p v1 v2 s1    = p §> v1 <§> v2 <§> s1
farm3PN p v1 v2 v3 s1 = p §> v1 <§> v2 <§> v3 <§> s1

pipevPN            = pipeV 
pipePN  n p        = pipeV (copyV n p)
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

maskPN c vs   = (filt c) §> vs
mask1PN vc vs = (filt) §> vc <§> vs

