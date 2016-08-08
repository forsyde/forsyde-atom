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
  farm, farm1, farm2, farm3,
  -- ** pipe patterns
  pipe, pipe1, pipe2, pipe3, pipev,
  -- ** systolic array patterns
  dim1Systolic, dim1Systolic1, dim1Systolic2, dim1Systolic3,
  -- ** reduce patterns
  reduce, reduce1, reduce2, reduce3,
  -- ** filter patterns
  mask, mask1,
) where

import ForSyDe.Core

-- $staticdoc
-- Static process networks do not change their structure throughout the execution, 
-- i.e. the (high-level) structure is not data dependent.

-- $dynadoc
-- Dynamic process networks change their structure throughout the execution. 
-- In order to implement dynamic process networks, transformations towards static 
-- process networks need to be defined. 

-- | The pattern constructor 'farm' takes a process and applies it on a vector of 'VecSig's
farm  :: (VecSig vsa, VecSig vsb) 
         => (vsa -> vsb) -- ^ process
         -> Vector vsa   -- ^ input vector of signals
         -> Vector vsb   -- ^ output vector of signals 

-- | The pattern constructor 'farm1' takes a process constructors which takes one parameter and applies it on a vector of 'VecSig's
farm1 :: (VecSig vsa, VecSig vsb) 
         => (c -> vsa -> vsb) -- ^ process constructor with one function parameter 
         -> Vector c          -- ^ vector of function parameters
         -> Vector vsa        -- ^ input vector of signals
         -> Vector vsb        -- ^ output vector of signals 

-- | The pattern constructor 'farm2' is similar to 'farm1', but the process constructor takes two parameters
farm2 :: (VecSig vsa, VecSig vsb) => (c -> d -> vsa -> vsb) -> Vector c -> Vector d -> Vector vsa -> Vector vsb

-- | The pattern constructor 'farm3' is similar to 'farm1', but the process constructor takes three parameters 
farm3 :: (VecSig vsa, VecSig vsb) => (c -> d -> e -> vsa -> vsb) -> Vector c -> Vector d -> Vector e -> Vector vsa -> Vector vsb



-- | The pattern constructor 'pipe' creates a serial composition by piping a process through /n/ stages.
pipe  :: VecSig vsa 
         => Int          -- ^ number of repeated compositions
         -> (vsa -> vsa) -- ^ process
         -> vsa          -- ^ input signal
         -> vsa          -- ^ output signal

-- | The pattern constructor 'pipe1' creates a serial composition by piping a process constructor and applying a vector of parameters against it.
pipe1 :: VecSig vsa 
         => (a -> vsa -> vsa) -- ^ process constructor with one function parameter
         -> Vector a          -- ^ vector of function parameters
         -> vsa               -- ^ input signal
         -> vsa               -- ^ output signal

-- | The pattern constructor 'pipe2' is similar to 'pipe1', but the process constructor takes two parameters 
pipe2 :: VecSig vsa => (a -> c -> vsa -> vsa) -> Vector a -> Vector c -> vsa -> vsa

-- | The pattern constructor 'pipe3' is similar to 'pipe1', but the process constructor takes three parameters 
pipe3 :: VecSig vsa => (a -> c -> d -> vsa -> vsa) -> Vector a -> Vector c -> Vector d -> vsa -> vsa

-- | The pattern constructor 'pipev' creates a serial composition by folding a vector of processes.
pipev :: VecSig vsa => Vector (vsa -> vsa) -> vsa -> vsa


-- | The pattern constructor 'dim1Systolic' creates a 1D systolic array (for example a FIR filter) by applying parallel prefix on a  /n/-stage pipe of processes.
dim1Systolic  :: VecSig vsa 
                 => Int          -- ^ number of stages
                 -> (vsa -> vsa) -- ^ base process
                 -> vsa          -- ^ input signal 
                 -> Vector vsa   -- ^ output vector of signals

-- | The pattern constructor 'dim1Systolic1' creates a 1D systolic array (for example a FIR filter) by applying parallel prefix on a pipe of processes created from a base process constructor and a vector of parameters.
dim1Systolic1 :: VecSig vsa 
        => (a -> vsa -> vsa) -- ^ process constructor with one function parameter
        -> Vector a          -- ^ vector of function parameters
        -> vsa               -- ^ input signal 
        -> Vector vsa        -- ^ output vector of signals

-- | The pattern constructor 'dim1Systolic2' is similar to 'dim1Systolic1', but the process constructor takes two parameters 
dim1Systolic2 :: VecSig vsa => (a -> c -> vsa -> vsa) -> Vector a -> Vector c -> vsa -> Vector vsa 

-- | The pattern constructor 'dim1Systolic3' is similar to 'dim1Systolic1', but the process constructor takes three parameters 
dim1Systolic3 :: VecSig vsa => (a -> c -> d -> vsa -> vsa) -> Vector a -> Vector c -> Vector d -> vsa -> Vector vsa 


-- | The pattern constructor 'reduce' creates a reduction network from a base process
reduce  :: VecSig vsa 
           => (vsa -> vsa -> vsa) -- ^ commutative process with two inputs
           -> Vector vsa          -- ^ input vector
           -> vsa                 -- ^ output signal

-- | The pattern constructor 'reduce' creates a reduction network from a base process constructor applied on a vector of parameters.
reduce1 :: VecSig vsa 
           => (b -> vsa -> vsa -> vsa)  -- ^ commutative process constructor with one function parameter and two inputs 
           -> Vector b                  -- ^ vector of function parameters
           -> Vector vsa                -- ^ input vector of signals
           -> vsa                       -- ^ output signal

-- | The pattern constructor 'reduce2' is similar to 'reduce1', but the process constructor takes two parameters 
reduce2 :: VecSig vsa => (b -> c -> vsa -> vsa -> vsa) -> Vector b -> Vector c -> Vector vsa -> vsa 

-- | The pattern constructor 'reduce3' is similar to 'reduce1', but the process constructor takes three parameters 
reduce3 :: VecSig vsa => (b -> c -> d -> vsa -> vsa -> vsa) -> Vector b -> Vector c -> Vector d -> Vector vsa -> vsa 


-- | The 'mask' pattern filters out signal values based on a boolean condition. It is a static process network, thus the output vector length will be fixed and equal to the input vector length.
mask  :: (Signals s) 
         => (a -> Bool)                -- ^ condition (function)
         -> Vector (s a) -- ^ input vector of signals
         -> Vector (s a) -- ^ output vector of absent extended signals

-- | the 'mask1' pattern filters out signal values based on a vector of boolean conditions. It is a static process network, thus the output vector length will be fixed and equal to the input vector length.
mask1 :: (Signals s) => Vector (a -> Bool)-- ^ vector of conditions function
        -> Vector (s a)   -- ^ input vector of signals
        -> Vector (s a)   -- ^ output vector of absent extended signals


farm                = (§>)
farm1 p v1 s1       = p §> v1 <§> s1
farm2 p v1 v2 s1    = p §> v1 <§> v2 <§> s1
farm3 p v1 v2 v3 s1 = p §> v1 <§> v2 <§> v3 <§> s1

pipev            = pipeV 
pipe  n p        = pipeV (copyV n p)
pipe1 p v1       = pipeV (p §> v1)
pipe2 p v1 v2    = pipeV (p §> v1 <§> v2)
pipe3 p v1 v2 v3 = pipeV (p §> v1 <§> v2 <§> v3) 

dim1Systolic n p s         = scanV (copyV n p) s <+> unitV s
dim1Systolic1 p v1 s       = scanV (p §> v1) s <+> unitV s 
dim1Systolic2 p v1 v2 s    = scanV (p §> v1 <§> v2) s <+> unitV s
dim1Systolic3 p v1 v2 v3 s = scanV (p §> v1 <§> v2 <§> v3) s <+> unitV s

reduce p vs           = pipeV (p §> tailV vs) (headV vs)
reduce1 p v1 vs       = pipeV (p §> v1 <§> tailV vs) (headV vs)
reduce2 p v1 v2 vs    = pipeV (p §> v1 <§> v2 <§> tailV vs) (headV vs)
reduce3 p v1 v2 v3 vs = pipeV (p §> v1 <§> v2 <§> v3 <§> tailV vs) (headV vs)

mask c vs   = (filt c) §> vs
mask1 vc vs = (filt) §> vc <§> vs

