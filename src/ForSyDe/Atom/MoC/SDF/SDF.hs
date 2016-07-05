{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SDF
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronuous library defines process constructors, processes and a signal conduit
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process. 
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF where

import Prelude hiding (zip, zip3, filter, unzip, unzip3)
import ForSyDe.Core
import ForSyDe.Core.Utility
import Data.TypeLevel hiding ((==))
import Data.Param.FSVec as V hiding (length, take)


infixl 5 -$-
(-$-) :: (Nat cons) => (FSVec cons a -> b) -> Signal a -> Signal b
_ -$- NullS = NullS
f -$- sig   = let x  = (reallyUnsafeVector . take c . fromSignal) sig
                  xs = dropS c sig 
                  c  = toInt $ lengthT x
              in if c == length (V.fromVector x) then f x :- (f -$- xs) else NullS


(-*-) :: (Nat cons) => Signal (V.FSVec cons a -> b) -> Signal a -> Signal b
NullS -*- _     = NullS
_   -*- NullS   = NullS
(f:-fs) -*- sig = let x  = (reallyUnsafeVector . take c . fromSignal) sig
                      xs = dropS c sig 
                      c  = toInt $ lengthT x
                  in if c == length (V.fromVector x) then f x :- (fs -*- xs) else NullS


infixl 3 ¤
(¤) :: (Nat prod) => Signal (V.FSVec prod a) -> Signal a
(¤) NullS   = NullS
(¤) (x:-xs) = signal (V.fromVector $ x) +-+ (xs ¤)


-- | operator for the default delay function
(->-) :: Signal a -> a -> Signal a
xs ->- i = i :- xs

infixl 3 -<, -<<, -<<<, -<<<<, -<<<<<, -<<<<<<, -<<<<<<<
(-<)       s = funzip2 (s ¤)
(-<<)      s = funzip3 (s ¤)
(-<<<)     s = funzip4 (s ¤)
(-<<<<)    s = funzip5 (s ¤)
(-<<<<<)   s = funzip6 (s ¤)
(-<<<<<<)  s = funzip7 (s ¤)
(-<<<<<<<) s = funzip8 (s ¤)

-----------------------------------------------------------------------------
-- PROCESS CONSTRUCTORS / PATTERNS
-----------------------------------------------------------------------------
comb11 f s1          = (f -$- s1 ¤)
comb12 f s1          = (f -$- s1 -<)
comb13 f s1          = (f -$- s1 -<<)
comb14 f s1          = (f -$- s1 -<<<)
comb21 f s1 s2       = (f -$- s1 -*- s2 ¤)
comb22 f s1 s2       = (f -$- s1 -*- s2 -<)
comb23 f s1 s2       = (f -$- s1 -*- s2 -<<)
comb24 f s1 s2       = (f -$- s1 -*- s2 -<<<)
comb31 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 ¤)
comb32 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<)
comb33 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<<)
comb34 f s1 s2 s3    = (f -$- s1 -*- s2 -*- s3 -<<<)
comb41 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 ¤)
comb42 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<)
comb43 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
comb44 f s1 s2 s3 s4 = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)
                      
delay s1 xs = s1 ->- xs
