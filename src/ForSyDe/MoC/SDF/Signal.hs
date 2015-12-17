{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015; 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the SDF Signal.
--
-- This module should be separately imported only for experimental purpose,
-- and should not be included in a standard ForSyDe design. The functions 
-- necessary for the standard ForSyDe framework are re-exported by the parent 
-- module 'ForSyDe.MoC.SY.SY'.  
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF.Signal where

import ForSyDe.Core
import qualified Data.Param.FSVec as V
import Data.TypeLevel.Num hiding ((-),(+),(*),(>),(<),(>=),(<=),(==))

infixl 4 §-
-- | this is the basic functor operator used in process constructors. It takes a function on a list and applies it on a tuple of form ('consumption_rate', 'Signal'). It returns a 'Stream' (the key for applying functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(§-) :: (Nat cons) => (V.FSVec cons a -> b) -> Signal a -> Signal b
f §- sig = let x  = (V.unsafeVector c . take (toInt c) . fromSignal) sig
               xs = dropS (toInt c) sig
               c  = V.lengthT x
           in if toInt c <= lengthS xs then f x :- (f §- xs) else pure (f x)

-- | this is the basic lifting operator used in process constructors. It takes a stream of functions on lists and applies it on a tuple of form ('consumption_rate', 'Signal'). It returns a 'Stream' (the key for applying functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(-§-) :: (Nat cons) => Signal (V.FSVec cons a -> b) -> Signal a -> Signal b
NullS   -§- _   = NullS
(f:-fs) -§- sig = let x  = (V.unsafeVector c . take (toInt c) . fromSignal) sig
                      xs = dropS (toInt c) sig
                      c  = V.lengthT x
                  in if toInt c <= lengthS xs then f x :- (fs -§- xs) else pure (f x)
{-
-- | guarded functor operator. It adds a guard to §- to verify if the consumption rate is a positive integer.
(§!-) :: ([a] -> b) -> (Int, Signal a) -> Stream b
f §!- (c, xs) | c <= 0    = error "Number of consumed tokens must be positive integer"
              | otherwise = f §- (c, xs)

-- | guarded lifting operator. It adds a guard to -§- to verify if the consumption rate is a positive integer.
(-§!-) :: Stream ([a] -> b) -> (Int, Signal a) -> Stream b
fs -§!- (c, xs) | c <= 0    = error "Number of consumed tokens must be positive integer"
                | otherwise = fs -§- (c, xs)

-- | turns the result of the applicative operators §- and -§- back into 'Signal's.
tokenize :: Stream [a] -> Signal a
tokenize = signal . concat . fromStream

-- | A SDF 'Signal' is foldable.
instance Foldable Stream where
  foldr f z NullS     = z
  foldr f z (x :- xs) = f x (foldr f z xs)

-- | Only 'Show' instance. To 'Read' it, use the 'Stream' instance.
instance (Show a) => Show (Signal a) where
  showsPrec p = showsPrec p . toS

-- | instance defining the SDFnchronous MoC behavior
instance Signals Signal where
    type TokT Signal a = a

 -- (§§-) :: (a -> b) -> Signal a -> Signal b
    (§§-) f = liftS (f <$>)

 -- (-§§-) :: Signal (a -> b) -> Signal a -> Signal b
    (-§§-) = liftS2 (<*>)

 -- (->-) :: Signal a -> AbstExt a -> Signal a
    xs ->- i = liftS (i :-) xs

 -- filt :: (a -> Bool) -> Signal a -> Signal a 
    filt p = liftS (foldr (\a as -> if p a then a :- as else as) NullS) 

 -- zipx :: Vector (Signal a) -> Signal (Vector (AbstExt a))
    zipx = fromS . takeWhileS (anyV isPresent) . zippx . (§>) toS

 -- unzipx :: Signal (Vector (AbstExt a)) -> Vector (Signal a)
    unzipx  = (§>) fromS . unzippx . toS


zippx :: Vector (Stream a) -> Stream (Vector (AbstExt a))
zippx NullV         = pure NullV
zippx (x:>xs)       = (:>) <$> padS Abst (pure <$> x) <*> zippx xs

unzippx :: Stream (Vector (AbstExt a)) -> Vector (Stream a)
unzippx NullS     = pure NullS
unzippx (x :- xs) = fromPadded §> x <§> unzippx xs
  where fromPadded Abst as     = as
        fromPadded (Prst a) as = a :- as  

anyV c = any c . fromVector
-}

