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

-- | The 'Signal' type denotes a 'Stream' that behaves according to the SDF MoC. Its tokens ('TokT') are plain values.
newtype Signal a = Signal { 
  -- | Function to unwrap a SDF signal to its base 'Stream'. This way one can operate directly on the tokens.
  toS :: Stream a 
}

-- | the function 'fromS' replaces the 'Signal' constructor for convenience. Wraps a 'Stream' of 'AbstExt' values into a SY 'Signal'.
fromS = Signal

-- | converts a list directly to a SDF signal.
signal :: [a] -> Signal a 
signal = fromS . stream 


-- | converts a SDF signal into a list.
fromSignal :: Signal a -> [a]
fromSignal = fromStream . toS

--- THE FOLLOWING IS NOT EXPORTED TO THE STANDARD DESIGN FRAMEWORK

-- | 'liftS' applies a function of streams on a 'Signal', and returns a 'Signal'
liftS :: (Stream a -> Stream b) -> Signal a -> Signal b
liftS f = fromS . f . toS

-- | 'liftS2' applies a function of 2 streams on two 'Signal's, and returns a 'Signal'
liftS2 :: (Stream a -> Stream b -> Stream c) -> Signal a -> Signal b -> Signal c
liftS2 f xs ys = fromS $ f (toS xs) (toS ys)

infixl 4 §-, -§-, §!-, -§!-
-- | this is the basic functor operator used in process constructors. It takes a function on a list and applies it on a tuple of form ('consumption_rate', 'Signal'). It returns a 'Stream' (the key for applying functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(§-) :: ([a] -> b) -> (Int, Signal a) -> Stream b
f §- (c, xs) = let x    = fromStream $ takeS c $ toS xs
                   xs'  = (dropS c) `liftS` xs
               in if length x == c then f x :- f §- (c,xs') else NullS

-- | this is the basic lifting operator used in process constructors. It takes a stream of functions on lists and applies it on a tuple of form ('consumption_rate', 'Signal'). It returns a 'Stream' (the key for applying functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(-§-) :: Stream ([a] -> b) -> (Int, Signal a) -> Stream b
NullS   -§- _          = NullS
(f:-fs) -§- (c, xs)    = let x  = fromStream $ takeS c $ toS xs 
                             xs'= (dropS c) `liftS` xs
                         in if length x == c then f x :- fs -§- (c,xs') else NullS

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

