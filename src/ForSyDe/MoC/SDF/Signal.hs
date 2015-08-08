{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSDFDe.MoC.SDF.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SDF.Signal where

import ForSyDe.Core

newtype Signal a = Signal { toS :: Stream a }


fromS = Signal

liftS :: (Stream a -> Stream b) 
       -> Signal a -> Signal b
liftS f = fromS . f . toS

liftS2 :: (Stream a -> Stream b -> Stream c) 
       -> Signal a -> Signal b -> Signal c
liftS2 f xs ys = fromS $ f (toS xs) (toS ys)


instance Foldable Stream where
  foldr f z NullS     = z
  foldr f z (x :- xs) = f x (foldr f z xs)

-- | Only 'Show' instance. To 'Read' it, use the 'Stream' instance.
instance (Show a) => Show (Signal a) where
  showsPrec p = showsPrec p . toS

-- | instance defining the SDFnchronous MoC behavior
instance Signals Signal where
 -- | To abide the synchronicicy law, a data-filtered signal must replace missing tokens with 
 --   absent values, thus the filtered type is 'AbstExt'
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



-- | converts a list directly to a SDF signal.
signal :: [a] -> Signal a 
signal = fromS . stream 


-- | converts a SDF signal into a list.
fromSignal :: Signal a -> [a]
fromSignal = fromStream . toS

infixl 4 §-, -§-
-- | operator for functional application on signals
(§-) :: ([a] -> b) -> (Int, Signal a) -> Stream b
f §- (c, xs) = let x    = fromStream $ takeS c $ toS xs
                   xs'  = (dropS c) `liftS` xs
               in if length x == c then f x :- f §- (c,xs') else NullS
  
(-§-) :: Stream ([a] -> b) -> (Int, Signal a) -> Stream b
NullS   -§- _          = NullS
(f:-fs) -§- (c, xs)    = let x  = fromStream $ takeS c $ toS xs 
                             xs'= (dropS c) `liftS` xs
                         in if length x == c then f x :- fs -§- (c,xs') else NullS
(§!-) :: ([a] -> b) -> (Int, Signal a) -> Stream b
f §!- (c, xs) | c <= 0    = error "Number of consumed tokens must be positive integer"
              | otherwise = f §- (c, xs)

(-§!-) :: Stream ([a] -> b) -> (Int, Signal a) -> Stream b
fs -§!- (c, xs) | c <= 0    = error "Number of consumed tokens must be positive integer"
                | otherwise = fs -§- (c, xs)


tokenize :: Stream [a] -> Signal a
tokenize = signal . concat . fromStream


zippx :: Vector (Stream a) -> Stream (Vector (AbstExt a))
zippx NullV         = pure NullV
zippx (x:>xs)       = (:>) <$> padS Abst (pure <$> x) <*> zippx xs

unzippx :: Stream (Vector (AbstExt a)) -> Vector (Stream a)
unzippx NullS     = pure NullS
unzippx (x :- xs) = fromPadded §> x <§> unzippx xs
  where fromPadded Abst as     = as
        fromPadded (Prst a) as = a :- as  

anyV c = any c . fromVector 

