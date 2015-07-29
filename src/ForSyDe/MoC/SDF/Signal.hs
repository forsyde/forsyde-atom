{-# LANGUAGE TypeFamilies #-}
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

module ForSyDe.MoC.SDF.Signal (
  SignalSDF (..),
  -- ** Interface functions
  signalSDF, fromSignalSDF, (§-), (-§-),
) where

import ForSyDe.Core

-- | The type 'SignalSDF' denotes a 'Stream' that behaves according to the SDFchronous MoC. 
newtype SignalSDF a = SignalSDF { fromSDF :: Stream a }

-- | Only 'Show' instance. To 'Read' it, use the 'Stream' instance.
instance (Show a) => Show (SignalSDF a) where
  showsPrec p = showsPrec p . fromSDF

instance Functor SignalSDF where
  fmap f = SignalSDF . fmapSDF f . fromSDF
    where fmapSDF _ NullS   = NullS
          fmapSDF f (x:-xs) = f x :- fmapSDF f xs

instance Applicative SignalSDF where
  pure a  = SignalSDF (repeatS a)
  a <*> b = SignalSDF $ starSDF (fromSDF a) (fromSDF b)
    where starSDF NullS     NullS     = NullS
          starSDF NullS     _         = NullS
          starSDF (f :- fs) (x :- xs) = f x :- starSDF fs xs

instance Foldable SignalSDF where
  foldr fa za xa = foldrSDF fa za (fromSDF xa)
    where foldrSDF f z NullS     = z
          foldrSDF f z (x :- xs) = f x (foldrSDF f z xs)

-- | instance defining the SDFnchronous MoC behavior
instance Signal SignalSDF where
  -- | To abide the SDFnchronicicy law, a data-filtered signal must replace missing tokens with 
  --   absent values, thus the filtered type is 'AbstExt'
  type Filtered SignalSDF a = a 
  type Padded SignalSDF a = AbstExt a
  --------
  toS   = fromSDF
  fromS = SignalSDF
  --------
  xs -#- p = foldr (\a as -> if p a then liftS (a :-) as else as) (fromS NullS) xs
  --------
  safe = ((<$>) . (§>)) Prst
  --------
  zipx = liftS (takeWhileS padded) . zippx
    where zippx NullV         = pure NullV
          zippx (x:>xs)       = (:>) <$> liftS (padS Abst) (fmap Prst x) <*> zippx xs
          padded NullV        = False
          padded (Abst :> vs) = padded vs
          padded _            = True
  --------
  unzipx = (§>) fromS . unzx . toS
    where unzx NullS     = pure NullS
          unzx (x :- xs) = fromPadded §> x <§> unzx xs
          fromPadded Abst as = as
          fromPadded (Prst a) as = a :- as    
  --------

-- | converts a list directly to a SDF signal.
signalSDF :: [a] -> SignalSDF a 
signalSDF = SignalSDF . stream 


-- | converts a SDF signal into a list.
fromSignalSDF :: SignalSDF a -> [a]
fromSignalSDF = fromStream . fromSDF


infixl 4 §-, -§-
-- | operator for functional application on signals
(§-) :: ([a] -> b) -> (Int, SignalSDF a) -> SignalSDF b
f §- (c, xs) = if length x == c then liftS (prod :-) (f §- (c,xs'))
               else fromS NullS
               where x    = fromStream $ takeS c $ toS xs
                     xs'  = liftS (dropS c) xs
                     prod = f x

(-§-) :: SignalSDF ([a] -> b) -> (Int, SignalSDF a) -> SignalSDF b
fs -§- (c, xs) 
    | isNull $ toS fs = fromS NullS
    | otherwise       = if length x == c then liftS (prod :-) (fs' -§- (c,xs'))
                        else fromS NullS
                        where x    = fromStream $ takeS c $ toS xs
                              xs'  = liftS (dropS c) xs
                              prod = (headS $ toS fs) x
                              fs'  = liftS tailS fs


