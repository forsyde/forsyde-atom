{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY.Signal where

import ForSyDe.Core

newtype Signal a = Signal { toS :: Stream (AbstExt a) }

fromS = Signal

liftS :: (Stream (AbstExt a) -> Stream (AbstExt b)) 
       -> Signal a -> Signal b
liftS f = fromS . f . toS

liftS2 :: (Stream (AbstExt a) -> Stream (AbstExt b) -> Stream (AbstExt c)) 
       -> Signal a -> Signal b -> Signal c
liftS2 f xs ys = fromS $ f (toS xs) (toS ys)


-- | Only 'Show' instance. To 'Read' it, use the 'Stream' instance.
instance (Show a) => Show (Signal a) where
  showsPrec p = showsPrec p . toS


-- | instance defining the Synchronous MoC behavior
instance Signals Signal where
 -- | To abide the synchronicicy law, a data-filtered signal must replace missing tokens with 
 --   absent values, thus the filtered type is 'AbstExt'
    type TokT Signal a = AbstExt a

 -- (§§-) :: (a -> b) -> Signal a -> Signal b
    (§§-) f = liftS (((<$>).(<$>)) f)

 -- (-§§-) :: Signal (a -> b) -> Signal a -> Signal b
    (-§§-) = liftS2 (\f x -> (<*>) <$> f <*> x)

 -- (->-) :: Signal a -> AbstExt a -> Signal a
    xs ->- i = liftS (i :-) xs

 -- filt :: (a -> Bool) -> Signal a -> Signal a 
    filt p = liftS ((\x -> if (p <$> x) == Prst True then x else Abst) <$>) 

 -- zipx :: Vector (Signal a) -> Signal (Vector (AbstExt a))
    zipx = zippx

 -- unzipx :: Signal (Vector (AbstExt a)) -> Vector (Signal a)
    unzipx  = (§>) fromS . unzippx . toS


ssignal :: [a] -> Signal a 
ssignal = signal . map Prst

signal :: [AbstExt a] -> Signal a 
signal = fromS . stream 

-- | The function 'fromSignal' converts a signal into a list.
fromSignal :: Signal a -> [AbstExt a]
fromSignal = fromStream . toS

zippx :: Vector (Signal a) -> Signal (Vector (AbstExt a))
zippx NullV   = (fromS . pure . pure) NullV
zippx (x:>xs) = tokenize $ ((<$>).(:>)) §- x -§- zippx xs

unzippx :: Stream (AbstExt (Vector (AbstExt a))) -> Vector (Stream (AbstExt a))
unzippx NullS            = pure NullS
unzippx (Abst :- xs)     = (:-) §> pure Abst <§> unzippx xs
unzippx ((Prst x) :- xs) = (:-) §> x <§> unzippx xs


infixl 4 §-, -§-,  §§!-, -§§!-
-- | operator for functional application on signals
(§-) :: (AbstExt a -> b) -> Signal a -> Stream b
(§-) f = (<$>) f . toS

(-§-) :: Stream (AbstExt a -> b) -> Signal a -> Stream b
(-§-) fs = (<*>) fs . toS

(§§!-) :: (a -> b) -> Signal a -> Signal b
f §§!- xs | anyS isAbsent $ toS xs = error "Unexpected absent value in szip"
          | otherwise         = f §§- xs

(-§§!-) :: Signal (a -> b) -> Signal a -> Signal b
fs -§§!- xs | anyS isAbsent $ toS xs = error "Unexpected absent value in szip"
            | otherwise         = fs -§§- xs


tokenize = fromS

