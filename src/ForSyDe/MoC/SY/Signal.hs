{-# LANGUAGE TypeFamilies #-}
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

module ForSyDe.MoC.SY.Signal (
  SignalSY (..),
  -- ** Interface functions
  signalSY, fromSignalSY,
) where

import ForSyDe.Core

-- | The type 'SignalSY' denotes a 'Stream' that behaves according to the Sychronous MoC. 
newtype SignalSY a = SignalSY { fromSY :: Stream a }

-- | Only 'Show' instance. To 'Read' it, use the 'Stream' instance.
instance (Show a) => Show (SignalSY a) where
  showsPrec p = showsPrec p . fromSY

instance Functor SignalSY where
  fmap f = SignalSY . fmapSY f . fromSY
    where fmapSY _ NullS    = NullS
          fmapSY f (x:-xs) = f x :- fmapSY f xs

instance Applicative SignalSY where
  pure a  = SignalSY (repeatS a)
  a <*> b = SignalSY $ starSY (fromSY a) (fromSY b)
    where starSY _         NullS     = NullS
          starSY NullS     _         = NullS
          starSY (f :- fs) (x :- xs) = f x :- starSY fs xs

-- | instance defining the Synchronous MoC behavior
instance Signal SignalSY where
  -- | To abide the synchronicicy law, a data-filtered signal must replace missing tokens with 
  --   absent values, thus the filtered type is 'AbstExt'
  type Filtered SignalSY a = AbstExt a 
  type Padded SignalSY a = a 
  --------
  toS   = fromSY
  fromS = SignalSY
  --------
  (§-)  = (<$>)
  (-§-) = (<*>)
  --------
  xs -#- p = fmap (\x -> if p x then Prst x else Abst) xs
  --------
  safe xs = xs
  --------
  zipx NullV = pure NullV
  zipx (x:>xs) =  (:>) §- x -§- zipx xs
  --------
  unzipx = (§>) fromS . unzx . toS
    where unzx NullS     = pure NullS
          unzx (x :- xs) = (:-) §> x <§> unzx xs
  --------

-- | converts a list directly to a SY signal.
signalSY :: [a] -> SignalSY a 
signalSY = SignalSY . stream 


-- | converts a SY signal into a list.
fromSignalSY :: SignalSY a -> [a]
fromSignalSY = fromStream . fromSY

