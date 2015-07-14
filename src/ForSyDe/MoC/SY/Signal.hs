{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY.Signal  where

import ForSyDe.Core


newtype SignalSY a = SignalSY { fromSY :: Signal a }

instance (Show a) => Show (SignalSY a) where
  showsPrec p = showsPrec p . fromSY

-- | provides 'fmap'
instance Functor SignalSY where
  fmap f = SignalSY . fmapSY f . fromSY
    where fmapSY _ NullS    = NullS
          fmapSY f (x:-xs) = f x :- fmapSY f xs

-- | provides 'pure', '<*>', '<$>'
instance Applicative SignalSY where
  pure a  = SignalSY (a :- NullS)
  a <*> b = SignalSY $ starSY (fromSY a) (fromSY b)
    where starSY _         NullS     = NullS
          starSY NullS     _         = NullS
          starSY (f :- fs) (x :- xs) = f x :- starSY fs xs

-- | 'Signals' instance for a  signal
instance Signals SignalSY where
  type Filtered SignalSY a = AbstExt a 
  toS   = fromSY
  fromS = SignalSY
  --------
  xs -#- p = fmap (\x -> if p x then Prst x else Abst) xs

signalSY :: [a] -> SignalSY a 
signalSY = SignalSY . signal 

fromSignalSY :: SignalSY a -> [a]
fromSignalSY = fromSignal . fromSY

