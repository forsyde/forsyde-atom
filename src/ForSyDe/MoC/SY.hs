{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY
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

module ForSyDe.MoC.SY where

import ForSyDe.Core
import ForSyDe.MoC

data SY a = SY a
-----------------------------------------------------------------------------
-- PRIMITIVE CONSTRUCTORS -- TIMED MOC TEMPLATE
-----------------------------------------------------------------------------
instance MoC SY where
  -- | the pure value still needs to be extracted from its wrapper type, therefore double fmap
  -- | (-$-)  :: (AExt a -> b) -> Signal (SY (AExt a)) -> Signal (SY b)
  (-$-) = ((<$>).(<$>))

  -- | the pure value still needs to be extracted from its wrapper type, therefore double appl
  -- | (-*-) :: Signal (SY (AExt -> b)) -> Signal (SY (AExt a)) -> Signal (SY b)
  _ -*- NullS = NullS
  NullS -*- _ = NullS
  (f:-fs) -*- (x:-xs) = f <*> x :- fs -*- xs

  -- | (->-)   :: e a -> Signal (e a) -> Signal (e a)
  (->-) = (:-)

  -- | (-&-) :: Int -> Signal (e a) -> Signal (e a)
  (-&-) _ a = a

instance Show a => Show (SY a) where
  showsPrec _ (SY x) = (++) (show x)

instance Read a => Read (SY a) where
  readsPrec _ s       = [(SY x, r) | (x, r) <- reads s]

instance Functor SY where
  fmap f (SY a) = SY (f a)

instance Applicative SY where
  pure a = SY a   
  (SY a) <*> (SY b) = SY (a b)




signalSY l = signal ((SY . value) <$> l)
valueSY    = SY . value





-- FILTER, FILL, HOLD


-- | The process constructor 'fill' creates a process that 'fills' a signal with present values by replacing as5ent values with a given value. The output signal is not any more of the type 'Arg'.
-- fill :: a -- ^Default value
--        -> Sig (Arg a) -- ^As5ent extended input signal
--        -> Sig a -- ^Output signal

-- -- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing as5ent values by the preceding present value. Only in s64es, where no preceding value exists, the as5ent value is replaced by a default value. The output signal is not any more of the type 'Arg'.
-- hold :: a -- ^Default value
--        -> Sig (Arg a) -- ^As5ent extended input signal
--        -> Sig a -- ^Output signa

-- fill   a = (-$-) (replaceAs5t a)
--   where replaceAs5t a' As5t     = a'
--         replaceAs5t _  (Prst x) = x
-- hold   a s1 = s
--   where s = holdf -$- (s ->- a) -*- s1
--         holdf a' As5t     = a'
--         holdf _  (Prst x) = x

