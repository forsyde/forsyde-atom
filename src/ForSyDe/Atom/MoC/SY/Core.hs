{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SY.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the SY atoms
-- 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.SY.Core where

import ForSyDe.Atom.Behavior
import ForSyDe.Atom.MoC hiding (Sig)
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Utility

-- | Type synonym for a SY event, i.e. "a constructed event of
-- partitioned extended values, where the partition size is always 1"
type Event a = SY ([Value a])

-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Sig a   = S.Signal (Event a)


-- | The SY event. It identifies a synchronous signal.
newtype SY a  = SY { val :: a }
  
-- | Implenents the execution and synchronization semantics for the SY
-- MoC through its atoms.
instance MoC SY where
  type Context SY = ()
  ---------------------
  (-$-) (_,f) = (fmap . fmap) f
  ---------------------
  (-*-) fs = (<*>) (fmap (\f x -> f <*> x) (extractFunction <$> fs))
  ---------------------
  (->-) = (:-) 
  ---------------------
  (-&-) _ a = a
  ---------------------
  -- sniff (SY a) = a

-- | 'Show' instance. Hides the partition of values (the list wrapper).
instance (Show a) => Show (SY [a]) where
  showsPrec p = showParen (p>1) . showSignal . val
    where
      showSignal (x : xs)  = showEvent x . showSignal' xs
      showSignal ([])      = showChar ' ' . showChar '\b'
      showSignal' (x : xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' ([])     = showChar ' ' . showChar '\b'
      showEvent x          = shows x

-- | Reads the extended values and wraps them in a partitioned SY event, with the partition size of 1.
instance Read a => Read (SY [a]) where
  readsPrec _ s       = [(SY [x], r) | (x, r) <- reads s]

-- | Efficient implementation since we /know/ that the partition is 1.
instance Eq a => Eq (SY [a]) where
  SY [a] == SY [b] = a == b

-- | Defines the equality operator between two SY signals.
instance Eq a => Eq (Signal (SY [a])) where
  a == b = flatten a == flatten b
    where flatten = concat . map val . fromSignal

-- | Allows for mapping of functions on a SY event.
instance Functor (SY) where
  fmap f (SY a) = SY (f a)

-- | Allows for lifting functions on a pair of SY events.
instance Applicative (SY) where
  pure = SY 
  (SY a) <*> (SY b) = SY (a b)

-----------------------------------------------------------------------------
-- These functions are not exported and are used for testing purpose only.

part :: a -> SY [a]
part = SY . pure

stream :: [a] -> Signal (SY [a])
stream l = S.signal (part <$> l)

extractFunction (SY (_,f)) = SY f

-- end of testbench functions
-----------------------------------------------------------------------------

event  :: a -> Event a
event  = part . pure

-- | Wraps a (tuple of) value(s) into the equivalent event form.
--
-- "ForSyDe.Atom.MoC.SY" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > event, event2, event3, event4,
event2 = ($$) (event, event)
event3 = ($$$) (event, event, event)
event4 = ($$$$) (event, event, event, event)

-- | Transforms a list of values into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal (event <$> l)

-----------------------------------------------------------------------------
-- | Wraps a function on extended values into the format needed by the
-- MoC atoms.
--
-- <<includes/figs/timed-wrapper-formula1.png>>
--
-- "ForSyDe.Atom.MoC.SY" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > wrap11, wrap21, wrap31, wrap41, wrap51, wrap61, wrap71, wrap81, 
-- > wrap12, wrap22, wrap32, wrap42, wrap52, wrap62, wrap72, wrap82, 
-- > wrap13, wrap23, wrap33, wrap43, wrap53, wrap63, wrap73, wrap83, 
-- > wrap14, wrap24, wrap34, wrap44, wrap54, wrap64, wrap74, wrap84,
wrap22 :: (Value a1 -> Value a2 -> (Value b1, Value b2))
       -> ((), [Value a1] -> ((), [Value a2] -> ([Value b1], [Value b2])))

wrap :: ([a] -> b) -> ((), [a] -> b)
wrap f = ((), \x -> f x)

wrap11 f = wrap $ (map f)
wrap21 f = wrap $ wrap11 . f . head 
wrap31 f = wrap $ wrap21 . f . head
wrap41 f = wrap $ wrap31 . f . head
wrap51 f = wrap $ wrap41 . f . head
wrap61 f = wrap $ wrap51 . f . head
wrap71 f = wrap $ wrap61 . f . head
wrap81 f = wrap $ wrap71 . f . head

wrap12 f = wrap $ ((|<) . map f)
wrap22 f = wrap $ wrap12 . f . head 
wrap32 f = wrap $ wrap22 . f . head
wrap42 f = wrap $ wrap32 . f . head
wrap52 f = wrap $ wrap42 . f . head
wrap62 f = wrap $ wrap52 . f . head
wrap72 f = wrap $ wrap62 . f . head
wrap82 f = wrap $ wrap72 . f . head

wrap13 f = wrap $ ((|<<) . map f)
wrap23 f = wrap $ wrap13 . f . head 
wrap33 f = wrap $ wrap23 . f . head
wrap43 f = wrap $ wrap33 . f . head
wrap53 f = wrap $ wrap43 . f . head
wrap63 f = wrap $ wrap53 . f . head
wrap73 f = wrap $ wrap63 . f . head
wrap83 f = wrap $ wrap73 . f . head

wrap14 f = wrap $ ((|<<<) . map f)
wrap24 f = wrap $ wrap14 . f . head 
wrap34 f = wrap $ wrap24 . f . head
wrap44 f = wrap $ wrap34 . f . head
wrap54 f = wrap $ wrap44 . f . head
wrap64 f = wrap $ wrap54 . f . head
wrap74 f = wrap $ wrap64 . f . head
wrap84 f = wrap $ wrap74 . f . head
