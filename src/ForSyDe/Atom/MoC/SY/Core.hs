{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
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
import ForSyDe.Atom.MoC
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Utility

-- | Type alias for a SY signal
type Event a = SY ([Value a])
type Sig a   = S.Signal (Event a)


-- | The SY event. It identifies a synchronous signal and implemets an
-- instance of the 'MoC' class. Since SY tags are implicit, this data
-- type only wraps values inside a constructor that identifies it as a
-- "synchronous event".
newtype SY a  = SY { fromSY :: a }
-----------------------------------------------------------------------------

  
-- | Implenents the SY semantics for the MoC atoms.
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
  fromEvent = fromSY

-- | Shows the (extended) value wrapped
-- instance Show a => Show (SY a) where
--   showsPrec _ (SY x) = (++) (show x)

-- | 'Show' instance. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (SY [a]) where
  showsPrec p = showParen (p>1) . showSignal . fromSY
    where
      showSignal (x : xs)  = showEvent x . showSignal' xs
      showSignal ([])      = showChar ' ' . showChar '\b'
      showSignal' (x : xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' ([])     = showChar ' ' . showChar '\b'
      showEvent x          = shows x

-- | Reads the (extended) value wrapped
instance Read a => Read (SY [a]) where
  readsPrec _ s       = [(SY [x], r) | (x, r) <- reads s]

instance Eq a => Eq (SY [a]) where
  SY [a] == SY [b] = a == b

-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor (SY) where
  fmap f (SY a) = SY (f a)

-- | Needed for the implementation of the '-*-' atom
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
event2 = ($$) (event, event)
event3 = ($$$) (event, event, event)
event4 = ($$$$) (event, event, event, event)

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal (event <$> l)

-----------------------------------------------------------------------------
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
