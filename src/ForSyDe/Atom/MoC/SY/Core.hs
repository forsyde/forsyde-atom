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
  (-$-) (_,f) = (,) () . (fmap . fmap) f
  ---------------------
  (-*-) (_,fs) = (,) () . (<*>) (fmap (\f x -> f <*> x) fs)
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

wrap11 :: (a1->b1)                             -> ((), [a1]->[b1])
wrap21 :: (a1->a2->b1)                         -> ((), [a1]->[a2]->[b1])
wrap31 :: (a1->a2->a3->b1)                     -> ((), [a1]->[a2]->[a3]->[b1])
wrap41 :: (a1->a2->a3->a4->b1)                 -> ((), [a1]->[a2]->[a3]->[a4]->[b1])
wrap51 :: (a1->a2->a3->a4->a5->b1)             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[b1])
wrap61 :: (a1->a2->a3->a4->a5->a6->b1)         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[b1])
wrap71 :: (a1->a2->a3->a4->a5->a6->a7->b1)     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[b1])
wrap81 :: (a1->a2->a3->a4->a5->a6->a7->a8->b1) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->[b1])

wrap12 :: (a1->(b1,b2))                             -> ((), [a1]->([b1],[b2]))
wrap22 :: (a1->a2->(b1,b2))                         -> ((), [a1]->[a2]->([b1],[b2]))
wrap32 :: (a1->a2->a3->(b1,b2))                     -> ((), [a1]->[a2]->[a3]->([b1],[b2]))
wrap42 :: (a1->a2->a3->a4->(b1,b2))                 -> ((), [a1]->[a2]->[a3]->[a4]->([b1],[b2]))
wrap52 :: (a1->a2->a3->a4->a5->(b1,b2))             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->([b1],[b2]))
wrap62 :: (a1->a2->a3->a4->a5->a6->(b1,b2))         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->([b1],[b2]))
wrap72 :: (a1->a2->a3->a4->a5->a6->a7->(b1,b2))     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->([b1],[b2]))
wrap82 :: (a1->a2->a3->a4->a5->a6->a7->a8->(b1,b2)) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->([b1],[b2]))

wrap13 :: (a1->(b1,b2,b3))                             -> ((), [a1]->([b1],[b2],[b3]))
wrap23 :: (a1->a2->(b1,b2,b3))                         -> ((), [a1]->[a2]->([b1],[b2],[b3]))
wrap33 :: (a1->a2->a3->(b1,b2,b3))                     -> ((), [a1]->[a2]->[a3]->([b1],[b2],[b3]))
wrap43 :: (a1->a2->a3->a4->(b1,b2,b3))                 -> ((), [a1]->[a2]->[a3]->[a4]->([b1],[b2],[b3]))
wrap53 :: (a1->a2->a3->a4->a5->(b1,b2,b3))             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->([b1],[b2],[b3]))
wrap63 :: (a1->a2->a3->a4->a5->a6->(b1,b2,b3))         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->([b1],[b2],[b3]))
wrap73 :: (a1->a2->a3->a4->a5->a6->a7->(b1,b2,b3))     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->([b1],[b2],[b3]))
wrap83 :: (a1->a2->a3->a4->a5->a6->a7->a8->(b1,b2,b3)) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->([b1],[b2],[b3]))

wrap14 :: (a1->(b1,b2,b3,b4))                             -> ((), [a1]->([b1],[b2],[b3],[b4]))
wrap24 :: (a1->a2->(b1,b2,b3,b4))                         -> ((), [a1]->[a2]->([b1],[b2],[b3],[b4]))
wrap34 :: (a1->a2->a3->(b1,b2,b3,b4))                     -> ((), [a1]->[a2]->[a3]->([b1],[b2],[b3],[b4]))
wrap44 :: (a1->a2->a3->a4->(b1,b2,b3,b4))                 -> ((), [a1]->[a2]->[a3]->[a4]->([b1],[b2],[b3],[b4]))
wrap54 :: (a1->a2->a3->a4->a5->(b1,b2,b3,b4))             -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->([b1],[b2],[b3],[b4]))
wrap64 :: (a1->a2->a3->a4->a5->a6->(b1,b2,b3,b4))         -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->([b1],[b2],[b3],[b4]))
wrap74 :: (a1->a2->a3->a4->a5->a6->a7->(b1,b2,b3,b4))     -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->([b1],[b2],[b3],[b4]))
wrap84 :: (a1->a2->a3->a4->a5->a6->a7->a8->(b1,b2,b3,b4)) -> ((), [a1]->[a2]->[a3]->[a4]->[a5]->[a6]->[a7]->[a8]->([b1],[b2],[b3],[b4]))

wrap11 = (,) () . psi11
wrap21 = (,) () . psi21
wrap31 = (,) () . psi31
wrap41 = (,) () . psi41
wrap51 = (,) () . psi51
wrap61 = (,) () . psi61
wrap71 = (,) () . psi71
wrap81 = (,) () . psi81
wrap12 = (,) () . psi12
wrap22 = (,) () . psi22
wrap32 = (,) () . psi32
wrap42 = (,) () . psi42
wrap52 = (,) () . psi52
wrap62 = (,) () . psi62
wrap72 = (,) () . psi72
wrap82 = (,) () . psi82
wrap13 = (,) () . psi13
wrap23 = (,) () . psi23
wrap33 = (,) () . psi33
wrap43 = (,) () . psi43
wrap53 = (,) () . psi53
wrap63 = (,) () . psi63
wrap73 = (,) () . psi73
wrap83 = (,) () . psi83
wrap14 = (,) () . psi14
wrap24 = (,) () . psi24
wrap34 = (,) () . psi34
wrap44 = (,) () . psi44
wrap54 = (,) () . psi54
wrap64 = (,) () . psi64
wrap74 = (,) () . psi74
wrap84 = (,) () . psi84

