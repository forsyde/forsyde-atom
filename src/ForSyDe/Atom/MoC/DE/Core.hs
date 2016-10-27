{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE.Core
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the core semantics of the DE MoC.
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE.Core where

import ForSyDe.Atom.MoC hiding (Sig)
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility
import Numeric.Natural

-- | Type alias for timestamps. They are natural numbers to ensure /t/ &#8805; 0.
type Tag = Natural

-- | Type synonym for a DE event, i.e. "a constructed event of
-- partitioned extended values, where the partition size is always 1"
type Event a = DE ([Value a])

-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Sig a   = S.Signal (Event a)

-- | The DE event. It identifies a discrete event signal.
data DE a  = DE { tag :: Tag,  -- ^ timestamp
                  val :: a     -- ^ the value
                }

-- | Implenents the execution and synchronization semantics for the DE
-- MoC through its atoms.
instance MoC DE where
  type Context DE = ()
  ---------------------
  (-$-) (_,f) = (fmap . fmap) f
  ---------------------
  sf -*- sx = init ue (extractFunction <$> sf) sx
    where init px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> f  <*> ue :- comb f  x  s1 xs
          init _ _ NullS            = NullS
          init _ NullS _            = NullS
          comb pf px s1@(f :- fs) s2@(x :- xs)
            | tag f == tag x        = f %> f  <*> x  :- comb f  x  fs xs
            | tag f <  tag x        = f %> f  <*> px :- comb f  px fs s2
            | tag f >  tag x        = x %> pf <*> x  :- comb pf x  s1 xs
          comb _ px (f :- fs) NullS = f %> f  <*> px :- comb f  px fs NullS
          comb pf _ NullS (x :- xs) = x %> pf <*> x  :- comb pf x  NullS xs
          comb _ _ NullS NullS      = NullS
  ---------------------
  (DE _ v) ->- xs = pure v :- xs
  ---------------------
  (DE t _) -&- xs = (\(DE t1 v) -> DE (t1 + t) v) <$> xs
  ---------------------
  -- sniff (DE _ a) = a

-- | Shows the event with tag @t@ and value @v@ as @ v \@t@. It hides the partition (the singleton list constructor).
instance Show a => Show (DE [a]) where
  showsPrec _ (DE t [x]) = (++) ( " " ++ show x ++ " @" ++ show t )

-- | Reads the string for a normal tuple @(Tag,Value a)@ as an event @DE a@, already partitioned.
instance (Read a) => Read (DE [a]) where
  readsPrec _ x     = [(DE t [v], x) | ((t,v), x) <- reads x]


-- -- | A more efficient instatiation since we /know/ that the partition
-- -- size is always 1.
-- instance Eq a => Eq (DE [a]) where
--   DE t1 [a] == DE t2 [b] = t1 == t2 && a == b 

-- | Defines the equality operator between two DE signals.
instance Eq a => Eq (Signal (DE [a])) where
  a == b = flatten a == flatten b
    where flatten = concat . map (\x -> ((,) (tag x)) <$> val x) . fromSignal

-- | Allows for mapping of functions on a DE event.
instance Functor (DE) where
  fmap f (DE t a) = DE t (f a)

-- | Allows for lifting functions on a pair of DE events.
instance Applicative (DE) where
  pure = DE 0
  (DE tf f) <*> (DE _ x) = DE tf (f x)

-----------------------------------------------------------------------------
-- These functions are not exported and are used for testing purpose only.

part :: (Tag, a) -> DE [a]
part (t,v) = DE t (pure v)

stream :: [(Tag, a)] -> Signal (DE [a])
stream l = S.signal (part <$> l)


infixl 7 %>
(DE t _) %> (DE _ x) = DE t x
ue = DE 0 [Undef]

extractFunction (DE t (_,f)) = DE t f

-- end of testbench functions
-----------------------------------------------------------------------------


-- -- | Wraps a tuple @(tag, value)@ into a DE event of extended values
event  :: (Tag, a) -> Event a 
event (t,v) = part (t, pure v)


-- | Wraps a (tuple of) pair(s) @(tag, value)@ into the equivalent
-- event container(s).
--
-- "ForSyDe.Atom.MoC.DE" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > event, event2, event3, event4,
event2 = ($$) (event,event)
event3 = ($$$) (event,event,event)
event4 = ($$$$) (event,event,event,event)

-- | Transforms a list of tuples such as the ones taken by 'event'
-- into a DE signal
signal   :: [(Tag, a)] -> Sig a
signal l = S.signal (event <$> l)

----------------------------------------------------------------------------- 

-- | Wraps a function on extended values into the format needed by the
-- MoC atoms.
--
-- <<includes/figs/timed-wrapper-formula1.png>>
--
-- "ForSyDe.Atom.MoC.DE" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > wrap11, wrap21, wrap31, wrap41, wrap51, wrap61, wrap71, wrap81, 
-- > wrap12, wrap22, wrap32, wrap42, wrap52, wrap62, wrap72, wrap82, 
-- > wrap13, wrap23, wrap33, wrap43, wrap53, wrap63, wrap73, wrap83, 
-- > wrap14, wrap24, wrap34, wrap44, wrap54, wrap64, wrap74, wrap84,
wrap22 :: (Value a1 -> Value a2 -> (Value b1, Value b2))
       -> ((), [Value a1] -> ((), [Value a2] -> ([Value b1], [Value b2])))

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

    -- where init (DE ptx px) s1@(DE tf f :- fs) s2@(DE tx x :- xs)
    --         | tf == tx = DE tf (f  x) :- comb (DE tf f) (DE  tx  x) fs xs
    --         | tf <  tx = DE tf (f px) :- comb (DE tf f) (DE ptx px) fs s2
    --         | tf >  tx = DE tx (f  Undef) :- init (DE tx x) s1 xs
    --       comb (DE ptf pf) (DE ptx px) s1@(DE tf f :- fs) s2@(DE tx x :- xs)
    --         | tf == tx = DE tf ( f  x) :- comb (DE  tf  f) (DE  tx  x) fs xs
    --         | tf <  tx = DE tf ( f px) :- comb (DE  tf  f) (DE ptx px) fs s2
    --         | tf >  tx = DE tx (pf  x) :- comb (DE ptf pf) (DE  tx  x) s1 xs
    --       comb _ (DE ptx px) (DE tf f :- fs) NullS
    --         = DE tf (f px) :- comb (DE tf f) (DE ptx px) fs NullS
    --       comb (DE ptf pf) _ NullS (DE tx x :- xs)
    --         = DE tx (pf x) :- comb (DE ptf pf) (DE tx x) NullS xs
    --       comb _ _ NullS NullS = NullS
