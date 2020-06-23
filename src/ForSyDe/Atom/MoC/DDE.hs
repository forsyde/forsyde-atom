{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module ForSyDe.Atom.MoC.DDE where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Utility (($$),($$$),($$$$))

import Prelude hiding (until)


-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Signal a   = Stream (DDE a)

-- | The DE event. It identifies a discrete event signal.
data DDE a  = DDE { tag :: TimeStamp,  -- ^ timestamp
                    val :: Msg a       -- ^ the value
                  } deriving (Eq)

data Msg a = M a | NM deriving (Eq)

nm = DDE 0 NM
dde t v = DDE t (M v)

isNullM (DDE _ NM) = True
isNullM _          = False

instance MoC DDE where
  type Fun DDE a b = a -> b
  type Ret DDE b   = b 
  ---------------------
  (-.-) = fmap . fmap
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (f:-fs) -*- (x:-xs) = f  <*> x  :- comb nm nm fs xs
    where
     comb pf px s1@(f :- fs) s2@(x :- xs)
       | tag f == tag x = let p = f %> f <*> x
                          in  p :- comb (next p f) (next p x) fs xs
       | tag f <  tag x = let p = f %> f  <*> px
                          in  p :- comb (next p f) nm fs s2
       | tag f >  tag x = let p = x %> pf <*> x
                          in  p :- comb nm (next p x) s1 xs
     comb _ _ _ NullS = NullS
     comb _ _ NullS _ = NullS
     next c x
       | isNullM c = x
       | otherwise = nm
  ---------------------
  (-*) = id
  ---------------------
  (DDE _ v :- _) -<- xs = (DDE 0 v) :- xs
  ---------------------
  (_ :- DDE d _ :- _) -&- xs = (\(DDE t v) -> DDE (t + d) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.DDE] signal delayed to infinity"
  ---------------------

-- | Shows the event with tag @t@ and value @v@ as @ v \@t@.
instance Show a => Show (DDE a) where
  showsPrec _ (DDE t (M x)) = (++) ( show x ++ "@" ++ show t )
  showsPrec _ (DDE t NM   ) = (++) ("**@" ++ show t )

-- | Reads the string of type @v\@t@ as an event @DDE t v@.
instance (Read a) => Read (DDE a) where
  readsPrec _ x = [ (DDE tg NM, r2)
                  | ("_",r1) <- lex $ takeWhile (/='@') x
                  , (tg, r2) <- reads $ tail $ dropWhile (/='@') x ]
                  ++ [ (DDE tg (M val), r2)
                  | (val,r1) <- reads $ takeWhile (/='@') x
                  , (tg, r2) <- reads $ tail $ dropWhile (/='@') x ]

instance Functor Msg where
  fmap _ NM    = NM
  fmap f (M a) = M (f a)

instance Applicative Msg where
  pure = M
  NM    <*> _     = NM
  _     <*> NM    = NM
  (M f) <*> (M a) = M (f a)

-- | Allows for mapping of functions on a DDE event.
instance Functor DDE where
  fmap f (DDE t a) = DDE t (f <$> a)

-- | Allows for lifting functions on a pair of DDE events.
instance Applicative DDE where
  pure = DDE 0 . M
  (DDE tf f) <*> (DDE _ x) = DDE tf (f <*> x)


unit  :: (TimeStamp, a) -> Signal a 
unit (t,v) = (pure v :- dde t v :- NullS)

-- | Wraps a (tuple of) pair(s) @(tag, value)@ into the equivalent
-- unit signal(s), in this case a signal with one event with the
-- period @tag@ carrying @value@.
--
-- The following helpers are exported:
--
-- > unit, unit2, unit3, unit4,
unit2 = ($$) (unit,unit)
unit3 = ($$$) (unit,unit,unit)
unit4 = ($$$$) (unit,unit,unit,unit)

-- | Creates an infinite signal.
infinite :: a -> Signal a
infinite v = pure v :- NullS

-- | Transforms a list of tuples @(tag, value)@ into a DDE
-- signal. Checks if it is well-formed.
signal :: [(TimeStamp, a)] -> Signal a
signal = checkSignal . stream . fmap (\(t, v) -> dde t v)

-- | Takes the first part of the signal util a given timestamp. The
-- last event of the resulting signal is at the given timestamp and
-- carries the previous value. This utility is useful when plotting
-- a signal, to specify the interval of plotting.
until :: TimeStamp -> Signal a -> Signal a
until _ NullS = NullS
until u (DDE t v:-NullS)
  | t < u     = DDE t v :- DDE u v :- NullS
  | otherwise = DDE u v :- NullS
until u (DDE t v:-xs)
  | t < u     = DDE t v :- until u xs
  | otherwise = DDE u v :- NullS

-- | Reads a signal from a string and checks if it is well-formed.
-- Like with the @read@ function from @Prelude@, you must specify the
-- type of the signal.
--
-- >>> readSignal "{ 1@0, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- { 1 @0s, 2 @2s, 3 @5s, 4 @7s, 5 @10s}
--
-- Incorrect usage (not covered by @doctest@):
--
-- > λ> readSignal "{ 1@0, 2@2, 3@5, 4@10, 5@7 }" :: Signal Int
-- > { 1 @0s, 2 @2s, 3 @5s*** Exception: [MoC.DDE] malformed signal
-- > λ> readSignal "{ 1@1, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- > *** Exception: [MoC.DDE] signal does not start from global 0
readSignal :: Read a => String -> Signal a
readSignal s = checkSignal $ read s

-- | Checks if a signal is well-formed or not, according to the DDE MoC
-- interpretation in @ForSyDe-Atom@.
checkSignal NullS = NullS
checkSignal s@(x:-_)
  | tag x == 0 = checkOrder s
  | otherwise  = error "[MoC.DDE] signal does not start from global 0"
  where
    checkOrder NullS      = NullS
    checkOrder (x:-NullS) = (x:-NullS)
    checkOrder (x:-y:-xs) | tag x < tag y = x :-checkOrder (y:-xs)
                          | otherwise = error "[MoC.DDE] malformed signal"

----------------------------------------------------------------------------- 
-- These functions are not exported and are used for testing purpose only.

infixl 7 %>
(DDE t _) %> (DDE _ x) = DDE t x

-- end of testbench functions
-----------------------------------------------------------------------------
