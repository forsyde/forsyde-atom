{-# LANGUAGE TypeFamilies, FlexibleInstances, PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Atom.MoC.DE.React.Core where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Utility.Tuple
import Prelude hiding (until)

type SignalBase t a = Stream (RE t a)
type Signal a = SignalBase TimeStamp a

-- | The DE event. It identifies a discrete event signal.
data RE t a  = RE { tag :: t,  -- ^ timestamp
                    val :: a   -- ^ the value
                  } deriving (Eq)

instance Num t => MoC (RE t) where
  type Fun (RE t) a b = (Bool, [a] -> b)
  type Ret (RE t) b   = ((), [b]) 
  ---------------------
  (-.-) = undefined
  ---------------------
  _       -*- NullS   = NullS
  NullS   -*- _       = NullS
  (RE t (trigger,f):-fs) -*- px@(RE _ x:-xs) 
    | trigger   = RE t (f [x]) :- fs -*- xs
    | otherwise = RE t (f [] ) :- fs -*- px
  ---------------------
  (-*) NullS = NullS
  (-*) (RE t (_,x):-xs) = stream (map (RE t) x) +-+ (xs -*)
  ---------------------
  (RE _ v :- _) -<- xs = pure v :- xs
  ---------------------
  (_ :- RE d _ :- _) -&- xs = (\(RE t v) -> RE (t + d) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.DE.RE] signal delayed to infinity"
  ---------------------
  
-- | Shows the event with tag @t@ and value @v@ as @v\@t@.
instance (Show t, Show a) => Show (RE t a) where
  showsPrec _ (RE t x) = (++) ( show x ++ "@" ++ show t )

-- | Reads the string of type @v\@t@ as an event @RE t v@.
instance (Read t, Read a) => Read (RE t a) where
  readsPrec _ x = [ (RE tg val, r2)
                  | (val,r1) <- reads $ takeWhile (/='@') x
                  , (tg, r2) <- reads $ tail $ dropWhile (/='@') x ]

-- | Allows for mapping of functions on a RE event.
instance Num t => Functor (RE t) where
  fmap f (RE t a) = RE t (f a)

-- | Allows for lifting functions on a pair of RE events.
instance Num t => Applicative (RE t) where
  pure = RE 0
  (RE tf f) <*> (RE _ x) = RE tf (f x)

----------------------------------------------------------------------------- 
-- These functions are not exported and are used internally.

infixl 5 -?-
infixl 5 -?

detect :: (Ord t, Num t) => SignalBase t a -> SignalBase t [Bool]
detect = (fmap . fmap) (const [True])

(-?-) :: (Ord t, Num t)
      => SignalBase t [Bool] -> SignalBase t a -> SignalBase t [Bool]
pg@(RE tg g :- gs) -?- px@(RE tx x :- xs) 
  | tg == tx = RE tg (True:g)  :- gs -?- xs
  | tg <  tx = RE tg (False:g) :- gs -?- px
  | tg >  tx = RE tx (True:falsify g) :- pg -?- xs
pg@(RE tg g :- gs) -?- NullS = RE tg (False:g) :- gs -?- NullS
NullS -?- px@(RE tx x :- xs) = RE tx (True:repeat False) :- NullS -?- xs

falsify (_:xs) = False : falsify xs
falsify []     = []

(-?) s wrap = (fmap . fmap) wrap s
  
-----------------------------------------------------------------------------


unit  :: (Num t, Ord t) => (t, a) -> SignalBase t a 
-- | Wraps a (tuple of) pair(s) @(tag, value)@ into the equivalent
-- unit signal(s). A unit signal is a signal with one event with the
-- period @tag@ carrying @value@.
--
-- Helpers: @unit@ and @unit[2-4]@.
unit2 :: (Num t, Ord t)
      => ((t,a1),(t, a2))
      -> (SignalBase t a1, SignalBase t a2)
unit3 :: (Num t, Ord t)
      => ((t,a1),(t, a2),(t, a3))
      -> (SignalBase t a1, SignalBase t a2, SignalBase t a3)
unit4 :: (Num t, Ord t)
      => ((t,a1),(t, a2),(t, a3),(t, a4))
      -> (SignalBase t a1, SignalBase t a2, SignalBase t a3, SignalBase t a4)

unit (t,v) = (RE 0 v :- RE t v :- NullS)
unit2 = ($$) (unit,unit)
unit3 = ($$$) (unit,unit,unit)
unit4 = ($$$$) (unit,unit,unit,unit)

-- | Creates an infinite signal.
infinite :: (Num t, Ord t) => a -> SignalBase t a
infinite v = RE 0 v :- NullS

-- | Transforms a list of tuples @(tag, value)@ into a RE
-- signal. Checks if it is well-formed.
signal :: (Num t, Ord t) => [(t, a)] -> SignalBase t a
signal = checkSignal . stream . fmap (\(t, v) -> RE t v)

-- | Takes the first part of the signal util a given timestamp. The
-- last event of the resulting signal is at the given timestamp and
-- carries the previous value. This utility is useful when plotting
-- a signal, to specify the interval of plotting.
until :: (Num t, Ord t) => t -> SignalBase t a -> SignalBase t a
until _ NullS = NullS
until u (RE t v:-NullS)
  | t < u     = RE t v :- RE u v :- NullS
  | otherwise = RE u v :- NullS
until u (RE t v:-xs)
  | t < u     = RE t v :- until u xs
  | otherwise = RE u v :- NullS

-- | Reads a signal from a string and checks if it is well-formed.
-- Like with the @read@ function from @Prelude@, you must specify the
-- type of the signal.
--
-- >>> readSignal "{ 1@0, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- {1@0s,2@2s,3@5s,4@7s,5@10s}
-- >>> readSignal "{ 1@1, 2@2, 3@5, 4@7, 5@10 }" :: Signal Int
-- {1@1s,2@2s,3@5s,4@7s,5@10s}
--
-- Incorrect usage (not covered by @doctest@):
--
-- > λ> readSignal "{ 1@0, 2@2, 3@5, 4@10, 5@7 }" :: Signal Int
-- > {1@0s,2@2s,3@5s*** Exception: [MoC.RE] malformed signal
readSignal :: (Num t, Ord t, Read t, Read a) => String -> SignalBase t a
readSignal s = checkSignal $ read s

-- | Checks if a signal is well-formed or not, according to the RE MoC
-- interpretation in ForSyDe-Atom.
checkSignal NullS      = NullS
checkSignal (x:-NullS) = (x:-NullS)
checkSignal (x:-y:-xs) | tag x < tag y = x :-checkSignal (y:-xs)
                       | otherwise = error "[MoC.DE.RE] malformed signal"

-----------------------------------------------------------------------------

fromList1 (a1:_)                      = a1
fromList2 (a1:a2:_)                   = (a2,a1)
fromList3 (a1:a2:a3:_)                = (a3,a2,a1)
fromList4 (a1:a2:a3:a4:_)             = (a4,a3,a2,a1)
fromList5 (a1:a2:a3:a4:a5:_)          = (a5,a4,a3,a2,a1)
fromList6 (a1:a2:a3:a4:a5:a6:_)       = (a6,a5,a4,a3,a2,a1)
fromList7 (a1:a2:a3:a4:a5:a6:a7:_)    = (a7,a6,a5,a4,a3,a2,a1)
fromList8 (a1:a2:a3:a4:a5:a6:a7:a8:_) = (a8,a7,a6,a5,a4,a3,a2,a1)

li101 f [s1]                    = (:[]) $ f s1
li201 f [s1] [s2]               = (:[]) $ f s1 s2
li301 f [s1] [s2] [s3]          = (:[]) $ f s1 s2 s3
li111 f [s1] a1                 = (:[]) $ f s1 a1
li211 f [s1] [s2] a1            = (:[]) $ f s1 s2 a1
li311 f [s1] [s2] [s3] a1       = (:[]) $ f s1 s2 s3 a1
li121 f [s1] a1 a2              = (:[]) $ f s1 a1 a2
li221 f [s1] [s2] a1 a2         = (:[]) $ f s1 s2 a1 a2
li321 f [s1] [s2] [s3] a1 a2    = (:[]) $ f s1 s2 s3 a1 a2
li131 f [s1] a1 a2 a3           = (:[]) $ f s1 a1 a2 a3
li231 f [s1] [s2] a1 a2 a3      = (:[]) $ f s1 s2 a1 a2 a3
li331 f [s1] [s2] [s3] a1 a2 a3 = (:[]) $ f s1 s2 s3 a1 a2 a3
li102 f [s1]                    = ((:[]),(:[])) $$ f s1
li202 f [s1] [s2]               = ((:[]),(:[])) $$ f s1 s2
li302 f [s1] [s2] [s3]          = ((:[]),(:[])) $$ f s1 s2 s3
li112 f [s1] a1                 = ((:[]),(:[])) $$ f s1 a1
li212 f [s1] [s2] a1            = ((:[]),(:[])) $$ f s1 s2 a1
li312 f [s1] [s2] [s3] a1       = ((:[]),(:[])) $$ f s1 s2 s3 a1
li122 f [s1] a1 a2              = ((:[]),(:[])) $$ f s1 a1 a2
li222 f [s1] [s2] a1 a2         = ((:[]),(:[])) $$ f s1 s2 a1 a2
li322 f [s1] [s2] [s3] a1 a2    = ((:[]),(:[])) $$ f s1 s2 s3 a1 a2
li132 f [s1] a1 a2 a3           = ((:[]),(:[])) $$ f s1 a1 a2 a3
li232 f [s1] [s2] a1 a2 a3      = ((:[]),(:[])) $$ f s1 s2 a1 a2 a3
li332 f [s1] [s2] [s3] a1 a2 a3 = ((:[]),(:[])) $$ f s1 s2 s3 a1 a2 a3
li103 f [s1]                    = ((:[]),(:[]),(:[])) $$$ f s1
li203 f [s1] [s2]               = ((:[]),(:[]),(:[])) $$$ f s1 s2
li303 f [s1] [s2] [s3]          = ((:[]),(:[]),(:[])) $$$ f s1 s2 s3
li113 f [s1] a1                 = ((:[]),(:[]),(:[])) $$$ f s1 a1
li213 f [s1] [s2] a1            = ((:[]),(:[]),(:[])) $$$ f s1 s2 a1
li313 f [s1] [s2] [s3] a1       = ((:[]),(:[]),(:[])) $$$ f s1 s2 s3 a1
li123 f [s1] a1 a2              = ((:[]),(:[]),(:[])) $$$ f s1 a1 a2
li223 f [s1] [s2] a1 a2         = ((:[]),(:[]),(:[])) $$$ f s1 s2 a1 a2
li323 f [s1] [s2] [s3] a1 a2    = ((:[]),(:[]),(:[])) $$$ f s1 s2 s3 a1 a2
li133 f [s1] a1 a2 a3           = ((:[]),(:[]),(:[])) $$$ f s1 a1 a2 a3
li233 f [s1] [s2] a1 a2 a3      = ((:[]),(:[]),(:[])) $$$ f s1 s2 a1 a2 a3
li333 f [s1] [s2] [s3] a1 a2 a3 = ((:[]),(:[]),(:[])) $$$ f s1 s2 s3 a1 a2 a3
li1 f [x] = f x

-----------------------------------------------------------------------------

