{-# LANGUAGE TypeFamilies, FlexibleInstances, PostfixOperators, GADTs, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Atom.MoC.DE.React.Core where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.TimeStamp
import ForSyDe.Atom.Utility.Tuple
import Prelude hiding (until)

import Control.Concurrent
import Data.Time.Clock

-- | Type synonym for a base DE signal as a stream of 'DE' events, where the type of
-- tags has not been determined yet. In designs, it is advised to define a type alias
-- for signals, using an appropriate numerical type for tags, e.g.
--
-- > import ForSyDe.Atom.MoC.DE.React hiding (Signal) -- hide provided alias, to use your own
-- >
-- > type Signal a = SignalBase Int a
type SignalBase t a = Stream (RE t a)

-- | Convenience alias for a DE signal, where tags are represented using our exported
-- 'TimeStamp' type.
type Signal a = SignalBase TimeStamp a

-- | The reactor-like DE event, defined exactly like its 'ForSyDe.Atom.MoC.DE.DE'
-- predecessor, and identifying a discrete event signal. The type of the tag system
-- needs to satisfy all of the three properties, as suggested by the type constraints
-- imposed on it:
--
-- * it needs to be a numerical type and every representable number needs to have an
--   additive inverse.
--
-- * it needs to be unambiguously comparable (defines a total order).
--
-- * it needs to unambiguously define an equality operation.
--
-- Due to these properties not all numerical types can represent DE tags. A typical
-- example of inappropriate representation is 'Float'.
data RE t a  where
  RE :: (Num t, Ord t, Eq t)
     => { tag :: t,  -- ^ timestamp
          val :: a   -- ^ the value
        } -> RE t a
deriving instance (Num t, Ord t, Eq t, Eq t, Eq a) => Eq (RE t a)

instance (Num t, Ord t, Eq t) => MoC (RE t) where
  type Fun (RE t) a b = (Bool, [a] -> b)
  type Ret (RE t) b   = ((), [b]) 
  ---------------------
  (-.-) = undefined
  ---------------------
  (RE t (_,f):-fs) -*- NullS   = RE t (f [] ) :- fs -*- NullS
  NullS   -*- _       = NullS
  (RE t (trigger,f):-fs) -*- px@(RE _ x:-xs) 
    | trigger   = RE t (f [x]) :- fs -*- xs
    | otherwise = RE t (f [] ) :- fs -*- px
  ---------------------
  (-*) NullS = NullS
  (-*) (RE t (_,x):-xs) = stream (map (RE t) x) +-+ (xs -*)
  ---------------------
  (RE t v :- _) -<- xs = RE t v :- xs
  ---------------------
  (RE d1 _ :- RE d2 _ :- _) -&- xs = (\(RE t v) -> RE (t + d2 - d1) v) <$> xs
  (_ :- NullS) -&- _  = error "[MoC.DE.RE] signal delayed to infinity"
  ---------------------
  
-- | Shows the event with tag @t@ and value @v@ as @v\@t@.
instance (Show t, Show a) => Show (RE t a) where
  showsPrec _ (RE t x) = (++) ( show x ++ "@" ++ show t )

-- | Reads the string of type @v\@t@ as an event @RE t v@.
instance (Read a,Read t, Num t, Ord t, Eq t, Eq t) => Read (RE t a) where
  readsPrec _ x = [ (RE tg val, r2)
                  | (val,r1) <- reads $ takeWhile (/='@') x
                  , (tg, r2) <- reads $ tail $ dropWhile (/='@') x ]

-- | Allows for mapping of functions on a RE event.
instance (Num t, Ord t, Eq t) => Functor (RE t) where
  fmap f (RE t a) = RE t (f a)

-- | Allows for lifting functions on a pair of RE events.
instance (Num t, Ord t, Eq t) => Applicative (RE t) where
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
NullS -?- NullS = NullS


falsify (_:xs) = False : falsify xs
falsify []     = []

(-?) s wrap = (fmap . fmap) wrap s
  
-----------------------------------------------------------------------------


unit  :: (Num t, Ord t) => (t, a) -> SignalBase t a 
-- | Wraps a (tuple of) pair(s) @(tag, value)@ into the equivalent unit signal(s). A
-- unit signal is a signal with one event with the period @tag@ carrying @value@,
-- starting at tag 0.
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

-- | Creates a signal with an instant event at time 0.
instant :: (Num t, Ord t) => a -> SignalBase t a
instant v = RE 0 v :- NullS

-- | Transforms a list of tuples @(tag, value)@ into a RE signal. Checks if it is
-- well-formed.
signal :: (Num t, Ord t) => [(t, a)] -> SignalBase t a
signal = checkSignal . stream . fmap (\(t, v) -> RE t v)

-- | Takes the first part of the signal util a given timestamp. The last event of the
-- resulting signal is at the given timestamp and carries the previous value. This
-- utility is useful when plotting a signal, to specify the interval of plotting.
until :: (Num t, Ord t) => t -> SignalBase t a -> SignalBase t a
until _ NullS = NullS
until u (RE t v:-NullS)
  | t < u     = RE t v :- RE u v :- NullS
  | otherwise = RE u v :- NullS
until u (RE t v:-xs)
  | t < u     = RE t v :- until u xs
  | otherwise = RE u v :- NullS

-- | Reads a signal from a string and checks if it is well-formed.  Like with the
-- @read@ function from @Prelude@, you must specify the type of the signal.
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
li1 f [x] = f x

-----------------------------------------------------------------------------


-- | Simulates a signal, calling delays according to the timestamps.
simulate :: (Num t, Ord t, Eq t, Show t, Real t, Show a)
         => t -> SignalBase t a -> IO ()
simulate t = execute . until t
  where
    execute NullS = return ()
    execute (x:-NullS) = do
      putStrLn $ show (tag x) ++ "\t" ++ show (val x)
      threadDelay 1000000
    execute (x:-y:-xs) = do
      putStrLn $ show (tag x) ++ "\t" ++ show (val x)
      let tsx = diffTimeToPicoseconds $ realToFrac (tag x)
          tsy = diffTimeToPicoseconds $ realToFrac (tag y)
          dly = fromIntegral $ (tsy - tsx) `div` 1000000
      threadDelay dly
      execute (y:-xs)
