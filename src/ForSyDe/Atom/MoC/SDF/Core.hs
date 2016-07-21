{-# LANGUAGE TypeFamilies, RankNTypes, PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SDF
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
module ForSyDe.Atom.MoC.SDF.Core where

import ForSyDe.Atom.MoC.Atom
import ForSyDe.Atom.MoC.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility (($$),($$$),($$$$), id1, id2, id3, id4)



import qualified Data.Param.FSVec as V
import Data.TypeLevel hiding ((==))

-- | Type alias for a CT signal
type Sig a   = S.Signal (Event a) 
type Event a = SDF (Value a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data SDF a = SDF a

instance Partitioned SDF where
  type Arg SDF rate a = (V.FSVec rate a)
  type Context SDF rate = (Nat rate)
  o = (S.signal . fmap SDF . concat . fmap V.fromVector . fmap fromSDF . S.fromSignal)

-- | Implenents the CT semantics for the MoC atoms
instance MoC SDF where
  ---------------------
  _ -$- NullS           = NullS
  f -$- xs = let x'  = extract c xs 
                 xs' = dropS (toInt c) xs
                 c   = V.lengthT x'
             in if toInt c == length (V.fromVector x')
                then SDF (f x') :- f -$- xs'
                else NullS
  ---------------------
  (SDF f :- fs) -*- xs = let x'  = extract c xs
                             xs' = dropS (toInt c) xs
                             c   = V.lengthT x'
                         in if toInt c == length (V.fromVector x')
                            then SDF (f x') :- fs -*- xs'
                            else NullS
  ---------------------
  (->-) = (+-+) . S.signal . fmap SDF . V.fromVector . fromSDF 
  ---------------------
  (-&-) _ a = a
  ---------------------

-- instance ContextFunctor SDF where
--   type Context SDF c    = (Nat c)
--   fmapC f (SDF a)       = SDF (f a)
--   liftC (SDF f) (SDF a) = SDF (f a)
  


-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor (SDF) where
  fmap f (SDF a) = SDF (f a)
 
-- | Shows the (extended) value wrapped
instance Show a => Show (SDF a) where
  showsPrec _ (SDF x) = (++) (show x)

-----------------------------------------------------------------------------

-- | Wraps a base value into a SY event of extended values

-- event     :: a         -> Event a
-- event2    :: (a,b)     -> (Event a,Event b)
-- event3    :: (a,b,c)   -> (Event a,Event b,Event c)
-- event4    :: (a,b,c,d) -> (Event a,Event b,Event c,Event d)

--event  :: a -> SY (SYArg c (Value a))

event  :: (Nat n1) => V.FSVec n1 a -> SDF (V.FSVec n1 (Value a))
event2 :: (Nat n1, Nat n2) => (V.FSVec n1 a, V.FSVec n2 b)
          -> (SDF (V.FSVec n1 (Value a)), SDF (V.FSVec n2 (Value b)))
event3 :: (Nat n1, Nat n2, Nat n3) => (V.FSVec n1 a, V.FSVec n2 b, V.FSVec n3 c)
          -> (SDF (V.FSVec n1 (Value a)), SDF (V.FSVec n2 (Value b)), SDF (V.FSVec n3 (Value c)))
event4 :: (Nat n1, Nat n2, Nat n3, Nat n4) => (V.FSVec n1 a, V.FSVec n2 b, V.FSVec n3 c, V.FSVec n4 d)
          -> (SDF (V.FSVec n1 (Value a)), SDF (V.FSVec n2 (Value b)), SDF (V.FSVec n3 (Value c)), SDF (V.FSVec n4 (Value d)))

event    = SDF . fmap pure
event2   = ($$) (event, event)
event3   = ($$$) (event, event, event)
event4   = ($$$$) (event, event, event, event)

-- -- | Wraps a list into a SY signal
-- signal   :: [a] -> Sig a
-- signal l = S.signal (event <$> l)

vid1 :: V.FSVec D1 a -> V.FSVec D1 a
vid2 :: V.FSVec D1 a -> V.FSVec D1 b -> (V.FSVec D1 a, V.FSVec D1 b)
vid3 :: V.FSVec D1 a -> V.FSVec D1 b -> V.FSVec D1 c -> (V.FSVec D1 a, V.FSVec D1 b, V.FSVec D1 c)
vid4 :: V.FSVec D1 a -> V.FSVec D1 b -> V.FSVec D1 c -> V.FSVec D1 d -> (V.FSVec D1 a, V.FSVec D1 b, V.FSVec D1 c, V.FSVec D1 d)
vid1 = id1
vid2 = id2
vid3 = id3
vid4 = id4
-----------------------------------------------------------------------------

fromSDF (SDF a) = a


unpack Abst       = []
unpack Undef      = []
unpack (Value as) = Value <$> as

extract :: (Nat s) => s -> Signal (SDF (Value a)) -> V.FSVec s (Value a)
extract c = V.reallyUnsafeVector . take (toInt c) . fmap fromSDF . fromSignal

-----------------------------------------------------------------------------


-- isp11 :: (Nat n1, Nat n2) => (V.FSVec n1 a -> V.FSVec n2 b) -> V.FSVec n1 (Value a) -> V.FSVec n2 (Value b)

pack ::(Nat s) => V.FSVec s (Value a) -> Value (V.FSVec s a)
pack = fmap V.reallyUnsafeVector . transpose . V.fromVector
  where transpose []     = Value []
        transpose (a:as) = (:) <$> a <*> transpose as

-- TODO:: WRONG!!!
unpack' ::(Nat s) => Value (V.FSVec s a) -> V.FSVec s (Value a)
unpack' Abst       = V.reallyUnsafeVector [Abst]
unpack' Undef      = V.reallyUnsafeVector [Undef]
unpack' (Value as) = Value <$> as


isp1 f a = f (pack a)
isp2 f a b = f (pack a) (pack b)
isp3 f a b c = f (pack a) (pack b) (pack c)
isp4 f a b c d = f (pack a) (pack b) (pack c) (pack d)

isp11 f a       = unpack' $ isp1 f a
isp21 f a b     = unpack' $ isp2 f a b
isp31 f a b c   = unpack' $ isp3 f a b c 
isp41 f a b c d = unpack' $ isp4 f a b c d

isp12 f a       = (unpack',unpack') $$ isp1 f a
isp22 f a b     = (unpack',unpack') $$ isp2 f a b
isp32 f a b c   = (unpack',unpack') $$ isp3 f a b c 
isp42 f a b c d = (unpack',unpack') $$ isp4 f a b c d

isp13 f a       = (unpack',unpack',unpack') $$$ isp1 f a
isp23 f a b     = (unpack',unpack',unpack') $$$ isp2 f a b
isp33 f a b c   = (unpack',unpack',unpack') $$$ isp3 f a b c 
isp43 f a b c d = (unpack',unpack',unpack') $$$ isp4 f a b c d

isp14 f a       = (unpack',unpack',unpack',unpack') $$$$ isp1 f a
isp24 f a b     = (unpack',unpack',unpack',unpack') $$$$ isp2 f a b
isp34 f a b c   = (unpack',unpack',unpack',unpack') $$$$ isp3 f a b c 
isp44 f a b c d = (unpack',unpack',unpack',unpack') $$$$ isp4 f a b c d
