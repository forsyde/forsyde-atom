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
  type Arg SDF rate a = Value (V.FSVec rate a)
  type Context SDF rate = (Nat rate)
  o = (S.signal . fmap SDF . concat . fmap unpack . (fmap . fmap) V.fromVector . fmap fromSDF . S.fromSignal)

-- | Implenents the CT semantics for the MoC atoms
instance MoC SDF where
  ---------------------
  _ -$- NullS           = NullS
  f -$- xs = let x'  = extract c xs 
                 xs' = dropS (toInt c) xs
                 c   = V.lengthT x'
             in if toInt c == length (V.fromVector x')
                then SDF (f $ pack x') :- f -$- xs'
                else NullS
  ---------------------
  (SDF f :- fs) -*- xs = let x'  = extract c xs
                             xs' = dropS (toInt c) xs
                             c   = V.lengthT x'
                         in if toInt c == length (V.fromVector x')
                            then SDF (f $ pack x') :- fs -*- xs'
                            else NullS
  ---------------------
  (->-) = (+-+) . S.signal . fmap SDF . unpack . fmap V.fromVector . fromSDF
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

event     :: a         -> Event a
event2    :: (a,b)     -> (Event a,Event b)
event3    :: (a,b,c)   -> (Event a,Event b,Event c)
event4    :: (a,b,c,d) -> (Event a,Event b,Event c,Event d)

event    = SDF . Value
event2   = ($$) (event, event)
event3   = ($$$) (event, event, event)
event4   = ($$$$) (event, event, event, event)

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal (event <$> l)

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

pack ::(Nat s) => V.FSVec s (Value a) -> Value (V.FSVec s a)
pack = fmap V.reallyUnsafeVector . transpose . V.fromVector
  where transpose []     = Value []
        transpose (a:as) = (:) <$> a <*> transpose as

unpack Abst       = []
unpack Undef      = []
unpack (Value as) = Value <$> as

extract :: (Nat s) => s -> Signal (SDF (Value a)) -> V.FSVec s (Value a)
extract c = V.reallyUnsafeVector . take (toInt c) . fmap fromSDF . fromSignal

-----------------------------------------------------------------------------
