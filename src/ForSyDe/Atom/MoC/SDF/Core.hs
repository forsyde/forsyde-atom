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


import qualified Data.Param.FSVec as V
import Data.TypeLevel hiding ((==))

-- | Type alias for a CT signal
-- type Sigr a = S.Signal (SDF D0 (Value a)) 


type Sig a    = (forall rate.Nat rate => S.Signal (SDF rate (Value a)))
type Sigr r a = S.Signal (SDF r (Value a))

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data SDF c a = SDF a

-- | Implenents the CT semantics for the MoC atoms
instance MoC SDF where
  type Arg SDF c a = Value (V.FSVec c a)
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
  (->-) = (:-)
  ---------------------
  (-&-) _ a = a
  ---------------------

instance ContextFunctor SDF where
  type Context SDF c    = (Nat c)
  fmapC f (SDF a)       = SDF (f a)
  liftC (SDF f) (SDF a) = SDF (f a)


-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor (SDF c) where
  fmap f (SDF a) = SDF (f a)
 
-- | Shows the (extended) value wrapped
instance Show a => Show (SDF c a) where
  showsPrec _ (SDF x) = (++) (show x)

-----------------------------------------------------------------------------

-- | Wraps a base value into a SY event of extended values
event    :: (Nat c) => a -> SDF c (Value a)
event    = SDF . Value 

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal (event <$> l)

-----------------------------------------------------------------------------

fromSDF (SDF a) = a

pack ::(Nat s) => V.FSVec s (Value a) -> Value (V.FSVec s a)
pack = fmap V.FSVec . transpose . V.fromVector
  where transpose []     = Value []
        transpose (a:as) = (:) <$> a <*> transpose as

extract :: (Nat s1, Nat s2) => s2 -> Signal (SDF s1 (Value a)) -> V.FSVec s2 (Value a)
extract c = V.reallyUnsafeVector . take (toInt c) . fmap fromSDF . fromSignal

cat :: (Nat p, Nat p') => Signal (SDF p' (Value (V.FSVec p a))) -> S.Signal (SDF p (Value a))
cat = (S.signal . fmap SDF . concat . fmap unpack . (fmap . fmap) V.fromVector . fmap fromSDF . S.fromSignal)
  where unpack Abst       = []
        unpack Undef      = []
        unpack (Value as) = Value <$> as


-- -----------------------------------------------------------------------------

-- test :: V.FSVec D1 a -> V.FSVec D2 a
-- test' :: V.FSVec D2 a -> (V.FSVec D1 Int, V.FSVec D2 a)
-- test'' :: V.FSVec D3 a -> V.FSVec D4 a -> V.FSVec D2 a
-- test'''' :: V.FSVec D4 a -> V.FSVec D2 a

-- test x = V.unsafeVector d2 [V.head x, V.last x]
-- test' x = (V.unsafeVector d1 [V.length x], V.unsafeVector d2 [V.head x, V.last x])
-- test'' x y = V.unsafeVector d2 [V.head x, V.last y]
-- test'''' x = V.unsafeVector d2 [V.head x, V.last x]


-- s = ForSyDe.Atom.MoC.SDF.Core.signal [1,2,3,4,5,6,7] 

-- q' = (psi12 test' -$- s ||<)
-- q = psi11 test -$- s 
-- q'' = psi21 test'' -$- s -*- s
-- q'''' = psi11 test'''' -$- s 


-- -- q''' a = cat $ psi21 test'' -$- a -*- a


-- -- -- e'''
-- -- --   :: Nat c2 =>
-- -- --      Sig a
-- -- --      -> Signal (SDF c2 (Value a)) -> Signal (SDF D4 (Value a))

-- -- e''' a b = cat $ C.comb21 (psi21 test'') a b

-- -- -- w  = cat q             
  
-- -- -- w' = (cat, cat) <**> q'  
-- -- w''= cat q'' :: Sigr Int     

-- -- -- z = psi11 test -$- w
