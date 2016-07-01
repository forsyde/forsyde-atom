{-# LANGUAGE TypeFamilies, PostfixOperators #-}
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

-- | Type alias for a CT signal
type Sig a = S.Signal (SDF (Value a))

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
data SDF a = SDF Rate a
type Rate  = Int

fromSDF (SDF _ a) = a

-- | Implenents the CT semantics for the MoC atoms
instance MoC SDF where
  type Arg SDF a = Value [a]
  ---------------------
  _ -$- NullS           = NullS
  f -$- s@(SDF c x:-xs) = let x'  = (take c . fmap fromSDF . fromSignal) s 
                              xs' = dropS c s
                          in if length x' == c then SDF c (f $ pack x') :- f -$- xs' else NullS
  ---------------------
  NullS -*- _                       = NullS
  (SDF _ f:-fs) -*- s@(SDF c x:-xs) = let x'  = (take c . fmap fromSDF . fromSignal) s  
                                          xs' = dropS c s
                                      in if length x' == c then SDF c (f $ pack x') :- fs -*- xs' else NullS
  ---------------------
  (->-) = (:-)
  ---------------------
  (-&-) _ a = a
  ---------------------

-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor SDF where
  fmap f (SDF c a) = SDF c (f a)
 
-- | Shows the (extended) value wrapped
instance Show a => Show (SDF a) where
  showsPrec _ (SDF _ x) = (++) (show x)

-----------------------------------------------------------------------------

-- | Wraps a base value into a SY event of extended values
event    :: a -> SDF (Value a)
event    = SDF 0 . Value

-- | Wraps a list into a SY signal
signal   :: [a] -> Sig a
signal l = S.signal (event <$> l)

eat :: Int -> Sig a -> Sig a
eat rate = fmap (\(SDF _ x) -> SDF rate x)

spit :: Int -> Signal (SDF (Value [a])) -> Sig a
spit p out = if anyS (\(SDF _ x) -> not $ (length <$> x) == Value p) out then 
               error "Function does not produce correct number of tokens" 
             else (S.signal . fmap (SDF p) . concat . fmap unpack . fmap fromSDF . S.fromSignal) out

pack :: [Value a] -> Value [a]
pack []     = Value []
pack (a:as) = (:) <$> a <*> pack as

unpack :: Value [a] -> [Value a]
unpack Abst       = []
unpack Undef      = []
unpack (Value as) = Value <$> as


-----------------------------------------------------------------------------
