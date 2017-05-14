{-# LANGUAGE TypeFamilies, FlexibleInstances, PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
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

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility

-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Signal a   = Stream (SDF a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
newtype SDF a = SDF { val :: a }

-- | Implenents the SDF semantics for the MoC atoms
instance MoC SDF where
  type Fun SDF a b = (Int, [a] -> b)
  type Res SDF a   = (Int, [a])
  ---------------------
  _ -.- NullS = NullS
  (c,f) -.- s = (comb c f . map val . fromStream) s
    where comb c f l = let x'  = take c l
                           xs' = drop c l
                       in if   length x' == c
                          then SDF (f x') :- comb c f xs'
                          else NullS
  ---------------------
  _  -*- NullS = NullS
  NullS -*- _  = NullS
  cfs -*- s = (comb2 cfs . map val . fromStream) s
    where comb2 NullS           _ = NullS
          comb2 (SDF (c,f):-fs) l = let x'  = take c l
                                        xs' = drop c l
                                    in if   length x' == c
                                       then SDF (f x') :- comb2 fs xs'
                                       else NullS
  ---------------------
  (-*) NullS = NullS
  (-*) ((SDF (p,r)):-xs) | length r == p = stream (map SDF r) +-+ (xs -*)
                         | otherwise = error "SDF: Wrong production"
  ---------------------
  (-<-) (SDF l) s = (stream $ map SDF l) +-+ s
  ---------------------
  (-&-) _ a = a
  ---------------------

-- | Allows for mapping of functions on a SDF event.
instance Functor SDF where
  fmap f (SDF a) = SDF (f a)

-- | Allows for lifting functions on a pair of SDF events.
instance Applicative SDF where
  pure = SDF 
  (SDF a) <*> (SDF b) = SDF (a b)

instance Foldable SDF where
    foldr f z (SDF x) = f x z
    foldl f z (SDF x) = f z x

instance Traversable SDF where
    traverse f (SDF x) = SDF <$> f x

-- | Shows the value wrapped
instance Show a => Show (SDF a) where
  showsPrec _ (SDF x) = (++) (show x)

-- | Reads the value wrapped
instance Read a => Read (SDF a) where
  readsPrec _ s = [(SDF x, r) | (x, r) <- reads s]

-----------------------------------------------------------------------------

-- | Transforms a list of values into a SDF signal with only one
-- partition, i.e. all events share the same (initial) tag.
signal :: [a] -> Signal a
signal l = stream (SDF <$> l)

part :: [a] -> SDF [a]
part l = SDF l

-- | Wraps a (tuple of) value list(s) into the equivalent event
-- partition form.
--
-- "ForSyDe.Atom.MoC.SDF" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > part, part2, part3, part4,
part2 (l1,l2)       = (part l1, part l2)
part3 (l1,l2,l3)    = (part l1, part l2, part l3)
part4 (l1,l2,l3,l4) = (part l1, part l2, part l3, part l4)

-----------------------------------------------------------------------------

