{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
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

import ForSyDe.Atom.MoC hiding (Sig)
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility

-- | Type synonym for a "partitions of SDF events", It also hides the
-- reverse implementation of the previous statement (but with the same
-- semantic).
type Partition a = SDF [Value a]

-- | Type synonym for a SDF signal, i.e. "a signal of partitions of
-- SDF events"
type Sig a       = S.Signal (Partition a)

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
newtype SDF a = SDF { partition :: a }

-- | Implenents the CT semantics for the MoC atoms
instance MoC SDF where
  type Context SDF = Int
  ---------------------
  _ -$- NullS = NullS
  (c,f) -$- s = (comb c f . concat . map partition . fromSignal) s
    where comb c f l = let x'  = take c l
                           xs' = drop c l
                       in if   length x' == c
                          then SDF (f x') :- comb c f xs'
                          else NullS
  ---------------------
  _  -*- NullS = NullS
  NullS -*- _  = NullS
  cfs -*- s = (comb2 cfs . concat . map partition . fromSignal) s
    where comb2 NullS           _ = NullS
          comb2 (SDF (c,f):-fs) l = let x'  = take c l
                                        xs' = drop c l
                                    in if   length x' == c
                                       then SDF (f x') :- comb2 fs xs'
                                       else NullS
  ---------------------
  (->-) = (:-) 
  ---------------------
  (-&-) _ a = a
  ---------------------
  -- sniff (SDF a) = a

-- | A more efficient instatiation since we /know/ that the partition
-- size is always 1.
instance Functor (SDF) where
  fmap f (SDF a) = SDF (f a)

-- | Allows for comparing signals with different partitions. __TODO!__ incomplete definition.
instance Eq a => Eq (Signal (SDF [a])) where
  a ==b = flatten a == flatten b
    where flatten = concat . map partition . fromSignal
  
-- | 'Show' instance. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (SDF [a]) where
  showsPrec p = showParen (p>1) . showPartition . partition
    where
      showPartition (x : xs)  = showEvent x . showPartition' xs 
      showPartition ([])      = showChar ' ' . showChar '\b'
      showPartition' (x : xs) = showChar ',' . showEvent x . showPartition' xs
      showPartition' ([])     = showChar ' ' . showChar '\b'
      showEvent x             = shows x

-----------------------------------------------------------------------------
-- | Transforms a list of values into a SDF signal with only one
-- partition, i.e. all events share the same (initial) tag.
signal :: [a] -> Sig a
signal l = S.signal [part l]

part :: [a] -> Partition a
part l = SDF (Value <$> l)

-- | Wraps a (tuple of) value list(s) into the equivalent event
-- partitions form.
--
-- "ForSyDe.Atom.MoC.SDF" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > part, part2, part3, part4,
part2 (l1,l2) = (part l1, part l2)
part3 (l1,l2,l3) = (part l1, part l2, part l3)
part4 (l1,l2,l3,l4) = (part l1, part l2, part l3, part l4)

-----------------------------------------------------------------------------

check p Abst      = take p $ repeat Abst
check p Undef     = take p $ repeat Undef
check p (Value x) | length x /= p = error "Wrong production"
                  | otherwise     = Value <$> x

-- | Wraps a function on extended values into the format needed by the
-- MoC atoms.
--
-- <<includes/figs/untimed-wrapper-formula1.png>>
--
-- "ForSyDe.Atom.MoC.DE" exports the helper functions below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > wrap11, wrap21, wrap31, wrap41, wrap51, wrap61, wrap71, wrap81, 
-- > wrap12, wrap22, wrap32, wrap42, wrap52, wrap62, wrap72, wrap82, 
-- > wrap13, wrap23, wrap33, wrap43, wrap53, wrap63, wrap73, wrap83, 
-- > wrap14, wrap24, wrap34, wrap44, wrap54, wrap64, wrap74, wrap84,
wrap22 :: (Int, Int)  -- ^ production rates
       -> (Int, Int)  -- ^ consumption rates
       -> (Value [a3] -> Value [a2] -> (Value [a], Value [a1]))
          -- ^ behvioural function on partitions to be wrapped
       -> (Int, [Value a3] -> (Int, [Value a2] -> ([Value a], [Value a1])))
          -- ^ wrapped form, as required by the atom constructor.

wrap :: Int -> ([Value a] -> b) -> (Int, [Value a] -> b)
wrap c f = (c, \x -> f x)

wrap11 (c1)                      p f = wrap c1 $ check p . f . sequenceA
wrap21 (c1,c2)                   p f = wrap c1 $ wrap11  c2 p . f . sequenceA
wrap31 (c1,c2,c3)                p f = wrap c1 $ wrap21 (c2,c3) p . f . sequenceA
wrap41 (c1,c2,c3,c4)             p f = wrap c1 $ wrap31 (c2,c3,c4) p . f . sequenceA
wrap51 (c1,c2,c3,c4,c5)          p f = wrap c1 $ wrap41 (c2,c3,c4,c5) p . f . sequenceA
wrap61 (c1,c2,c3,c4,c5,c6)       p f = wrap c1 $ wrap51 (c2,c3,c4,c5,c6) p . f . sequenceA
wrap71 (c1,c2,c3,c4,c5,c6,c7)    p f = wrap c1 $ wrap61 (c2,c3,c4,c5,c6,c7) p . f . sequenceA
wrap81 (c1,c2,c3,c4,c5,c6,c7,c8) p f = wrap c1 $ wrap71 (c2,c3,c4,c5,c6,c7,c8) p . f . sequenceA

wrap12 (c1)                 (p1,p2) f = wrap c1 $ ($$) (check p1, check p2) . f . sequenceA
wrap22 (c1,c2)                   ps f = wrap c1 $ wrap12  c2 ps . f . sequenceA
wrap32 (c1,c2,c3)                ps f = wrap c1 $ wrap22 (c2,c3) ps . f . sequenceA
wrap42 (c1,c2,c3,c4)             ps f = wrap c1 $ wrap32 (c2,c3,c4) ps . f . sequenceA
wrap52 (c1,c2,c3,c4,c5)          ps f = wrap c1 $ wrap42 (c2,c3,c4,c5) ps . f . sequenceA
wrap62 (c1,c2,c3,c4,c5,c6)       ps f = wrap c1 $ wrap52 (c2,c3,c4,c5,c6) ps . f . sequenceA
wrap72 (c1,c2,c3,c4,c5,c6,c7)    ps f = wrap c1 $ wrap62 (c2,c3,c4,c5,c6,c7) ps . f . sequenceA
wrap82 (c1,c2,c3,c4,c5,c6,c7,c8) ps f = wrap c1 $ wrap72 (c2,c3,c4,c5,c6,c7,c8) ps . f . sequenceA

wrap13 (c1)              (p1,p2,p3) f = wrap c1 $ ($$$) (check p1, check p2, check p3) . f . sequenceA
wrap23 (c1,c2)                   ps f = wrap c1 $ wrap13  c2 ps . f . sequenceA
wrap33 (c1,c2,c3)                ps f = wrap c1 $ wrap23 (c2,c3) ps . f . sequenceA
wrap43 (c1,c2,c3,c4)             ps f = wrap c1 $ wrap33 (c2,c3,c4) ps . f . sequenceA
wrap53 (c1,c2,c3,c4,c5)          ps f = wrap c1 $ wrap43 (c2,c3,c4,c5) ps . f . sequenceA
wrap63 (c1,c2,c3,c4,c5,c6)       ps f = wrap c1 $ wrap53 (c2,c3,c4,c5,c6) ps . f . sequenceA
wrap73 (c1,c2,c3,c4,c5,c6,c7)    ps f = wrap c1 $ wrap63 (c2,c3,c4,c5,c6,c7) ps . f . sequenceA
wrap83 (c1,c2,c3,c4,c5,c6,c7,c8) ps f = wrap c1 $ wrap73 (c2,c3,c4,c5,c6,c7,c8) ps . f . sequenceA

wrap14 (c1)           (p1,p2,p3,p4) f = wrap c1 $ ($$$$) (check p1, check p2, check p3, check p4) . f . sequenceA
wrap24 (c1,c2)                   ps f = wrap c1 $ wrap14  c2 ps . f . sequenceA
wrap34 (c1,c2,c3)                ps f = wrap c1 $ wrap24 (c2,c3) ps . f . sequenceA
wrap44 (c1,c2,c3,c4)             ps f = wrap c1 $ wrap34 (c2,c3,c4) ps . f . sequenceA
wrap54 (c1,c2,c3,c4,c5)          ps f = wrap c1 $ wrap44 (c2,c3,c4,c5) ps . f . sequenceA
wrap64 (c1,c2,c3,c4,c5,c6)       ps f = wrap c1 $ wrap54 (c2,c3,c4,c5,c6) ps . f . sequenceA
wrap74 (c1,c2,c3,c4,c5,c6,c7)    ps f = wrap c1 $ wrap64 (c2,c3,c4,c5,c6,c7) ps . f . sequenceA
wrap84 (c1,c2,c3,c4,c5,c6,c7,c8) ps f = wrap c1 $ wrap74 (c2,c3,c4,c5,c6,c7,c8) ps . f . sequenceA

-- s = ForSyDe.Atom.MoC.SDF.Core.signal [1,2,3,4,5,6,7,8,9]

-- tst1 x = [head x]
-- tst2 x y = [head x + head y, last y - last x]

