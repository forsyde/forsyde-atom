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
import ForSyDe.Atom.Utility.Tuple

-- | Type synonym for production rate
type Cons = Int

-- | Type synonym for consumption rate
type Prod = Int

-- | Type synonym for a SY signal, i.e. "a signal of SY events"
type Signal a   = Stream (SDF a)

-- | The SDF event. It identifies a synchronous dataflow signal, and
-- wraps only a value.
newtype SDF a = SDF { val :: a }

-- | Implenents the SDF semantics for the MoC atoms.
instance MoC SDF where
  type Fun SDF a b = (Cons, [a] -> b)
  type Ret SDF a   = (Prod, [a])
  ---------------------
  _ -.- NullS = NullS
  (c,f) -.- s = (comb c f . map val . fromStream) s
    where comb c f l = let x'  = take c l
                           xs' = drop c l
                       in if   length x' == c
                          then SDF (f x') :- comb c f xs'
                          else NullS
  ---------------------
  cfs -*- s = (comb2 cfs . map val . fromStream) s
    where comb2 NullS           _ = NullS
          comb2 (SDF (c,f):-fs) l = let x'  = take c l
                                        xs' = drop c l
                                    in if   length x' == c
                                       then SDF (f x') :- comb2 fs xs'
                                       else NullS
  ---------------------
  (-*) NullS = NullS
  (-*) ((SDF (p,r)):-xs)
    | length r == p = stream (map SDF r) +-+ (xs -*)
    | otherwise     = error "[MoC.SDF] Wrong production"
  ---------------------
  (-<-) = (+-+)
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

signal2 (l1,l2)       = (signal l1, signal l2)
signal3 (l1,l2,l3)    = (signal l1, signal l2, signal l3)
signal4 (l1,l2,l3,l4) = (signal l1, signal l2, signal l3, signal l4)

-- | Transforms a signal back to a list
fromSignal :: Signal a -> [a]
fromSignal = fromStream  . fmap (\(SDF a) -> a) 


-- | Reads a signal from a string. Like with the @read@ function from
-- @Prelude@, you must specify the tipe of the signal.
--
-- >>> readSignal "{1,2,3,4,5}" :: Signal Int
-- {1,2,3,4,5}
readSignal :: Read a => String -> Signal a
readSignal = read

----------------------------------------------------------------------

scen11 (c,p,f) = ctxt11 c p f 
scen12 (c,p,f) = ctxt12 c p f 
scen13 (c,p,f) = ctxt13 c p f 
scen14 (c,p,f) = ctxt14 c p f 
scen21 (c,p,f) = ctxt21 c p f 
scen22 (c,p,f) = ctxt22 c p f 
scen23 (c,p,f) = ctxt23 c p f 
scen24 (c,p,f) = ctxt24 c p f
scen31 (c,p,f) = ctxt31 c p f 
scen32 (c,p,f) = ctxt32 c p f 
scen33 (c,p,f) = ctxt33 c p f 
scen34 (c,p,f) = ctxt34 c p f 
scen41 (c,p,f) = ctxt41 c p f 
scen42 (c,p,f) = ctxt42 c p f 
scen43 (c,p,f) = ctxt43 c p f 
scen44 (c,p,f) = ctxt44 c p f
scen51 (c,p,f) = ctxt51 c p f 
scen52 (c,p,f) = ctxt52 c p f 
scen53 (c,p,f) = ctxt53 c p f 
scen54 (c,p,f) = ctxt54 c p f 
scen61 (c,p,f) = ctxt61 c p f 
scen62 (c,p,f) = ctxt62 c p f 
scen63 (c,p,f) = ctxt63 c p f 
scen64 (c,p,f) = ctxt64 c p f 
scen71 (c,p,f) = ctxt71 c p f 
scen72 (c,p,f) = ctxt72 c p f 
scen73 (c,p,f) = ctxt73 c p f 
scen74 (c,p,f) = ctxt74 c p f 
scen81 (c,p,f) = ctxt81 c p f 
scen82 (c,p,f) = ctxt82 c p f 
scen83 (c,p,f) = ctxt83 c p f 
scen84 (c,p,f) = ctxt84 c p f  
