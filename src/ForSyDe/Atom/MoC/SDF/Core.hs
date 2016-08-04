{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
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

import ForSyDe.Atom.MoC.AtomLib
import ForSyDe.Atom.Signal as S
import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Utility



-- | Type alias for a CT signal
type Sig a       = S.Signal (Partition a)
type Partition a = SDF [Value a]

-- | The CT type, identifying a discrete time event and implementing an
-- instance of the 'MoC' class. A discrete event explicitates its tag
-- which is represented as an integer.
newtype SDF a = SDF { fromSDF :: a }

-- | Implenents the CT semantics for the MoC atoms
instance MoC SDF where
  type Param SDF = [Int]
  ---------------------
  (env ,_) -$- NullS = (env, NullS)
  (c:cs,f) -$- s = (cs, comb c f $ concat $ map fromSDF $ fromSignal s)
    where comb c f l = let x'  = take c l
                           xs' = drop c l
                       in if   length x' == c
                          then SDF (f x') :- comb c f xs'
                          else NullS
  ---------------------
  (env ,_)  -*- NullS = (env, NullS)
  (env ,NullS) -*- _  = (env, NullS)
  (c:cs,fs) -*- s = (cs, comb2 c fs $ concat $ map fromSDF $ fromSignal s)
    where comb2 c (SDF f:-fs) l = let x'  = take c l
                                      xs' = drop c l
                                  in if   length x' == c
                                     then SDF (f x') :- comb2 c fs xs'
                                     else NullS
  ---------------------
  (->-) = (:-) 
  ---------------------
  (-&-) _ a = a
  ---------------------

-- | Needed for the implementation of the '-$-' atom and also the
-- @unzip@ utilities.
instance Functor (SDF) where
  fmap f (SDF a) = SDF (f a)
 
-- | 'Show' instance. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
instance (Show a) => Show (SDF [a]) where
  showsPrec p = showParen (p>1) . showSignal . fromSDF
    where
      showSignal (x : xs)  = showEvent x . showSignal' xs
      showSignal ([])      = showChar ' ' . showChar '\b'
      showSignal' (x : xs) = showChar ',' . showEvent x . showSignal' xs
      showSignal' ([])     = showChar ' ' . showChar '\b'
      showEvent x          = shows x

-----------------------------------------------------------------------------

signal :: [a] -> Sig a
signal l = S.signal [part l]

part :: [a] -> Partition a
part l = SDF (Value <$> l)

part2 (l1,l2) = (part l1, part l2)
part3 (l1,l2,l3) = (part l1, part l2, part l3)
part4 (l1,l2,l3,l4) = (part l1, part l2, part l3, part l4)

-----------------------------------------------------------------------------

check p Abst      = take p $ repeat Abst
check p Undef     = take p $ repeat Undef
check p (Value x) | length x /= p = error "Wrong production"
                  | otherwise     = Value <$> x

check11 p f x1                      = check p $ sequenceA1 f x1
check21 p f x1 x2                   = check p $ sequenceA2 f x1 x2
check31 p f x1 x2 x3                = check p $ sequenceA3 f x1 x2 x3 
check41 p f x1 x2 x3 x4             = check p $ sequenceA4 f x1 x2 x3 x4  
check51 p f x1 x2 x3 x4 x5          = check p $ sequenceA5 f x1 x2 x3 x4 x5   
check61 p f x1 x2 x3 x4 x5 x6       = check p $ sequenceA6 f x1 x2 x3 x4 x5 x6
check71 p f x1 x2 x3 x4 x5 x6 x7    = check p $ sequenceA7 f x1 x2 x3 x4 x5 x6 x7
check81 p f x1 x2 x3 x4 x5 x6 x7 x8 = check p $ sequenceA8 f x1 x2 x3 x4 x5 x6 x7 x8

check12 ps f x1                      = (check,check) $$ ps $$ sequenceA1 f x1
check22 ps f x1 x2                   = (check,check) $$ ps $$ sequenceA2 f x1 x2
check32 ps f x1 x2 x3                = (check,check) $$ ps $$ sequenceA3 f x1 x2 x3 
check42 ps f x1 x2 x3 x4             = (check,check) $$ ps $$ sequenceA4 f x1 x2 x3 x4  
check52 ps f x1 x2 x3 x4 x5          = (check,check) $$ ps $$ sequenceA5 f x1 x2 x3 x4 x5   
check62 ps f x1 x2 x3 x4 x5 x6       = (check,check) $$ ps $$ sequenceA6 f x1 x2 x3 x4 x5 x6
check72 ps f x1 x2 x3 x4 x5 x6 x7    = (check,check) $$ ps $$ sequenceA7 f x1 x2 x3 x4 x5 x6 x7
check82 ps f x1 x2 x3 x4 x5 x6 x7 x8 = (check,check) $$ ps $$ sequenceA8 f x1 x2 x3 x4 x5 x6 x7 x8

check13 ps f x1                      = (check,check,check) $$$ ps $$$ sequenceA1 f x1
check23 ps f x1 x2                   = (check,check,check) $$$ ps $$$ sequenceA2 f x1 x2
check33 ps f x1 x2 x3                = (check,check,check) $$$ ps $$$ sequenceA3 f x1 x2 x3 
check43 ps f x1 x2 x3 x4             = (check,check,check) $$$ ps $$$ sequenceA4 f x1 x2 x3 x4  
check53 ps f x1 x2 x3 x4 x5          = (check,check,check) $$$ ps $$$ sequenceA5 f x1 x2 x3 x4 x5   
check63 ps f x1 x2 x3 x4 x5 x6       = (check,check,check) $$$ ps $$$ sequenceA6 f x1 x2 x3 x4 x5 x6
check73 ps f x1 x2 x3 x4 x5 x6 x7    = (check,check,check) $$$ ps $$$ sequenceA7 f x1 x2 x3 x4 x5 x6 x7
check83 ps f x1 x2 x3 x4 x5 x6 x7 x8 = (check,check,check) $$$ ps $$$ sequenceA8 f x1 x2 x3 x4 x5 x6 x7 x8

check14 ps f x1                      = (check,check,check,check) $$$$ ps $$$$ sequenceA1 f x1
check24 ps f x1 x2                   = (check,check,check,check) $$$$ ps $$$$ sequenceA2 f x1 x2
check34 ps f x1 x2 x3                = (check,check,check,check) $$$$ ps $$$$ sequenceA3 f x1 x2 x3 
check44 ps f x1 x2 x3 x4             = (check,check,check,check) $$$$ ps $$$$ sequenceA4 f x1 x2 x3 x4  
check54 ps f x1 x2 x3 x4 x5          = (check,check,check,check) $$$$ ps $$$$ sequenceA5 f x1 x2 x3 x4 x5   
check64 ps f x1 x2 x3 x4 x5 x6       = (check,check,check,check) $$$$ ps $$$$ sequenceA6 f x1 x2 x3 x4 x5 x6
check74 ps f x1 x2 x3 x4 x5 x6 x7    = (check,check,check,check) $$$$ ps $$$$ sequenceA7 f x1 x2 x3 x4 x5 x6 x7
check84 ps f x1 x2 x3 x4 x5 x6 x7 x8 = (check,check,check,check) $$$$ ps $$$$ sequenceA8 f x1 x2 x3 x4 x5 x6 x7 x8


wrap11 (c1)                      ps = (,) [c1]                      . check11 ps
wrap21 (c1,c2)                   ps = (,) [c1,c2]                   . check21 ps
wrap31 (c1,c2,c3)                ps = (,) [c1,c2,c3]                . check31 ps
wrap41 (c1,c2,c3,c4)             ps = (,) [c1,c2,c3,c4]             . check41 ps
wrap51 (c1,c2,c3,c4,c5)          ps = (,) [c1,c2,c3,c4,c5]          . check51 ps
wrap61 (c1,c2,c3,c4,c5,c6)       ps = (,) [c1,c2,c3,c4,c5,c6]       . check61 ps
wrap71 (c1,c2,c3,c4,c5,c6,c7)    ps = (,) [c1,c2,c3,c4,c5,c6,c7]    . check71 ps
wrap81 (c1,c2,c3,c4,c5,c6,c7,c8) ps = (,) [c1,c2,c3,c4,c5,c6,c7,c8] . check81 ps

wrap12 (c1)                      ps = (,) [c1]                      . check12 ps
wrap22 (c1,c2)                   ps = (,) [c1,c2]                   . check22 ps
wrap32 (c1,c2,c3)                ps = (,) [c1,c2,c3]                . check32 ps
wrap42 (c1,c2,c3,c4)             ps = (,) [c1,c2,c3,c4]             . check42 ps
wrap52 (c1,c2,c3,c4,c5)          ps = (,) [c1,c2,c3,c4,c5]          . check52 ps
wrap62 (c1,c2,c3,c4,c5,c6)       ps = (,) [c1,c2,c3,c4,c5,c6]       . check62 ps
wrap72 (c1,c2,c3,c4,c5,c6,c7)    ps = (,) [c1,c2,c3,c4,c5,c6,c7]    . check72 ps
wrap82 (c1,c2,c3,c4,c5,c6,c7,c8) ps = (,) [c1,c2,c3,c4,c5,c6,c7,c8] . check82 ps

wrap13 (c1)                      ps = (,) [c1]                      . check13 ps
wrap23 (c1,c2)                   ps = (,) [c1,c2]                   . check23 ps
wrap33 (c1,c2,c3)                ps = (,) [c1,c2,c3]                . check33 ps
wrap43 (c1,c2,c3,c4)             ps = (,) [c1,c2,c3,c4]             . check43 ps
wrap53 (c1,c2,c3,c4,c5)          ps = (,) [c1,c2,c3,c4,c5]          . check53 ps
wrap63 (c1,c2,c3,c4,c5,c6)       ps = (,) [c1,c2,c3,c4,c5,c6]       . check63 ps
wrap73 (c1,c2,c3,c4,c5,c6,c7)    ps = (,) [c1,c2,c3,c4,c5,c6,c7]    . check73 ps
wrap83 (c1,c2,c3,c4,c5,c6,c7,c8) ps = (,) [c1,c2,c3,c4,c5,c6,c7,c8] . check83 ps

wrap14 (c1)                      ps = (,) [c1]                      . check14 ps
wrap24 (c1,c2)                   ps = (,) [c1,c2]                   . check24 ps
wrap34 (c1,c2,c3)                ps = (,) [c1,c2,c3]                . check34 ps
wrap44 (c1,c2,c3,c4)             ps = (,) [c1,c2,c3,c4]             . check44 ps
wrap54 (c1,c2,c3,c4,c5)          ps = (,) [c1,c2,c3,c4,c5]          . check54 ps
wrap64 (c1,c2,c3,c4,c5,c6)       ps = (,) [c1,c2,c3,c4,c5,c6]       . check64 ps
wrap74 (c1,c2,c3,c4,c5,c6,c7)    ps = (,) [c1,c2,c3,c4,c5,c6,c7]    . check74 ps
wrap84 (c1,c2,c3,c4,c5,c6,c7,c8) ps = (,) [c1,c2,c3,c4,c5,c6,c7,c8] . check84 ps


-- s = ForSyDe.Atom.MoC.SDF.Core.signal [1,2,3,4,5,6,7,8,9]

-- tst1 x = [head x]
-- tst2 x y = [head x + head y, last y - last x]

