{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module ForSyDe.Atom.Skeleton where

import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Signal
import ForSyDe.Atom.Utility

import Prelude hiding (map, fold, take)

infixl 5 =$=, =*=, =<<=

class (Functor c) => Skeleton c where
  empty  :: c a
  single :: a -> c a
  first  :: c a -> a
  rest   :: c a -> c a
  (=++=) :: c a -> c a -> c a
  ----------------------------------
  (=$=)  :: (a -> b) -> c a -> c b
  (=*=)  :: c (a -> b) -> c a -> c b
  (=<<=) :: c (a -> a) -> a -> a

pipe s ps = ps =<<= s

map11 p c1                      = (p =$= c1)
map21 p c1 c2                   = (p =$= c1 =*= c2)
map31 p c1 c2 c3                = (p =$= c1 =*= c2 =*= c3)
map41 p c1 c2 c3 c4             = (p =$= c1 =*= c2 =*= c3 =*= c4)
map51 p c1 c2 c3 c4 c5          = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5)
map61 p c1 c2 c3 c4 c5 c6       = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6)
map71 p c1 c2 c3 c4 c5 c6 c7    = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c7)
map81 p c1 c2 c3 c4 c5 c6 c7 c8 = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c7 =*= c8)

map12 p c1                      = (p =$= c1 |<)
map22 p c1 c2                   = (p =$= c1 =*= c2 |<)
map32 p c1 c2 c3                = (p =$= c1 =*= c2 =*= c3 |<)
map42 p c1 c2 c3 c4             = (p =$= c1 =*= c2 =*= c3 =*= c4 |<)
map52 p c1 c2 c3 c4 c5          = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 |<)
map62 p c1 c2 c3 c4 c5 c6       = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 |<)
map72 p c1 c2 c3 c4 c5 c6 c7    = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c7 |<)
map82 p c1 c2 c3 c4 c5 c6 c7 c8 = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c5 =*= c8 |<)

map13 p c1                      = (p =$= c1 |<<)
map23 p c1 c2                   = (p =$= c1 =*= c2 |<<)
map33 p c1 c2 c3                = (p =$= c1 =*= c2 =*= c3 |<<)
map43 p c1 c2 c3 c4             = (p =$= c1 =*= c2 =*= c3 =*= c4 |<<)
map53 p c1 c2 c3 c4 c5          = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 |<<)
map63 p c1 c2 c3 c4 c5 c6       = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 |<<)
map73 p c1 c2 c3 c4 c5 c6 c7    = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c7 |<<)
map83 p c1 c2 c3 c4 c5 c6 c7 c8 = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c5 =*= c8 |<<)

map14 p c1                      = (p =$= c1 |<<<)
map24 p c1 c2                   = (p =$= c1 =*= c2 |<<<)
map34 p c1 c2 c3                = (p =$= c1 =*= c2 =*= c3 |<<<)
map44 p c1 c2 c3 c4             = (p =$= c1 =*= c2 =*= c3 =*= c4 |<<<)
map54 p c1 c2 c3 c4 c5          = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 |<<<)
map64 p c1 c2 c3 c4 c5 c6       = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 |<<<)
map74 p c1 c2 c3 c4 c5 c6 c7    = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c7 |<<<)
map84 p c1 c2 c3 c4 c5 c6 c7 c8 = (p =$= c1 =*= c2 =*= c3 =*= c4 =*= c5 =*= c6 =*= c7 =*= c8 |<<<)

red1 p c1                      = map11 p (rest c1)                      =<<= first c1
red2 p c1 c2                   = map21 p c1 (rest c2)                   =<<= first c2
red3 p c1 c2 c3                = map31 p c1 c2 (rest c3)                =<<= first c3
red4 p c1 c2 c3 c4             = map41 p c1 c2 c3 (rest c4)             =<<= first c4
red5 p c1 c2 c3 c4 c5          = map51 p c1 c2 c3 c4 (rest c5)          =<<= first c5
red6 p c1 c2 c3 c4 c5 c6       = map61 p c1 c2 c3 c4 c5 (rest c6)       =<<= first c6
red7 p c1 c2 c3 c4 c5 c6 c7    = map71 p c1 c2 c3 c4 c5 c6 (rest c7)    =<<= first c7
red8 p c1 c2 c3 c4 c5 c6 c7 c8 = map81 p c1 c2 c3 c4 c5 c6 c7 (rest c8) =<<= first c8

-- scan11 :: (Skeleton c) => c (a -> a) -> a -> c a
-- scan11 ps s = (=++=) =$= (single $ (first ps) $ ((rest ps) =<<= s)) =*= scan11 (rest ps) s

scan11 p = red1 (\x y -> x =++= map11 (p $ first x) y) . map11 single

tails :: (Skeleton c, Skeleton c1) => c a -> c (c1 a)
tails    = red1 (\x y -> x =++= map11 (first x =++=) y) . map11 (single . single)

scans :: (Skeleton c) => c (a -> a) -> a -> c a
scans ps s = map11 (=<<= s) ts
  where ts = tails ps

--------------------------------------------------------------------------------------------

-- pipeV         :: Vector (b -> b) -> b -> b
-- pipeV NullV   = id
-- pipeV (v:>vs) = v . pipeV vs 

-- scanV         :: Vector (b -> b) -> b -> Vector b
-- scanV NullV   = pure NullV  
-- scanV (x:>xs) = (:>) <$> x . pipeV xs <*> scanV xs
