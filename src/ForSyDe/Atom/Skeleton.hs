{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module ForSyDe.Atom.Skeleton where

import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Signal
import ForSyDe.Atom.Utility

import Prelude hiding (map, fold, take)

infixl 5 =$=, =*=, =<<=

class (Functor c) => Skeleton c where
  (=$=)  :: (a -> b) -> c a -> c b
  (=*=)  :: c (a -> b) -> c a -> c b
  (=<<=) :: c (a -> a) -> a -> a
  hd     :: c a -> a
  tl     :: c a -> c a

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

red1 p c1                      = map11 p (tl c1)                      =<<= hd c1
red2 p c1 c2                   = map21 p c1 (tl c2)                   =<<= hd c2
red3 p c1 c2 c3                = map31 p c1 c2 (tl c3)                =<<= hd c3
red4 p c1 c2 c3 c4             = map41 p c1 c2 c3 (tl c4)             =<<= hd c4
red5 p c1 c2 c3 c4 c5          = map51 p c1 c2 c3 c4 (tl c5)          =<<= hd c5
red6 p c1 c2 c3 c4 c5 c6       = map61 p c1 c2 c3 c4 c5 (tl c6)       =<<= hd c6
red7 p c1 c2 c3 c4 c5 c6 c7    = map71 p c1 c2 c3 c4 c5 c6 (tl c7)    =<<= hd c7
red8 p c1 c2 c3 c4 c5 c6 c7 c8 = map81 p c1 c2 c3 c4 c5 c6 c7 (tl c8) =<<= hd c8

