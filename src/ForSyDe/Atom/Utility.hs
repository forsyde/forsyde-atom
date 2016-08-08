{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Utility
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements general purpose utility functions. It mainly
-- hosts functions dealing with tuples. Utility are provided for up
-- until 9-tuples. Follow the examples in the source code in case it
-- does not suffice.
-----------------------------------------------------------------------------

module ForSyDe.Atom.Utility(
  -- * @at@ functions
  
  -- | The @at@/xy/ functions return the /y/-th element of an
  -- /x/-tuple.
  
  at21, at22,
  at31, at32, at33,
  at41, at42, at43, at44,
  at51, at52, at53, at54, at55,
  at61, at62, at63, at64, at65, at66, 
  at71, at72, at73, at74, at75, at76, at77,
  at81, at82, at83, at84, at85, at86, at87, at88,
  at91, at92, at93, at94, at95, at96, at97, at98, at99,

  -- * The @unzip@ utilities

  -- | These utility functions provided as postfix operators "lift" a
  -- tuple from one layer to several layers above (check
  -- "ForSyDe.Core" for recalling what a "layer" is). It is necessary
  -- to reconstruct containers, for example signals from processes
  -- with multiple outputs, since outputs need to be expressed in
  -- tuple form.
  --
  -- The operator convention is @(|+<+)@, where the number of @|@
  -- represent the number of layers the n-tuple is lifted, while the
  -- number of @<@ + 1 is the order /n/ of the n-tuple.

  -- ** one layer
  
  (|<),
  (|<<),
  (|<<<),
  (|<<<<),
  (|<<<<<),
  (|<<<<<<),
  (|<<<<<<<),
  (|<<<<<<<<),

  -- ** two layers
 
  (||<),
  (||<<),
  (||<<<),
  (||<<<<),
  (||<<<<<),
  (||<<<<<<),
  (||<<<<<<<),
  (||<<<<<<<<),

  -- ** three layers

  (|||<),
  (|||<<),
  (|||<<<),
  (|||<<<<),
  (|||<<<<<),
  (|||<<<<<<),
  (|||<<<<<<<),
  (|||<<<<<<<<), 

  -- ** four layers
  
  (||||<),
  (||||<<),
  (||||<<<),
  (||||<<<<),
  (||||<<<<<),
  (||||<<<<<<),
  (||||<<<<<<<),
  (||||<<<<<<<<), 

  -- * The @curry@ utilities

  (<>),
  (<>>),
  (<>>>),
  (<>>>>),
  (<>>>>>),
  (<>>>>>>),
  (<>>>>>>>),
  (<>>>>>>>>),

  -- * Function application

  ($$),
  ($$$),
  ($$$$),
  ($$$$$),
  ($$$$$$),
  ($$$$$$$),
  ($$$$$$$$),
  ($$$$$$$$$),

  id1, id2, id3, id4, id5, id6, id7, id8, id9,


  sequenceA1, sequenceA2, sequenceA3, sequenceA4, sequenceA5, sequenceA6, sequenceA7, sequenceA8, sequenceA9,
  
  psi11, psi12, psi13, psi14, psi15, psi16, psi17, psi18,
  psi21, psi22, psi23, psi24, psi25, psi26, psi27, psi28,
  psi31, psi32, psi33, psi34, psi35, psi36, psi37, psi38,
  psi41, psi42, psi43, psi44, psi45, psi46, psi47, psi48,
  psi51, psi52, psi53, psi54, psi55, psi56, psi57, psi58,
  psi61, psi62, psi63, psi64, psi65, psi66, psi67, psi68,
  psi71, psi72, psi73, psi74, psi75, psi76, psi77, psi78,
  psi81, psi82, psi83, psi84, psi85, psi86, psi87, psi88
                                                   
  ) where

import Prelude hiding (unzip3, filter)

at21 (x,_)             = x
at22 (_,x)             = x
at31 (x,_,_)           = x
at32 (_,x,_)           = x
at33 (_,_,x)           = x
at41 (x,_,_,_)         = x
at42 (_,x,_,_)         = x
at43 (_,_,x,_)         = x
at44 (_,_,_,x)         = x
at51 (x,_,_,_,_)       = x
at52 (_,x,_,_,_)       = x
at53 (_,_,x,_,_)       = x
at54 (_,_,_,x,_)       = x
at55 (_,_,_,_,x)       = x
at61 (x,_,_,_,_,_)     = x
at62 (_,x,_,_,_,_)     = x
at63 (_,_,x,_,_,_)     = x
at64 (_,_,_,x,_,_)     = x
at65 (_,_,_,_,x,_)     = x
at66 (_,_,_,_,_,x)     = x
at71 (x,_,_,_,_,_,_)   = x
at72 (_,x,_,_,_,_,_)   = x
at73 (_,_,x,_,_,_,_)   = x
at74 (_,_,_,x,_,_,_)   = x
at75 (_,_,_,_,x,_,_)   = x
at76 (_,_,_,_,_,x,_)   = x
at77 (_,_,_,_,_,_,x)   = x
at81 (x,_,_,_,_,_,_,_) = x
at82 (_,x,_,_,_,_,_,_) = x
at83 (_,_,x,_,_,_,_,_) = x
at84 (_,_,_,x,_,_,_,_) = x
at85 (_,_,_,_,x,_,_,_) = x
at86 (_,_,_,_,_,x,_,_) = x
at87 (_,_,_,_,_,_,x,_) = x
at88 (_,_,_,_,_,_,_,x) = x
at91 (x,_,_,_,_,_,_,_,_) = x
at92 (_,x,_,_,_,_,_,_,_) = x
at93 (_,_,x,_,_,_,_,_,_) = x
at94 (_,_,_,x,_,_,_,_,_) = x
at95 (_,_,_,_,x,_,_,_,_) = x
at96 (_,_,_,_,_,x,_,_,_) = x
at97 (_,_,_,_,_,_,x,_,_) = x
at98 (_,_,_,_,_,_,_,x,_) = x
at99 (_,_,_,_,_,_,_,_,x) = x


infixl 3 |<, |<<, |<<<, |<<<<, |<<<<<, |<<<<<<, |<<<<<<<, |<<<<<<<<
(|<)        x = (at21 <$> x,
                 at22 <$> x)
(|<<)       x = (at31 <$> x,
                 at32 <$> x,
                 at33 <$> x)
(|<<<)      x = (at41 <$> x,
                 at42 <$> x,
                 at43 <$> x,
                 at44 <$> x)
(|<<<<)     x = (at51 <$> x,
                 at52 <$> x,
                 at53 <$> x,
                 at54 <$> x,
                 at55 <$> x)
(|<<<<<)    x = (at61 <$> x,
                 at62 <$> x,
                 at63 <$> x,
                 at64 <$> x,
                 at65 <$> x,
                 at66 <$> x)
(|<<<<<<)   x = (at71 <$> x,
                 at72 <$> x,
                 at73 <$> x,
                 at74 <$> x,
                 at75 <$> x,
                 at76 <$> x,
                 at77 <$> x)
(|<<<<<<<)  x = (at81 <$> x,
                 at82 <$> x,
                 at83 <$> x,
                 at84 <$> x,
                 at85 <$> x,
                 at86 <$> x,
                 at87 <$> x,
                 at88 <$> x)
(|<<<<<<<<) x = (at91 <$> x,
                 at92 <$> x,
                 at93 <$> x,
                 at94 <$> x,
                 at95 <$> x,
                 at96 <$> x,
                 at97 <$> x,
                 at98 <$> x,
                 at99 <$> x)  


infixl 3 ||<, ||<<, ||<<<, ||<<<<, ||<<<<<, ||<<<<<<, ||<<<<<<<, ||<<<<<<<<
(||<)        s = ((|<) <$> s        |<)
(||<<)       s = ((|<<) <$> s       |<<)
(||<<<)      s = ((|<<<) <$> s      |<<<)
(||<<<<)     s = ((|<<<<) <$> s     |<<<<)
(||<<<<<)    s = ((|<<<<<) <$> s    |<<<<<)
(||<<<<<<)   s = ((|<<<<<<) <$> s   |<<<<<<)
(||<<<<<<<)  s = ((|<<<<<<<) <$> s  |<<<<<<<)
(||<<<<<<<<) s = ((|<<<<<<<<) <$> s |<<<<<<<<)


infixl 3 |||<,  |||<<, |||<<<, |||<<<<, |||<<<<<, |||<<<<<<, |||<<<<<<<, |||<<<<<<<<
(|||<)        s = (((|<) <$>) <$> s        ||<)
(|||<<)       s = (((|<<) <$>) <$> s       ||<<)
(|||<<<)      s = (((|<<<) <$>) <$> s      ||<<<)
(|||<<<<)     s = (((|<<<<) <$>) <$> s     ||<<<<)
(|||<<<<<)    s = (((|<<<<<) <$>) <$> s    ||<<<<<)
(|||<<<<<<)   s = (((|<<<<<<) <$>) <$> s   ||<<<<<<)
(|||<<<<<<<)  s = (((|<<<<<<<) <$>) <$> s  ||<<<<<<<)
(|||<<<<<<<<) s = (((|<<<<<<<<) <$>) <$> s ||<<<<<<<<)


infixl 3 ||||<,  ||||<<, ||||<<<, ||||<<<<, ||||<<<<<, ||||<<<<<<, ||||<<<<<<<, ||||<<<<<<<<
(||||<)        s = ((((|<) <$>) <$>) <$> s        |||<)
(||||<<)       s = ((((|<<) <$>) <$>) <$> s       |||<<)
(||||<<<)      s = ((((|<<<) <$>) <$>) <$> s      |||<<<)
(||||<<<<)     s = ((((|<<<<) <$>) <$>) <$> s     |||<<<<)
(||||<<<<<)    s = ((((|<<<<<) <$>) <$>) <$> s    |||<<<<<)
(||||<<<<<<)   s = ((((|<<<<<<) <$>) <$>) <$> s   |||<<<<<<)
(||||<<<<<<<)  s = ((((|<<<<<<<) <$>) <$>) <$> s  |||<<<<<<<)
(||||<<<<<<<<) s = ((((|<<<<<<<<) <$>) <$>) <$> s |||<<<<<<<<)



infixl 6 <>,  <>>, <>>>, <>>>>, <>>>>>, <>>>>>>, <>>>>>>>, <>>>>>>>>
f <>        (a1, a2)                             = f a1 a2
f <>>       (a1, a2, a3)                         = f a1 a2 a3
f <>>>      (a1, a2, a3, a4)                     = f a1 a2 a3 a4
f <>>>>     (a1, a2, a3, a4, a5)                 = f a1 a2 a3 a4 a5
f <>>>>>    (a1, a2, a3, a4, a5, a6)             = f a1 a2 a3 a4 a5 a6
f <>>>>>>   (a1, a2, a3, a4, a5, a6, a7)         = f a1 a2 a3 a4 a5 a6 a7
f <>>>>>>>  (a1, a2, a3, a4, a5, a6, a7, a8)     = f a1 a2 a3 a4 a5 a6 a7 a8
f <>>>>>>>> (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f a1 a2 a3 a4 a5 a6 a7 a8 a9

infixl 6 $$, $$$, $$$$, $$$$$, $$$$$$, $$$$$$$, $$$$$$$$, $$$$$$$$$
(f1,f2)                      $$        (a1,a2)                      = (f1 a1, f2 a2)
(f1,f2,f3)                   $$$       (a1,a2,a3)                   = (f1 a1, f2 a2, f3 a3)
(f1,f2,f3,f4)                $$$$      (a1,a2,a3,a4)                = (f1 a1, f2 a2, f3 a3, f4 a4)
(f1,f2,f3,f4,f5)             $$$$$     (a1,a2,a3,a4,a5)             = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5)
(f1,f2,f3,f4,f5,f6)          $$$$$$    (a1,a2,a3,a4,a5,a6)          = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6)
(f1,f2,f3,f4,f5,f6,f7)       $$$$$$$   (a1,a2,a3,a4,a5,a6,a7)       = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6, f7 a7)
(f1,f2,f3,f4,f5,f6,f7,f8)    $$$$$$$$  (a1,a2,a3,a4,a5,a6,a7,a8)    = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6, f7 a7, f8 a8)
(f1,f2,f3,f4,f5,f6,f7,f8,f9) $$$$$$$$$ (a1,a2,a3,a4,a5,a6,a7,a8,a9) = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6, f7 a7, f8 a8, f9 a9)



id1 a = a
id2 a b = (a, b)
id3 a b c = (a, b, c)
id4 a b c d = (a, b, c, d)
id5 a b c d e = (a, b, c, d, e)
id6 a b c d e f = (a, b, c, d, e, f)
id7 a b c d e f g = (a, b, c, d, e, f, g)
id8 a b c d e f g h = (a, b, c, d, e, f, g, h)
id9 a b c d e f g h i = (a, b, c, d, e, f, g, h, i)



psi11 f a1                      = (f <$> a1)
psi12 f a1                      = (f <$> a1 |<)
psi13 f a1                      = (f <$> a1 |<<)
psi14 f a1                      = (f <$> a1 |<<<)
psi15 f a1                      = (f <$> a1 |<<<<)
psi16 f a1                      = (f <$> a1 |<<<<<)
psi17 f a1                      = (f <$> a1 |<<<<<<)
psi18 f a1                      = (f <$> a1 |<<<<<<<)
psi21 f a1 a2                   = (f <$> a1 <*> a2)
psi22 f a1 a2                   = (f <$> a1 <*> a2 |<)
psi23 f a1 a2                   = (f <$> a1 <*> a2 |<<)
psi24 f a1 a2                   = (f <$> a1 <*> a2 |<<<)
psi25 f a1 a2                   = (f <$> a1 <*> a2 |<<<<)
psi26 f a1 a2                   = (f <$> a1 <*> a2 |<<<<<)
psi27 f a1 a2                   = (f <$> a1 <*> a2 |<<<<<<)
psi28 f a1 a2                   = (f <$> a1 <*> a2 |<<<<<<<)
psi31 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3)
psi32 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3 |<)
psi33 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3 |<<)
psi34 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3 |<<<)
psi35 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3 |<<<<)
psi36 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3 |<<<<<)
psi37 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3 |<<<<<<)
psi38 f a1 a2 a3                = (f <$> a1 <*> a2 <*> a3 |<<<<<<<)
psi41 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4)
psi42 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4 |<)
psi43 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4 |<<)
psi44 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4 |<<<)
psi45 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4 |<<<<)
psi46 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4 |<<<<<)
psi47 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4 |<<<<<<)
psi48 f a1 a2 a3 a4             = (f <$> a1 <*> a2 <*> a3 <*> a4 |<<<<<<<)
psi51 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi52 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 |<)
psi53 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 |<<)
psi54 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 |<<<)
psi55 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 |<<<<)
psi56 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 |<<<<<)
psi57 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 |<<<<<<)
psi58 f a1 a2 a3 a4 a5          = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 |<<<<<<<)
psi61 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi62 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 |<)
psi63 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 |<<)
psi64 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 |<<<)
psi65 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 |<<<<)
psi66 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 |<<<<<)
psi67 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 |<<<<<<)
psi68 f a1 a2 a3 a4 a5 a6       = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 |<<<<<<<)
psi71 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi72 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 |<)
psi73 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 |<<)
psi74 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 |<<<)
psi75 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 |<<<<)
psi76 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 |<<<<<)
psi77 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 |<<<<<<)
psi78 f a1 a2 a3 a4 a5 a6 a7    = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 |<<<<<<<)
psi81 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi82 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 |<)
psi83 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 |<<)
psi84 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 |<<<)
psi85 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 |<<<<)
psi86 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 |<<<<<)
psi87 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 |<<<<<<)
psi88 f a1 a2 a3 a4 a5 a6 a7 a8 = (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 |<<<<<<<)




sequenceA1 f x1                         = f (sequenceA x1)
sequenceA2 f x1 x2                      = f (sequenceA x1) (sequenceA x2)
sequenceA3 f x1 x2 x3                   = f (sequenceA x1) (sequenceA x2) (sequenceA x3)
sequenceA4 f x1 x2 x3 x4                = f (sequenceA x1) (sequenceA x2) (sequenceA x3)
                                            (sequenceA x4)
sequenceA5 f x1 x2 x3 x4 x5             = f (sequenceA x1) (sequenceA x2) (sequenceA x3)
                                            (sequenceA x4) (sequenceA x5)
sequenceA6 f x1 x2 x3 x4 x5 x6          = f (sequenceA x1) (sequenceA x2) (sequenceA x3)
                                            (sequenceA x4) (sequenceA x5) (sequenceA x6)
sequenceA7 f x1 x2 x3 x4 x5 x6 x7       = f (sequenceA x1) (sequenceA x2) (sequenceA x3)
                                            (sequenceA x4) (sequenceA x5) (sequenceA x6)
                                            (sequenceA x7)
sequenceA8 f x1 x2 x3 x4 x5 x6 x7 x8    = f (sequenceA x1) (sequenceA x2) (sequenceA x3)
                                            (sequenceA x4) (sequenceA x5) (sequenceA x6)
                                            (sequenceA x7) (sequenceA x8)
sequenceA9 f x1 x2 x3 x4 x5 x6 x7 x8 x9 = f (sequenceA x1) (sequenceA x2) (sequenceA x3)
                                            (sequenceA x4) (sequenceA x5) (sequenceA x6)
                                            (sequenceA x7) (sequenceA x8) (sequenceA x9)


-- assert c s a | not c     = error s
--              | otherwise = a

-- assertBin1 op lhs rhs = assert (lhs `op` rhs)
-- assertBin2 op lhs rhs msg as = (assert,assert) $$ ((op,op) $$ lhs $$ rhs) $$ (msg,msg) $$  as
-- assertBin3 op lhs rhs msg as = (assert,assert,assert) $$$ ((op,op,op) $$$ lhs $$$ rhs) $$$ (msg,msg,msg) $$$ as
-- assertBin4 op lhs rhs msg as = (assert,assert,assert,assert) $$$$ ((op,op,op,op) $$$$ lhs $$$$ rhs)
--                                $$$$ (msg,msg,msg,msg) $$$$ as
