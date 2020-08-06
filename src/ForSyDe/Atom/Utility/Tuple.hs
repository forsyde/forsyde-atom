{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK prune, show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Utility.Tuple
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements general purpose utility functions. It mainly hosts functions
-- dealing with tuples. Utility are provided for up until 9-tuples. Follow the
-- examples in the source code in case it does not suffice.
--
-- __IMPORTANT!!!__ Most of the multi-parameter higher-order functions provided by the
-- library API are named along the lines of @functionMN@ where @M@ represents the
-- number of __/curried/__ inputs (i.e. @a1 -> a2 -> ... -> aM@), while @N@ represents
-- the number of __/tupled/__ outputs (i.e. @(b1,b2,...,bN)@). To avoid repetition we
-- document only functions with 2 inputs and 2 outputs (i.e. @function22@).
-----------------------------------------------------------------------------

module ForSyDe.Atom.Utility.Tuple(
  
  at21, at22,
  at31, at32, at33,
  at41, at42, at43, at44,
  at51, at52, at53, at54, at55,
  at61, at62, at63, at64, at65, at66, 
  at71, at72, at73, at74, at75, at76, at77,
  at81, at82, at83, at84, at85, at86, at87, at88,
  at91, at92, at93, at94, at95, at96, at97, at98, at99,
  
     (|<),    (|<<),    (|<<<),    (|<<<<),    (|<<<<<),    (|<<<<<<),    (|<<<<<<<),    (|<<<<<<<<),
    (||<),   (||<<),   (||<<<),   (||<<<<),   (||<<<<<),   (||<<<<<<),   (||<<<<<<<),   (||<<<<<<<<),
   (|||<),  (|||<<),  (|||<<<),  (|||<<<<),  (|||<<<<<),  (|||<<<<<<),  (|||<<<<<<<),  (|||<<<<<<<<),  
  (||||<), (||||<<), (||||<<<), (||||<<<<), (||||<<<<<), (||||<<<<<<), (||||<<<<<<<), (||||<<<<<<<<), 

  (><),
  (><<),
  (><<<),
  (><<<<),
  (><<<<<),
  (><<<<<<),
  (><<<<<<<),
  (><<<<<<<<),

  ($$),
  ($$$),
  ($$$$),
  ($$$$$),
  ($$$$$$),
  ($$$$$$$),
  ($$$$$$$$),
  ($$$$$$$$$),
  
  ) where

import Prelude hiding (unzip3, filter, (<>))

-- | The @at@/xy/ functions return the /y/-th element of an /x/-tuple.
--
-- "ForSyDe.Atom.Utility" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > at21, at22,
-- > at31, at32, at33,
-- > at41, at42, at43, at44,
-- > at51, at52, at53, at54, at55,
-- > at61, at62, at63, at64, at65, at66, 
-- > at71, at72, at73, at74, at75, at76, at77,
-- > at81, at82, at83, at84, at85, at86, at87, at88,
-- > at91, at92, at93, at94, at95, at96, at97, at98, at99,
--
-- Example:
--
-- >>>  at53 (1,2,3,4,5)
-- 3
at22 :: (a1, a2) -> a2

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


-- | This set of utility functions "unzip" nested n-tuples, provided
-- as postfix operators. They are crucial for reconstructing data
-- types from higher-order functions which input functions with
-- multiple outputs. It relies on the nested types being instances of
-- 'Prelude.Functor'.
--
-- The operator convention is @(|+<+)@, where the number of @|@
-- represent the number of layers the n-tuple is lifted, while the
-- number of @<@ + 1 is the order /n/ of the n-tuple.
--
-- "ForSyDe.Atom.Utility" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- >   |<,    |<<,    |<<<,    |<<<<,    |<<<<<,    |<<<<<<,    |<<<<<<<,    |<<<<<<<<,
-- >  ||<,   ||<<,   ||<<<,   ||<<<<,   ||<<<<<,   ||<<<<<<,   ||<<<<<<<,   ||<<<<<<<<,
-- > |||<,  |||<<,  |||<<<,  |||<<<<,  |||<<<<<,  |||<<<<<<,  |||<<<<<<<,  |||<<<<<<<<,  
-- >||||<, ||||<<, ||||<<<, ||||<<<<, ||||<<<<<, ||||<<<<<<, ||||<<<<<<<, ||||<<<<<<<<, 
--
-- Example:
--
-- >>> :set -XPostfixOperators
-- >>> ([Just (1,2,3), Nothing, Just (4,5,6)] ||<<)
-- ([Just 1,Nothing,Just 4],[Just 2,Nothing,Just 5],[Just 3,Nothing,Just 6])
(||<) :: (Functor f1, Functor f2) => f1 (f2 (a1, a2)) -> (f1 (f2 a1), f1 (f2 a2))

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


-- | Infix currying operators used for convenience. 
--
-- The operator convention is @(><+)@, where the number of @<@ + 1 is
-- the order /n/ of the n-tuple.
--
-- "ForSyDe.Atom.Utility" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > ><, ><<, ><<<, ><<<<, ><<<<<, ><<<<<<, ><<<<<<<, ><<<<<<<<
--
-- Example:
--
-- >>> (+) >< (1,2)
-- 3
(><) :: (a1 -> a2 -> b1) -> (a1, a2) -> b1

infixl 6 ><,  ><<, ><<<, ><<<<, ><<<<<, ><<<<<<, ><<<<<<<, ><<<<<<<<
f ><        (a1, a2)                             = f a1 a2
f ><<       (a1, a2, a3)                         = f a1 a2 a3
f ><<<      (a1, a2, a3, a4)                     = f a1 a2 a3 a4
f ><<<<     (a1, a2, a3, a4, a5)                 = f a1 a2 a3 a4 a5
f ><<<<<    (a1, a2, a3, a4, a5, a6)             = f a1 a2 a3 a4 a5 a6
f ><<<<<<   (a1, a2, a3, a4, a5, a6, a7)         = f a1 a2 a3 a4 a5 a6 a7
f ><<<<<<<  (a1, a2, a3, a4, a5, a6, a7, a8)     = f a1 a2 a3 a4 a5 a6 a7 a8
f ><<<<<<<< (a1, a2, a3, a4, a5, a6, a7, a8, a9) = f a1 a2 a3 a4 a5 a6 a7 a8 a9


-- | Infix function application operator for tuples. 
--
-- The operator convention is @($+)@, where the number of @$@ is the
-- order /n/ of the n-tuple. For Applying a function on nontuples we
-- rely on 'Prelude.$' provided by "Prelude".
--
-- "ForSyDe.Atom.Utility" exports the constructors below. Please
-- follow the examples in the source code if they do not suffice:
--
-- > $$, $$$, $$$$, $$$$$, $$$$$$, $$$$$$$, $$$$$$$$, $$$$$$$$$
--
-- Example:
--
-- >>> ((+),(-)) $$ (1,1) $$ (2,2) 
-- (3,-1)
($$) :: (a1 -> b1, a2 -> b2) -> (a1, a2) -> (b1, b2)

infixl 6 $$, $$$, $$$$, $$$$$, $$$$$$, $$$$$$$, $$$$$$$$, $$$$$$$$$
(f1,f2)                      $$        (a1,a2)                      = (f1 a1, f2 a2)
(f1,f2,f3)                   $$$       (a1,a2,a3)                   = (f1 a1, f2 a2, f3 a3)
(f1,f2,f3,f4)                $$$$      (a1,a2,a3,a4)                = (f1 a1, f2 a2, f3 a3, f4 a4)
(f1,f2,f3,f4,f5)             $$$$$     (a1,a2,a3,a4,a5)             = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5)
(f1,f2,f3,f4,f5,f6)          $$$$$$    (a1,a2,a3,a4,a5,a6)          = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6)
(f1,f2,f3,f4,f5,f6,f7)       $$$$$$$   (a1,a2,a3,a4,a5,a6,a7)       = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6, f7 a7)
(f1,f2,f3,f4,f5,f6,f7,f8)    $$$$$$$$  (a1,a2,a3,a4,a5,a6,a7,a8)    = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6, f7 a7, f8 a8)
(f1,f2,f3,f4,f5,f6,f7,f8,f9) $$$$$$$$$ (a1,a2,a3,a4,a5,a6,a7,a8,a9) = (f1 a1, f2 a2, f3 a3, f4 a4, f5 a5, f6 a6, f7 a7, f8 a8, f9 a9)

