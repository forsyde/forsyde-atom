{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Utilities
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements general purpose utility functions. It mainly
-- hosts functions dealing with tuples. Utilities are provided for up
-- until 9-tuples. Follow the examples in the source code in case it
-- does not suffice.
-----------------------------------------------------------------------------

module ForSyDe.Core.Utilities(
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
