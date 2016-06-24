{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Behavior.Cons
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016;
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the atoms and wrappers for the behavior
-- layer in ForSyDe processes.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Behavior.Cons where

import ForSyDe.Atom.Behavior.Atom
import ForSyDe.Atom.Behavior.ValueExt
import ForSyDe.Atom.Utility

psi11 f a1                      = (f >$ a1)
psi12 f a1                      = (f >$ a1 |<)
psi13 f a1                      = (f >$ a1 |<<)
psi14 f a1                      = (f >$ a1 |<<<)
psi15 f a1                      = (f >$ a1 |<<<<)
psi16 f a1                      = (f >$ a1 |<<<<<)
psi17 f a1                      = (f >$ a1 |<<<<<<)
psi18 f a1                      = (f >$ a1 |<<<<<<<)
psi21 f a1 a2                   = (f >$ a1 >* a2)
psi22 f a1 a2                   = (f >$ a1 >* a2 |<)
psi23 f a1 a2                   = (f >$ a1 >* a2 |<<)
psi24 f a1 a2                   = (f >$ a1 >* a2 |<<<)
psi25 f a1 a2                   = (f >$ a1 >* a2 |<<<<)
psi26 f a1 a2                   = (f >$ a1 >* a2 |<<<<<)
psi27 f a1 a2                   = (f >$ a1 >* a2 |<<<<<<)
psi28 f a1 a2                   = (f >$ a1 >* a2 |<<<<<<<)
psi31 f a1 a2 a3                = (f >$ a1 >* a2 >* a3)
psi32 f a1 a2 a3                = (f >$ a1 >* a2 >* a3 |<)
psi33 f a1 a2 a3                = (f >$ a1 >* a2 >* a3 |<<)
psi34 f a1 a2 a3                = (f >$ a1 >* a2 >* a3 |<<<)
psi35 f a1 a2 a3                = (f >$ a1 >* a2 >* a3 |<<<<)
psi36 f a1 a2 a3                = (f >$ a1 >* a2 >* a3 |<<<<<)
psi37 f a1 a2 a3                = (f >$ a1 >* a2 >* a3 |<<<<<<)
psi38 f a1 a2 a3                = (f >$ a1 >* a2 >* a3 |<<<<<<<)
psi41 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4)
psi42 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4 |<)
psi43 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4 |<<)
psi44 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4 |<<<)
psi45 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4 |<<<<)
psi46 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4 |<<<<<)
psi47 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4 |<<<<<<)
psi48 f a1 a2 a3 a4             = (f >$ a1 >* a2 >* a3 >* a4 |<<<<<<<)
psi51 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5)
psi52 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5 |<)
psi53 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5 |<<)
psi54 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5 |<<<)
psi55 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5 |<<<<)
psi56 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5 |<<<<<)
psi57 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5 |<<<<<<)
psi58 f a1 a2 a3 a4 a5          = (f >$ a1 >* a2 >* a3 >* a4 >* a5 |<<<<<<<)
psi61 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6)
psi62 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 |<)
psi63 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 |<<)
psi64 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 |<<<)
psi65 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 |<<<<)
psi66 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 |<<<<<)
psi67 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 |<<<<<<)
psi68 f a1 a2 a3 a4 a5 a6       = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 |<<<<<<<)
psi71 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7)
psi72 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 |<)
psi73 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 |<<)
psi74 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 |<<<)
psi75 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 |<<<<)
psi76 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 |<<<<<)
psi77 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 |<<<<<<)
psi78 f a1 a2 a3 a4 a5 a6 a7    = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 |<<<<<<<)
psi81 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8)
psi82 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8 |<)
psi83 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8 |<<)
psi84 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8 |<<<)
psi85 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8 |<<<<)
psi86 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8 |<<<<<)
psi87 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8 |<<<<<<)
psi88 f a1 a2 a3 a4 a5 a6 a7 a8 = (f >$ a1 >* a2 >* a3 >* a4 >* a5 >* a6 >* a7 >* a8 |<<<<<<<)


store1 buff a1                      = at22 $ ((:), buff) >%! a1
store2 buff a1 a2                   = at22 $ ((:), buff) >%! a1 >%! a2
store3 buff a1 a2 a3                = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3
store4 buff a1 a2 a3 a4             = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4
store5 buff a1 a2 a3 a4 a5          = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5
store6 buff a1 a2 a3 a4 a5 a6       = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5 >%! a6
store7 buff a1 a2 a3 a4 a5 a6 a7    = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5 >%! a6 >%! a7
store8 buff a1 a2 a3 a4 a5 a6 a7 a8 = at22 $ ((:), buff) >%! a1 >%! a2 >%! a3 >%! a4 >%! a5 >%! a6 >%! a7 >%! a8


reduce2 f a1 a2                   = at22 $ (f, a1) >% a2
reduce3 f a1 a2 a3                = at22 $ (f, a1) >% a2 >% a3
reduce4 f a1 a2 a3 a4             = at22 $ (f, a1) >% a2 >% a3 >% a4
reduce5 f a1 a2 a3 a4 a5          = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5
reduce6 f a1 a2 a3 a4 a5 a6       = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5 >% a6
reduce7 f a1 a2 a3 a4 a5 a6 a7    = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5 >% a6 >% a7
reduce8 f a1 a2 a3 a4 a5 a6 a7 a8 = at22 $ (f, a1) >% a2 >% a3 >% a4 >% a5 >% a6 >% a7 >% a8


replaceV       v p x = p >#  (x,v)
replaceU         p x = p >#  (x,Undef)
replaceA         p x = p >#  (x,Abst)
unsafeReplaceV v p x = p >#! (x,v)
unsafeReplaceU   p x = p >#! (x,Undef)
