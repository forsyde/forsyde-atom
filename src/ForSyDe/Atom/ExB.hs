{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK prune #-}
----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.ExB
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the core entities of the extended behavior
-- layer: interfaces for atoms and common patterns of atoms. It does
-- /NOT/ export any implementation or instantiation of any specific
-- behavior extension type.
--
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
----------------------------------------------------------------------------
module ForSyDe.Atom.ExB (

  -- * Atoms

  ExB (..),
  
  -- * Patterns

  resolution11, resolution12, resolution13, resolution14,
  resolution21, resolution22, resolution23, resolution24,
  resolution31, resolution32, resolution33, resolution34,
  resolution41, resolution42, resolution43, resolution44,
  resolution51, resolution52, resolution53, resolution54,
  resolution61, resolution62, resolution63, resolution64,
  resolution71, resolution72, resolution73, resolution74,
  resolution81, resolution82, resolution83, resolution84,

  filter, filter',

  degrade,

  ignore11, ignore12, ignore13, ignore14,
  ignore21, ignore22, ignore23, ignore24,
  ignore31, ignore32, ignore33, ignore34,
  ignore41, ignore42, ignore43, ignore44
  
  ) where

import Prelude hiding (filter)
import ForSyDe.Atom.Utility

infixl 4 /.\, /*\, /&\, /!\

-- | Class which defines the atoms for the extended behavior layer.
--
-- As its name suggests, this layer is extending the behavior of
-- processes (or merely of functions if we ignore timing semantics
-- completely), by expanding the domains of the wrapped layer
-- (e.g. the set of values) with symbols having clearly-defined
-- semantics (e.g. special events with known responses).
--
-- The types associated with this layer can simply be describes as:
--
-- <<docfiles/figs/eqs-exb-types.png>>
--
-- where  /&#945;/ is a base type and /b/ is the type extension,
-- i.e. a set of symbols with clearly-defined semantics.
--
-- Extended behavior atoms are functions of these types, defined as
-- interfaces in the 'ExB' type class.
class Functor b => ExB b where
  -- | Extends a value (from a layer below) with a set of symbols with
  -- known semantics, as described by a type instantiating this class.
  extend :: a -> b a

  -- | Basic functor operator. Lifts a function (from a layer below)
  -- into the domain of the extended behavior layer.
  --
  -- <<docfiles/figs/eqs-exb-atom-func.png>>
  (/.\) :: (a -> a') -> b a -> b a'

  -- | Applicative operator. Defines a resolution between two extended
  -- behavior symbols.
  --
  -- <<docfiles/figs/eqs-exb-atom-app.png>>
  (/*\) :: b (a -> a') -> b a -> b a'

  -- | Predicate operator. Generates a defined behavior based on an
  -- extended boolean predicate.
  --
  -- <<docfiles/figs/eqs-exb-atom-phi.png>>
  (/&\) :: b Bool -> b a -> b a

  -- | Degrade operator. Degrades a behavior-extended value into a
  -- non-extended one (from a layer below), based on a kernel
  -- value. Used also to throw exceptions.
  --
  -- <<docfiles/figs/eqs-exb-atom-deg.png>>
  (/!\) :: a -> b a -> a



-- |  
-- <<docfiles/figs/eqs-exb-pattern-resolution.png>>
--
-- The @resolution@ behavior pattern lifts a function on values to the
-- extended behavior domain, and applies a resolution between two
-- extended behavior symbols.
--
-- These are the functions exported:
--
-- > resolution11, resolution12, resolution13, resolution14,
-- > resolution21, resolution22, resolution23, resolution24,
-- > resolution31, resolution32, resolution33, resolution34,
-- > resolution41, resolution42, resolution43, resolution44,
-- > resolution51, resolution52, resolution53, resolution54,
-- > resolution61, resolution62, resolution63, resolution64,
-- > resolution71, resolution72, resolution73, resolution74,
-- > resolution81, resolution82, resolution83, resolution84,
resolution22 :: ExB b
  => (a1 -> a2 -> (a1', a2')) -- ^ function on values
  -> b a1                     -- ^ first input
  -> b a2                     -- ^ second input
  -> (b a1', b a2')           -- ^ tupled output
  
resolution11 f b1                      = (f /.\ b1)
resolution21 f b1 b2                   = (f /.\ b1 /*\ b2)
resolution31 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3)
resolution41 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4)
resolution51 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5)
resolution61 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6)
resolution71 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7)
resolution81 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 /*\ b8)
resolution12 f b1                      = (f /.\ b1 |<)
resolution22 f b1 b2                   = (f /.\ b1 /*\ b2 |<)
resolution32 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3 |<)
resolution42 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 |<)
resolution52 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 |<)
resolution62 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 |<)
resolution72 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 |<)
resolution82 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b5 /*\ b8 |<)
resolution13 f b1                      = (f /.\ b1 |<<)
resolution23 f b1 b2                   = (f /.\ b1 /*\ b2 |<<)
resolution33 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3 |<<)
resolution43 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 |<<)
resolution53 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 |<<)
resolution63 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 |<<)
resolution73 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 |<<)
resolution83 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b5 /*\ b8 |<<)
resolution14 f b1                      = (f /.\ b1 |<<<)
resolution24 f b1 b2                   = (f /.\ b1 /*\ b2 |<<<)
resolution34 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3 |<<<)
resolution44 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 |<<<)
resolution54 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 |<<<)
resolution64 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 |<<<)
resolution74 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 |<<<)
resolution84 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 /*\ b8 |<<<)

-- | Prefix name for the prefix operator '/&\'.
filter  p    = (/&\) p

-- | Same as 'filter' but takes base (non-extended) values as
-- input arguments.
filter' p a  = (/&\) (extend p) (extend a)

-- | Prefix name for the degrade operator '/!\'.
degrade a = (/!\) a

-- | 
-- <<docfiles/figs/eqs-exb-pattern-ignore.png>>
--
-- The @ignoreXY@ pattern takes a funcion of @Y + X@ arguments, @Y@
-- basic inputs followed by @X@ behavior-extended inputs. The @X@
-- behavior-extended arguments are subjugated to a resolution, and the
-- result is then degraded using the first @Y@ arguments as
-- fallback. The effect is similar to "ignoring" a the result of a
-- resolution function if &#8712; /b/.
--
-- The main application of this pattern is as extended behavior
-- wrapper for state machine functions which do not "understand"
-- extended behavior semantics, i.e. it simply propagates the current
-- state (&#8712; /&#945;/) if the inputs (their resolution) belongs
-- to the set of extended values (&#8712; /b/).
--
-- You can choose from the following exported functions:
--
-- > ignore11, ignore12, ignore13, ignore14,
-- > ignore21, ignore22, ignore23, ignore24,
-- > ignore31, ignore32, ignore33, ignore34,
-- > ignore41, ignore42, ignore43, ignore44,
ignore22 :: ExB b
         => (a1 -> a2 -> a1' -> a2' -> (a1, a2))
         -- ^ function of @Y + X@ arguments
         -> a1 -> a2 -> b a1' -> b a2' -> (a1, a2)
     
ignore11 f a1 b1          = degrade a1 $ resolution11 (f a1) b1
ignore21 f a1 b1 b2       = degrade a1 $ resolution21 (f a1) b1 b2
ignore31 f a1 b1 b2 b3    = degrade a1 $ resolution31 (f a1) b1 b2 b3
ignore41 f a1 b1 b2 b3 b4 = degrade a1 $ resolution41 (f a1) b1 b2 b3 b4
ignore12 f a1 a2 b1          = degrade (a1, a2) $ resolution11 (f a1 a2) b1
ignore22 f a1 a2 b1 b2       = degrade (a1, a2) $ resolution21 (f a1 a2) b1 b2
ignore32 f a1 a2 b1 b2 b3    = degrade (a1, a2) $ resolution31 (f a1 a2) b1 b2 b3
ignore42 f a1 a2 b1 b2 b3 b4 = degrade (a1, a2) $ resolution41 (f a1 a2) b1 b2 b3 b4
ignore13 f a1 a2 a3 b1          = degrade (a1, a2, a3) $ resolution11 (f a1 a2 a3) b1
ignore23 f a1 a2 a3 b1 b2       = degrade (a1, a2, a3) $ resolution21 (f a1 a2 a3) b1 b2
ignore33 f a1 a2 a3 b1 b2 b3    = degrade (a1, a2, a3) $ resolution31 (f a1 a2 a3) b1 b2 b3
ignore43 f a1 a2 a3 b1 b2 b3 b4 = degrade (a1, a2, a3) $ resolution41 (f a1 a2 a3) b1 b2 b3 b4
ignore14 f a1 a2 a3 a4 b1          = degrade (a1, a2, a3, a4) $ resolution11 (f a1 a2 a3 a4) b1
ignore24 f a1 a2 a3 a4 b1 b2       = degrade (a1, a2, a3, a4) $ resolution21 (f a1 a2 a3 a4) b1 b2
ignore34 f a1 a2 a3 a4 b1 b2 b3    = degrade (a1, a2, a3, a4) $ resolution31 (f a1 a2 a3 a4) b1 b2 b3
ignore44 f a1 a2 a3 a4 b1 b2 b3 b4 = degrade (a1, a2, a3, a4) $ resolution41 (f a1 a2 a3 a4) b1 b2 b3 b4
