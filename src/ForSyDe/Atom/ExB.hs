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

  res11, res12, res13, res14,
  res21, res22, res23, res24,
  res31, res32, res33, res34,
  res41, res42, res43, res44,
  res51, res52, res53, res54,
  res61, res62, res63, res64,
  res71, res72, res73, res74,
  res81, res82, res83, res84,

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
-- <<fig/eqs-exb-types.png>>
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
  -- <<fig/eqs-exb-atom-func.png>>
  (/.\) :: (a -> a') -> b a -> b a'

  -- | Applicative operator. Defines a res between two extended
  -- behavior symbols.
  --
  -- <<fig/eqs-exb-atom-app.png>>
  (/*\) :: b (a -> a') -> b a -> b a'

  -- | Predicate operator. Generates a defined behavior based on an
  -- extended Boolean predicate.
  --
  -- <<fig/eqs-exb-atom-phi.png>>
  (/&\) :: b Bool -> b a -> b a

  -- | Degrade operator. Degrades a behavior-extended value into a
  -- non-extended one (from a layer below), based on a kernel
  -- value. Used also to throw exceptions.
  --
  -- <<fig/eqs-exb-atom-deg.png>>
  (/!\) :: a -> b a -> a



-- |  
-- <<fig/eqs-exb-pattern-resolution.png>>
--
-- The @res@ behavior pattern lifts a function on values to the
-- extended behavior domain, and applies a resolution between two
-- extended behavior symbols.
--
-- These are the functions exported:
--
-- > res11, res12, res13, res14,
-- > res21, res22, res23, res24,
-- > res31, res32, res33, res34,
-- > res41, res42, res43, res44,
-- > res51, res52, res53, res54,
-- > res61, res62, res63, res64,
-- > res71, res72, res73, res74,
-- > res81, res82, res83, res84,
res22 :: ExB b
  => (a1 -> a2 -> (a1', a2')) -- ^ function on values
  -> b a1                     -- ^ first input
  -> b a2                     -- ^ second input
  -> (b a1', b a2')           -- ^ tupled output
  
res11 f b1                      = (f /.\ b1)
res21 f b1 b2                   = (f /.\ b1 /*\ b2)
res31 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3)
res41 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4)
res51 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5)
res61 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6)
res71 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7)
res81 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 /*\ b8)
res12 f b1                      = (f /.\ b1 |<)
res22 f b1 b2                   = (f /.\ b1 /*\ b2 |<)
res32 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3 |<)
res42 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 |<)
res52 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 |<)
res62 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 |<)
res72 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 |<)
res82 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b5 /*\ b8 |<)
res13 f b1                      = (f /.\ b1 |<<)
res23 f b1 b2                   = (f /.\ b1 /*\ b2 |<<)
res33 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3 |<<)
res43 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 |<<)
res53 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 |<<)
res63 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 |<<)
res73 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 |<<)
res83 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b5 /*\ b8 |<<)
res14 f b1                      = (f /.\ b1 |<<<)
res24 f b1 b2                   = (f /.\ b1 /*\ b2 |<<<)
res34 f b1 b2 b3                = (f /.\ b1 /*\ b2 /*\ b3 |<<<)
res44 f b1 b2 b3 b4             = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 |<<<)
res54 f b1 b2 b3 b4 b5          = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 |<<<)
res64 f b1 b2 b3 b4 b5 b6       = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 |<<<)
res74 f b1 b2 b3 b4 b5 b6 b7    = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 |<<<)
res84 f b1 b2 b3 b4 b5 b6 b7 b8 = (f /.\ b1 /*\ b2 /*\ b3 /*\ b4 /*\ b5 /*\ b6 /*\ b7 /*\ b8 |<<<)

-- | Prefix name for the prefix operator '/&\'.
filter  p    = (/&\) p

-- | Same as 'filter' but takes base (non-extended) values as
-- input arguments.
filter' p a  = (/&\) (extend p) (extend a)

-- | Prefix name for the degrade operator '/!\'.
degrade a = (/!\) a

-- | 
-- <<fig/eqs-exb-pattern-ignore.png>>
--
-- The @ignoreXY@ pattern takes a function of @Y + X@ arguments, @Y@
-- basic inputs followed by @X@ behavior-extended inputs. The @X@
-- behavior-extended arguments are subjugated to a res, and the
-- result is then degraded using the first @Y@ arguments as
-- fallback. The effect is similar to "ignoring" a the result of a
-- res function if &#8712; /b/.
--
-- The main application of this pattern is as extended behavior
-- wrapper for state machine functions which do not "understand"
-- extended behavior semantics, i.e. it simply propagates the current
-- state (&#8712; /&#945;/) if the inputs (their res) belongs
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
     
ignore11 f a1 b1          = degrade a1 $ res11 (f a1) b1
ignore21 f a1 b1 b2       = degrade a1 $ res21 (f a1) b1 b2
ignore31 f a1 b1 b2 b3    = degrade a1 $ res31 (f a1) b1 b2 b3
ignore41 f a1 b1 b2 b3 b4 = degrade a1 $ res41 (f a1) b1 b2 b3 b4
ignore12 f a1 a2 b1          = degrade (a1, a2) $ res11 (f a1 a2) b1
ignore22 f a1 a2 b1 b2       = degrade (a1, a2) $ res21 (f a1 a2) b1 b2
ignore32 f a1 a2 b1 b2 b3    = degrade (a1, a2) $ res31 (f a1 a2) b1 b2 b3
ignore42 f a1 a2 b1 b2 b3 b4 = degrade (a1, a2) $ res41 (f a1 a2) b1 b2 b3 b4
ignore13 f a1 a2 a3 b1          = degrade (a1, a2, a3) $ res11 (f a1 a2 a3) b1
ignore23 f a1 a2 a3 b1 b2       = degrade (a1, a2, a3) $ res21 (f a1 a2 a3) b1 b2
ignore33 f a1 a2 a3 b1 b2 b3    = degrade (a1, a2, a3) $ res31 (f a1 a2 a3) b1 b2 b3
ignore43 f a1 a2 a3 b1 b2 b3 b4 = degrade (a1, a2, a3) $ res41 (f a1 a2 a3) b1 b2 b3 b4
ignore14 f a1 a2 a3 a4 b1          = degrade (a1, a2, a3, a4) $ res11 (f a1 a2 a3 a4) b1
ignore24 f a1 a2 a3 a4 b1 b2       = degrade (a1, a2, a3, a4) $ res21 (f a1 a2 a3 a4) b1 b2
ignore34 f a1 a2 a3 a4 b1 b2 b3    = degrade (a1, a2, a3, a4) $ res31 (f a1 a2 a3 a4) b1 b2 b3
ignore44 f a1 a2 a3 a4 b1 b2 b3 b4 = degrade (a1, a2, a3, a4) $ res41 (f a1 a2 a3 a4) b1 b2 b3 b4
