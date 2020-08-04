{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skel
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the Skeleton layer, and is concerned in modeling the aspects of
-- inherent potential for parallelism in CPS. Its formal foundation is the theory of
-- algorithmic skeletons <ForSyDe-Atom.html#skillicorn05 [Skillicorn05]>, and it
-- adapts it according to the atom approach. For more on our approach, and how it fits
-- into the layered framework, please consult
-- <ForSyDe-Atom.html#ungureanu20a [Ungureanu20a]>
--
-- This library is concerned in modeling the theoretical aspects as faithfully as
-- possible using a shallow DSL, and not necessarily reaping the benefits of
-- parallelization directly in simulations. The latter would require further
-- engineering and the modeling concepts are likely to be lost the process. Instead
-- most of the atom and pattern formulations in this module and sub-modules might not
-- be the most efficient implementations for the given functionality, but rather
-- expose the fundamental properties which can be further exploited in design
-- processes. Of these properties our special interest lies in the /factorization/
-- theorem (see <ForSyDe-Atom.html#skillicorn05 [Skillicorn05]>,
-- <ForSyDe-Atom.html#gorlatch03 [Gorlatch03]>), which sets the framework for any
-- skeleton to be further transformed into semantically-equivalent forms, more
-- appropriate for execution on various platforms.
--
-- Similar to other layer libraries, this module defines only atoms and patterns as
-- type class methods, i.e. "shells" which are not loaded with any semantics. It is
-- unlikely that the user will need this API, but rather load any of its sub-modules
-- directly:
--
-- * "ForSyDe.Atom.Skel.Vector" is a shallow interpretation of the /vector/
--   category, susceptible to algorithmic skeletons. It defines a large library of
--   patterns commonly used in designs.
--
-- * "ForSyDe.Atom.Skel.FastVector" is an un-official alternative to
--   "ForSyDe.Atom.Skel.Vector" meant for simulations of large data which is
--   likely to become too cumbersome. It /does not/ use atoms, but rather it wraps
--   native Haskell types into @newtype@ wrappers and uses "Prelude" functions
--   internally. The API tries to copy that of "ForSyDe.Atom.Skel.Vector" so that
--   switching betwen libraries can be made seamlessly just by changing the import.
-------------------------------------------------------------------------------

module ForSyDe.Atom.Skel (

  -- * Atoms
  
  Skeleton(..),

  -- * Skeleton constructors

  -- | Patterns of in the skeleton layer are provided, like all other patterns in
  -- ForSyDe-Atom, as constructors. If the layer below this one is the
  -- 'ForSyDe.Atom.MoC.MoC' layer, i.e. the functions taken as arguments are
  -- processes, then these skeletons can be regarded as process network constructors,
  -- as the structures created are process networks with inherent potential for
  -- parallel implementation.
  
  farm11, farm12, farm13, farm14,
  farm21, farm22, farm23, farm24,
  farm31, farm32, farm33, farm34,
  farm41, farm42, farm43, farm44,
  farm51, farm52, farm53, farm54,
  farm61, farm62, farm63, farm64,
  farm71, farm72, farm73, farm74,
  farm81, farm82, farm83, farm84,

  reduce, reducei, pipe,
  
  pipe1, pipe2, pipe3, pipe4,
  pipe5, pipe6, pipe7, pipe8,
  
  ) where

import ForSyDe.Atom.Utility.Tuple

infixl 4 =.=, =*=
infixl 2 =\=, =<<=

-- | Class containing all the Skeleton layer atoms.
--
-- This class is instantiated by a set of categorical types, i.e. types which describe
-- an inherent potential for being evaluated in parallel. Skeletons are patterns from
-- this layer. All skeletons can be described as composition of the three atoms
-- below. This possible due to an existing theorem in the categorical type theory,
-- also called the Bird-Merteens formalism:
--
-- #factorization#
--
-- [factorization] A function on a categorical type is an algorithmic skeleton
-- (i.e. catamorphism) /iff/ it can be represented in a factorized form, i.e. as a
-- /map/ composed with a /reduce/.
class Functor c => Skeleton c where
  -- | Atom which maps a function on each element of a structure
  -- (i.e. categorical type), defined as:
  --
  -- <<fig/eqs-skel-atom-dot.png>>
  --
  -- '=.=' together with '=*=' form the @map@ pattern.
  (=.=)  :: (a -> b) -> c a -> c b

  -- | Atom which applies the functions contained by as structure (i.e. categorical
  -- type), on the elements of another structure, defined as:
  --
  -- <<fig/eqs-skel-atom-star.png>>
  --
  -- '=.=' together with '=*=' form the @map@ pattern.
  (=*=)  :: c (a -> b) -> c a -> c b

  -- | Atom which reduces a structure to an element based on an /associative/
  -- function, defined as:
  --
  -- <<fig/eqs-skel-atom-red.png>>
  (=\=)  :: (a -> a -> a) -> c a -> a

  -- | Skeleton which /pipes/ an element through all the functions contained by a
  -- structure. This is not an atom. It has an implicit definition which might be
  -- augmented by instances of this class to include edge cases.
  --
  -- <<fig/eqs-skel-pattern-pipe.png>>
  --
  -- As the composition operation is not associative, we cannot treat @pipe@ as a true
  -- reduction. However it can still be exploited in parallel since it exposes another
  -- type of parallelism: time parallelism.
  (=<<=) :: c (a -> a)  -- ^ vector of functions
         -> a           -- ^ kernel element
         -> a           -- ^ result 
  (=<<=) ps = (.) =\= ps

            
  -- | Returns the first element in a structure. This is not an atom. It has an
  -- implicit definition which might be replaced by instances of this class with a
  -- more efficient implementation.
  --
  -- <<fig/eqs-skel-pattern-first.png>>
  first :: c a -> a
  first v = (\x y -> x) =\= v

  -- | Returns the last element in a structure. This is not an atom. It has an
  -- implicit definition which might be replaced by instances of this class with a
  -- more efficient implementation.
  --
  -- <<fig/eqs-skel-pattern-last.png>>
  last :: c a -> a
  last v = (\x y -> y) =\= v

--------------
-- PATTERNS --
--------------

-- | @farm@ maps a function on a vector. It is the embodiment of the
-- @map@ homomorphism, and its naming is inspired from the pattern
-- predominant in HPC. Indeed, if we consider the layer below as being
-- the 'ForSyDe.Atom.MoC.MoC' layer (i.e. the passed functions are
-- processes), the resulting structure could be regarded as a "farm of
-- data-parallel processes".
--
-- Constructors: @farm[1-8][1-4]@.
--
-- <<fig/eqs-skel-pattern-farm.png>>
-- <<fig/skel-pattern-farm.png>>
farm22 :: Skeleton c => (a1 -> a2 -> (b1, b2)) -> c a1 -> c a2 -> (c b1, c b2)
farm11 p v1                      = (p =.= v1)
farm21 p v1 v2                   = (p =.= v1 =*= v2)
farm31 p v1 v2 v3                = (p =.= v1 =*= v2 =*= v3)
farm41 p v1 v2 v3 v4             = (p =.= v1 =*= v2 =*= v3 =*= v4)
farm51 p v1 v2 v3 v4 v5          = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5)
farm61 p v1 v2 v3 v4 v5 v6       = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6)
farm71 p v1 v2 v3 v4 v5 v6 v7    = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7)
farm81 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 =*= v8)
farm12 p v1                      = (p =.= v1 |<)
farm22 p v1 v2                   = (p =.= v1 =*= v2 |<)
farm32 p v1 v2 v3                = (p =.= v1 =*= v2 =*= v3 |<)
farm42 p v1 v2 v3 v4             = (p =.= v1 =*= v2 =*= v3 =*= v4 |<)
farm52 p v1 v2 v3 v4 v5          = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<)
farm62 p v1 v2 v3 v4 v5 v6       = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<)
farm72 p v1 v2 v3 v4 v5 v6 v7    = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<)
farm82 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v5 =*= v8 |<)
farm13 p v1                      = (p =.= v1 |<<)
farm23 p v1 v2                   = (p =.= v1 =*= v2 |<<)
farm33 p v1 v2 v3                = (p =.= v1 =*= v2 =*= v3 |<<)
farm43 p v1 v2 v3 v4             = (p =.= v1 =*= v2 =*= v3 =*= v4 |<<)
farm53 p v1 v2 v3 v4 v5          = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<<)
farm63 p v1 v2 v3 v4 v5 v6       = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<<)
farm73 p v1 v2 v3 v4 v5 v6 v7    = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<<)
farm83 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v5 =*= v8 |<<)
farm14 p v1                      = (p =.= v1 |<<<)
farm24 p v1 v2                   = (p =.= v1 =*= v2 |<<<)
farm34 p v1 v2 v3                = (p =.= v1 =*= v2 =*= v3 |<<<)
farm44 p v1 v2 v3 v4             = (p =.= v1 =*= v2 =*= v3 =*= v4 |<<<)
farm54 p v1 v2 v3 v4 v5          = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 |<<<)
farm64 p v1 v2 v3 v4 v5 v6       = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 |<<<)
farm74 p v1 v2 v3 v4 v5 v6 v7    = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 |<<<)
farm84 p v1 v2 v3 v4 v5 v6 v7 v8 = (p =.= v1 =*= v2 =*= v3 =*= v4 =*= v5 =*= v6 =*= v7 =*= v8 |<<<)

-- | Infix name for the '=\=' atom operator.
--
-- (*) if the operation is not associative then the network can be
-- treated like a pipeline.
reduce :: Skeleton c
       => (a -> a -> a) -- ^ associative function (*)
       -> c a           -- ^ structure
       -> a             -- ^ reduced element
reduce = (=\=)

-- | 'reducei' is special case of 'reduce' where an initial element is
-- specified outside the reduced vector. It is implemented as a
-- 'pipe' with switched arguments, and the reduction function is
-- constrained to be associative. It is semantically equivalent to the
-- pattern depicted below.
--
-- (*) if the operation is not associative then the network is
-- semantically equivalent to @pipe1@ (see 'pipe2').
--
-- <<fig/eqs-skel-pattern-reducei.png>>
-- <<fig/skel-pattern-reducei.png>>
reducei :: Skeleton c
        => (a -> a -> a) -- ^ associative function (*)
        -> a             -- ^ initial element of structure
        -> c a           -- ^ structure
        -> a             -- ^ reduced element
reducei p i v = farm11 p v =<<= i  


-- | Infix name for the '=<<=' skeleton operator.
pipe :: Skeleton c
     => c (a -> a)  -- ^ vector of functions
     -> a           -- ^ kernel element
     -> a           -- ^ result 
pipe = (=<<=)

-- | The @pipe@ constructors are a more generic form of the '=<<='
-- ('pipe') skeleton apt for successive partial application and create
-- more robust parameterizable pipeline networks.
--
-- Constructors: @comb[1-8]@.
--
-- <<fig/eqs-skel-pattern-pipe1.png>>
-- <<fig/skel-pattern-pipe1.png>>
pipe2 :: Skeleton c
      => (a1 -> a2 -> a -> a)
      -> c a1 -> c a2
      -> a -> a
pipe1 p v1 s                      = farm11 p v1 =<<= s
pipe2 p v1 v2 s                   = farm21 p v1 v2 =<<= s
pipe3 p v1 v2 v3 s                = farm31 p v1 v2 v3 =<<= s
pipe4 p v1 v2 v3 v4 s             = farm41 p v1 v2 v3 v4 =<<= s
pipe5 p v1 v2 v3 v4 v5 s          = farm51 p v1 v2 v3 v4 v5 =<<= s
pipe6 p v1 v2 v3 v4 v5 v6 s       = farm61 p v1 v2 v3 v4 v5 v6 =<<= s
pipe7 p v1 v2 v3 v4 v5 v6 v7 s    = farm71 p v1 v2 v3 v4 v5 v6 v7 =<<= s
pipe8 p v1 v2 v3 v4 v5 v6 v7 v8 s = farm81 p v1 v2 v3 v4 v5 v6 v7 v8 =<<= s




