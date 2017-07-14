{-# OPTIONS_HADDOCK prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skeleton.Vector
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the data type 'Vector' as a categorical type,
-- and implements the atoms for the 'ForSyDe.MoC.Skeleton.Skeleton'
-- class. Algorithmic skeletons for 'Vector' are mostly described in
-- their factorized form, which ensures that they are catamorphisms
-- (see the <ForSyDe-Atom-Skeleton.html#factorization factorization>
-- theorem). Where efficiency or practicality is a concern, some
-- skeletons are implemented as recurrences. One can still prove that
-- they are catamorphisms through alternative theorems (see
-- <ForSyDe-Atom.html#skillicorn05 [Skillicorn05]>).
--
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
----------------------------------------------------------------------
module ForSyDe.Atom.Skeleton.Vector (

  -- * Vector data type

  Vector(..),

  -- * \"Constructors\"

  -- | Theoretical constructors for the 'Vector' type, used in the
  -- definition of skeletons as catamorphisms.
  null, unit, (<++>),

  -- * Utilities

  vector, fromVector, indexes, isNull, (<:),

  -- * Skeletons

  -- | Algorithmic skeletons on vectors are mainly presented in terms
  -- of compositions of the atoms associated with the
  -- 'Skeleton' Layer. When defining them,
  -- we use the following operators:
  --
  -- <<docfiles/figs/eqs-skel-vector-operators.png>>
  --
  -- where:
  --
  -- * (1) is the 'unit' constructor, constructing a singleton vector.
  -- * (2) is the '<++>' constructor, concatenating two vectors.
  -- * (3) is the '<@!>' selector. The subscript notation is used to
  -- denote element at position /n/ in a vector.
  -- * (4) suggests an arbitrary selector which returns a vector with
  -- another one's elements, based on some indices. The shown example
  -- is an alternative notation for the 'tail' skeleton.
  
  -- ** Functional networks

  -- | This sub-category denotes skeletons (patterns) which are take
  -- functions as arguments. If the functions are
  -- 'ForSyDe.Atom.MoC.MoC' layer entities, i.e. processes, then these
  -- patterns are capable of constructing parallel process
  -- networks. Using the applicative mechanism, the designer has a
  -- high degree of freedom when customizing process networks through
  -- systematic partial application, rendering numerous possible
  -- usages for the same pattern. To avoid over-encumbering the
  -- figures, they depict small test cases, which might not expose the
  -- full potential of the constructors.
  --
  -- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
  -- on how to interpret, use and develop your own constructors.
  farm11, farm12, farm13, farm14,
  farm21, farm22, farm23, farm24,
  farm31, farm32, farm33, farm34,
  farm41, farm42, farm43, farm44,
  
  reduce, reducei,
  prefix, prefixi,
  suffix, suffixi,

  pipe, pipe1, pipe2, pipe3, pipe4,
  (=/=), recur, recur1, recur2, recur3, recur4,
  cascade, cascade1, cascade2, cascade3, cascade4, 
  mesh, mesh1, mesh2, mesh3, mesh4, 

  -- -- ** Queries

  -- -- | Queries return various information about a vector. They are
  -- -- also built as skeletons.

  -- length, index,
  
  -- -- ** Generators

  -- -- | Generators are specific applications of the @prefix@ or
  -- -- @suffix@ skeletons.

  -- fanout, fanoutn, generate, iterate, 
  
  -- -- ** Permutators

  -- -- | Permutators perform operations on the very structure of
  -- -- vectors, and make heavy use of the vector constructors.

  -- first, last, init, tail, inits, tails,
  -- concat, reverse, group, shiftr, shiftl, rotr, rotl, 
  -- take, drop, filterIdx, odds, evens, stride,
  -- get, (<@), (<@!), gather1, gather2, gather3, gather4, gather5,
  -- (<@!>), (<<@!>>), (<<<@!>>>), (<<<<@!>>>>), (<<<<<@!>>>>>),
  -- replace, scatter,
  -- -- takeWhile, filter
  -- bitrev, duals, unduals,

  -- -- ** Interfaces

  -- zipx, unzipx,
  
  ) where

import ForSyDe.Atom.Skeleton.Vector.Core
import ForSyDe.Atom.Skeleton.Vector.Lib
-- import ForSyDe.Atom.Skeleton.Vector.Interface

import Prelude hiding (null, last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)


