{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------
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
-- implementing the atoms for the 'ForSyDe.MoC.Skeleton.Skeleton'
-- class. Algorithmic skeletons are mostly described for 'Vector' in
-- their factorized form (i.e. as a /map-reduce/ pattern, using
-- skeleton layer atoms), assuring that they are catamorphisms. Where
-- practical matters, or efficience are a concern, some skeletons are
-- implemented as recurrences. One can still prove that they are
-- catamorphisms through alternative theorems (e.g. by proving that
-- they have an inverse over 'Vector').
-----------------------------------------------------------------------------
module ForSyDe.Atom.Skeleton.Vector (

  -- * Vector data type

  Vector(..),

  -- * \"Constructors\"

  -- | According to <#bird96 [2]>, 'Vector' should be implemented as
  -- following:
  --
  -- > data Vector a = Null                   -- null element
  -- >               | Unit a                 -- singleton vector
  -- >               | Vector a <++> Vector a -- concatenate two vectors
  --
  -- This construction suggests the possibility of splitting a
  -- 'Vector' into multiple parts and evaluating it in parallel. While
  -- due to practical considerations we have chosen another
  -- implementation (see above), when defining skeletons on vectors we
  -- shall use this particular interpretation. Therefore @Null@,
  -- @Unit@ and @\<++\>@ are provided as functions.
  null, unit, (<++>),

  -- * Utilities

  vector, fromVector, indexes, isNull, (<:),

  -- * Skeletons

  -- | Algorithmic skeletons on vectors are mainly presented in terms
  -- of compositions of the atoms associated with the
  -- 'Skeleton' Layer. When defining them,
  -- we use the following operators:
  --
  -- <<includes/figs/skel-operators-formula.png>>
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

  -- | This sub-category denotes skeletons (i.e. Skeleton Layer
  -- patterns) which are capable of constructing parallel process
  -- networks from processes (i.e. MoC layer entities) passed as
  -- arguments. Using the applicative mechanism, the designer has a
  -- high degree of freedom when customizing process networks through
  -- systematic partial application, rendering numerous possible
  -- usages for the same patterns. To avoid over-encumberating our
  -- examples with otherwise optional details, only 'map22' and 'red2'
  -- depict a somewhat complete scenario, the rest of the patterns
  -- assuming fully-applied processes as arguments.
  --
  -- Due to Haskell's strict type system and the implementation
  -- mechanisms, we need to provide separate constructors @skelXY@,
  -- where @skel@ is the process network constructor type, @X@ is the
  -- number of inputs and @Y@ is the number of outputs. This module
  -- provides constructors with @ X = [0..8]@ and @ Y = [1..4]@. If
  -- needed, the designer is free to implement her own constructor by
  -- following the atom composition rules in the source code.

  map11, map12, map13, map14,
  map21, map22, map23, map24,
  map31, map32, map33, map34,
  map41, map42, map43, map44,
  map51, map52, map53, map54,
  map61, map62, map63, map64,
  map71, map72, map73, map74,
  map81, map82, map83, map84,

  reduce1,  reduce2,  reduce3,  reduce4,  reduce5,  reduce6,  reduce7,  reduce8,
  reduce1', reduce2', reduce3', reduce4', reduce5', reduce6', reduce7', reduce8',
  
  prefix1,  prefix2,  prefix3,  prefix4,  prefix5,  prefix6, 
  prefix1', prefix2', prefix3', prefix4', prefix5', prefix6',
  
  suffix1,  suffix2,  suffix3,  suffix4,  suffix5,  suffix6,  
  suffix1', suffix2', suffix3', suffix4', suffix5', suffix6',

  pipe0,     pipe1,     pipe2,     pipe3,     pipe4,     pipe5,     pipe6,     pipe7,     pipe8,
  systolic0, systolic1, systolic2, systolic3, systolic4, systolic5, systolic6, systolic7, systolic8,
  
  cascade0, cascade1, cascade2, cascade3, cascade4, 
  mesh0,    mesh1,    mesh2,    mesh3,    mesh4, 

  -- ** Queries

  -- | Queries return various information about a vector. They are
  -- also built as skeletons.

  length, index,
  
  -- ** Generators

  -- | Generators are specific applications of the @prefix@ or
  -- @suffix@ skeletons.

  fanout, fanoutn, generate, iterate, 
  
  -- ** Permutators

  -- | Permutators perform operations on the very structure of
  -- vectors, and make heavy use of the vector constructors.

  first, last, init, tail, inits, tails,
  concat, reverse, group, shiftr, shiftl, rotr, rotl, 
  take, drop, filterIdx, odds, evens, stride,
  get, (<@), (<@!), gather1, gather2, gather3, gather4, gather5,
  (<@!>), (<<@!>>), (<<<@!>>>), (<<<<@!>>>>), (<<<<<@!>>>>>),
  replace, scatter,
  -- takeWhile, filter
  bitrev, duals, unduals,

  -- ** Interfaces

  zipx, unzipx,
  
  -- * Bibliography
  
  -- | #gorlatch03# <http://link.springer.com/chapter/10.1007/978-1-4471-0097-3_1#page-1 [1]> Fischer, J., Gorlatch, S., & Bischof, H. (2003). Foundations of data-parallel skeletons. In /Patterns and skeletons for parallel and distributed computing/ (pp. 1-27). Springer London.
  
  -- | #bird96# [2] Bird, R., & De Moor, O. (1996, January). The algebra of programming. In /NATO ASI DPD/ (pp. 167-203).
  
  -- | #skillicorn05# <https://books.google.se/books?hl=ro&lr=&id=rQwsL5xsZigC&oi=fnd&pg=PP1&dq=skillicorn+foundation+parallel+programming&ots=UJMBr0uO2Q&sig=ncyXxE0gFNkUZwVOYyFb_ezWlGY&redir_esc=y#v=onepage&q=skillicorn%20foundation%20parallel%20programming&f=false [3]> Skillicorn, D. B. (2005). Foundations of parallel programming (No. 6). Cambridge University Press.

  -- | #reekie95# <http://ptolemy.eecs.berkeley.edu/~johnr/papers/pdf/thesis.pdf [4]> Reekie, H. J. (1995). Realtime signal processing: Dataflow, visual, and functional programming.
  ) where

import ForSyDe.Atom.Skeleton.Vector.Core
import ForSyDe.Atom.Skeleton.Vector.Lib
import ForSyDe.Atom.Skeleton.Vector.Interface

import Prelude hiding (null, last, init, tail, map, reverse, length, concat, take, drop, filter, takeWhile, iterate, generate)


