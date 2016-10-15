{-# OPTIONS_HADDOCK show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.Skeleton
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports a type class with the interfaces for the
-- Skeleton layer atoms. It does /NOT/ export any implementation of
-- atoms not any constructor as composition of atoms.
-----------------------------------------------------------------------------


module ForSyDe.Atom.Skeleton (

  -- * Atoms
  
  Skeleton(..)

  -- * Bibliography
  
  -- | #gorlatch03# <http://link.springer.com/chapter/10.1007/978-1-4471-0097-3_1#page-1 [1]> Fischer, J., Gorlatch, S., & Bischof, H. (2003). Foundations of data-parallel skeletons. In /Patterns and skeletons for parallel and distributed computing/ (pp. 1-27). Springer London.
  
  -- | #bird96# [2] Bird, R., & De Moor, O. (1996, January). The algebra of programming. In /NATO ASI DPD/ (pp. 167-203).
  
  -- | #skillicorn05# <https://books.google.se/books?hl=ro&lr=&id=rQwsL5xsZigC&oi=fnd&pg=PP1&dq=skillicorn+foundation+parallel+programming&ots=UJMBr0uO2Q&sig=ncyXxE0gFNkUZwVOYyFb_ezWlGY&redir_esc=y#v=onepage&q=skillicorn%20foundation%20parallel%20programming&f=false [3]> Skillicorn, D. B. (2005). Foundations of parallel programming (No. 6). Cambridge University Press.

  ) where

infixl 4 =$=, =*=
infixl 2 =\=, =<<=


-- | Class containing all the Skeleton layer atoms.
--
-- This class is instantiated by a set of categorical types,
-- i.e. types which desctibe an inherent potential for parallel
-- evaluation. All skeletons, i.e. constructors for process networks
-- which can can be mapped on parallel platforms, are then described
-- as composition of these three atoms (the fourth being just a
-- specific instantiation of the third). This possible due to an
-- existing theorem enunciated in <#gorlatch03 [1]>, based on the
-- Bird-Merteens formalism <#bird96 [2]>:
--
-- [factorization] A function on a categorical type is an algorithmic
-- skeleton (i.e. catamorphism) /iff/ it can be represented in a
-- factorized form, i.e. as a /map/ composed with a /reduce/.
--
-- Consequently, most of the skeletons for the implemented categorical
-- types are described in their factorized form, taking as arguments
-- either:
--
-- * type constructors or functions derived from type constructors
-- * processes, i.e. MoC layer enities
--
-- Most of the ground-work on algorithmic skeletons on which this
-- module is founded has been laid bt Skillicorn in <#skillicorn03 [3]>.
class Skeleton c where
  -- | Atom which maps a function on each element of a structure
  -- (i.e. categorical type), defined as:
  --
  -- <<includes/figs/skel-map-atom-formula.png>>
  --
  -- '=$=' together with '=*=' and some utilities defined in
  -- "ForSyDe.Atom.Utility", form the @map@ pattern.
  (=$=)  :: (a -> b) -> c a -> c b

  -- | Atom which applies the functions contained by as structure
  -- (i.e. categorical type), on the elements of another structure,
  -- defined as:
  --
  -- <<includes/figs/skel-appl-atom-formula.png>>
  --
  -- '=$=' together with '=*=' and some utilities defined in
  -- "ForSyDe.Atom.Utility", form the @map@ pattern.
  (=*=)  :: c (a -> b) -> c a -> c b

  -- | Atom which reduces a structure to an element based on an
  -- associative function, defined as:
  --
  -- <<includes/figs/skel-red-atom-formula.png>>
  (=\=)  :: (a -> a -> a) -> c a -> a
  -----------------------------------

  -- | Skeleton which /pipes/ an element through all the functions
  -- contained by a structure. __OBS:__ this is not an atom, as it is
  -- implicitly defined as:
  --
  -- <<includes/figs/skel-pipe-atom-formula.png>>
  (=<<=) :: c (a -> a) -> a -> a    
  (=<<=) ps = (.) =\= ps
