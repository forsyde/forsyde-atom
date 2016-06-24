-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.AbsentExt
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016;
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the atoms and wrappers for the behavior
-- layer in ForSyDe processes.
-----------------------------------------------------------------------------
module ForSyDe.Atom.Behavior.Atom where

infixl 4 >$, >*, >%, >%!, >#, >#!

class Behavior w where
  -- | The exact equivalent of the 'fmap' function or the '<$>' infix
  -- operator for extended values. It bypasses the special values and
  -- maps a function @f@ to a wrapped value. It may be regarded as the
  -- "default behavior" of a process.
  --
  -- >>> let (a,u,v)=(Abst,Undef,Value 1)
  -- >>> (a,u,v)
  -- (⟂,?,1)
  -- >>> let f = (+1)
  -- >>> (f >$ a, f >$ u, f >$ v)
  -- (⟂,?,2)
  -- >>> (f <$> a, f <$> u, f <$> v)
  -- (⟂,?,2)
  --
  -- As seen in the example above, '>$' is interchangeable with '<$>'.
  (>$) :: (a -> b) -> w a -> w b

  -- | The exact equivalent of the applicative operator '<*>' for
  -- extended values. It may be regarded as the default behavior in case
  -- two events interact with each other. Check the source source code
  -- for the Applicative instance to see the lifting rules.
  --
  -- >>> let (a,u,v)=(Abst,Undef,Value 1)
  -- >>> (a,u,v)
  -- (⟂,?,1)
  -- >>> (+) >$ v >* v
  -- 2
  -- >>> (+) >$ v >* u
  -- ?
  -- >>> (+) >$ a >* v
  -- *** Exception: Illegal occurrence of an absent and present event
  --
  -- As with '>$', '>*' is also interchangeable with '<*>'.
  (>*) :: w (a -> b) -> w a -> w b

  -- | Reduce atom. Used to recursively fold multiple values from
  -- different signals. Undefined values affect the result.
  --
  -- >>> let x = ((:),Value []) >% (Value 1) >% Abst >% (Value 2) 
  -- >>> snd x
  -- [2,1]
  -- >>> snd $ x >% Undef 
  -- Undef
  (>%) :: ((a -> b -> b), w b) -- ^ (folding function, kernel value)
       -> w a                  -- ^ current term
       -> ((a -> b -> b), w b) -- ^ (same folding function to be
                               -- passed to the next term, folding
                               -- result)

  -- | Unsafe reduce atom. Used to recursively fold multiple values from
  -- different signals. Undefined values are ignored!
  --
  -- >>> let x = ((:),Value []) >% (Value 1) >% Abst >% (Value 2) >% Undef
  -- >>> snd x
  -- [2,1]
  (>%!) :: ((a -> b -> b), w b) -- ^ (folding function, kernel value)
        -> w a                  -- ^ current term
        -> ((a -> b -> b), w b) -- ^ (same folding function to be
                                -- passed to the next term, folding
                                -- result)

  -- | Predicate atom which replaces a value with another value.
  -- The 'Abst' event persists.
  --
  -- >>> let ps = map Value [False, True]
  -- >>> zipWith (>#) ps $ repeat (Value 1, Value 5)
  -- [1,5]
  -- >>> zipWith (>#) ps $ repeat (Undef, Value 5)
  -- [?,5]
  -- >>> zipWith (>#) ps $ repeat (Abst, Value 5)
  -- [⟂,⟂]
  (>#) :: w Bool     -- ^ boolean predicate
       -> (w a, w a) -- ^ (value to replace, value to replace with)
       -> w a        -- ^ result

  -- | Unsafe predicate atom which replaces a value with another
  -- value. Overwrites even 'Abst' events.
  --
  -- >>> let ps = map Value [False, True]
  -- >>> zipWith (>#!) ps $ repeat (Value 1, Value 5)
  -- [1,5]
  -- >>> zipWith (>#!) ps $ repeat (Undef, Value 5)
  -- [?,5]
  -- >>> zipWith (>#!) ps $ repeat (Abst, Value 5)
  -- [⟂,5]
  (>#!) :: w Bool     -- ^ boolean predicate
        -> (w a, w a) -- ^ (value to replace, value to replace with)
        -> w a        -- ^ result

