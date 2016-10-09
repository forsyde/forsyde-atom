{-# OPTIONS_HADDOCK show-extensions #-}

module ForSyDe.Atom.Skeleton where

infixl 4 =$=, =*=
infixl 2 =\=, =<<=

class Skeleton c where
  (=$=)  :: (a -> b) -> c a -> c b    -- fmap
  (=*=)  :: c (a -> b) -> c a -> c b  -- applicative
  (=\=)  :: (a -> a -> a) -> c a -> a -- reduce
  -----------------------------------
  (=<<=) :: c (a -> a) -> a -> a      -- pipe (implicit)
  (=<<=) ps = (.) =\= ps
