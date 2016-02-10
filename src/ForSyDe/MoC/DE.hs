{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.DE
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015; 
--                    SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronuous library defines process constructors, processes and a signal conduit
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process. 
-----------------------------------------------------------------------------

module ForSyDe.MoC.DE where

import ForSyDe.Core

data Tok a = Tok Tag a       deriving (Show)
data Tag   = Tag Int | Infty deriving (Eq, Ord)
instance Show Tag where
  showsPrec _ x = showsTag x
    where showsTag Infty   = (++) "\8734"       
          showsTag (Tag x) = (++) (show x)

instance Enum Tag where
  fromEnum (Tag a) = a
  fromEnum Infty = maxBound
  toEnum 9223372036854775807 = Infty
  toEnum a = Tag a

tagPlus :: Tag -> Tag -> Tag
tagPlus Infty _ = Infty
tagPlus _ Infty = Infty
tagPlus (Tag a) (Tag b) = Tag (a + b)
          
-----------------------------------------------------------------------------

infixl 5 §-
(§-) :: (a -> b) -> Signal (Tok a) -> Signal (Tok b)
_ §- NullS           = NullS
f §- (Tok t x :- xs) = Tok t (f x) :- f §- xs


infixl 5 -§-
(-§-) :: Signal (Tok (a -> b)) -> Signal (Tok a) -> Signal (Tok b)
NullS -§- _     = NullS
_     -§- NullS = NullS
s1@(Tok t1 f :- fs) -§- s2@(Tok t2 x :- xs) 
    | t1 == t2 = Tok t1 (f x) :- (fs -§- xs)
    | t1 <  t2 = Tok t1 (f x) :- (fs -§- s2)
    | t1 >  t2 = Tok t2 (f x) :- (s1 -§- xs)

infixl 5 ->-
(->-) :: Signal (Tok a) -> Tok a -> Signal (Tok a)
xs ->- i@(Tok t _) = i :- (\(Tok t1 g) -> Tok (t1 `tagPlus` t) g) <$> xs

infixl 4 ¤
(¤) :: Eq a => Signal (Tok a) -> Signal (Tok a)
(¤) (x:-xs) = clean x xs
  where clean _ NullS = NullS
        clean (Tok t1 v1) (Tok t2 v2 :- vs)
          | t2 <  t1  = error "Tags not ordered"
          | v1 == v2  = clean (Tok t2 v2) vs
          | otherwise = (Tok t1 v1) :- clean (Tok t2 v2) vs

-----------------------------------------------------------------------------
-- ARGUMENT DATA TYPE - ABSENT EXTENDED
-----------------------------------------------------------------------------

data AbstExt a =  Abst | Prst a deriving (Eq)

instance Show a => Show (AbstExt a) where
  showsPrec _ x = showsAE x
    where showsAE Abst     = (++) "_"       
          showsAE (Prst x) = (++) (show x)

instance Read a => Read (AbstExt a) where
  readsPrec _ x       =  readsAE x 
    where readsAE s = [(Abst, r1) | ("_", r1) <- lex s] ++ [(Prst x, r2) | (x, r2) <- reads s]

instance Functor AbstExt where
  fmap _ Abst     = Abst
  fmap f (Prst x) = Prst (f x)

instance Applicative AbstExt where
  pure a  = Prst a
  _      <*> Abst   = Abst
  Abst   <*> _      = Abst
  (Prst x) <*> (Prst y) = Prst (x y)

----------------------
---- CONSTRUCTORS ----
----------------------

{--- | The `comb` take a combinatorial function as argument and returns a process with one input signals and one output signal.
comb :: (a -> b) -- ^ combinatorial function
       -> Signal (Subsig a) -- ^ input signal
       -> Signal (Subsig b) -- ^ output signal

-- | Behaves like 'comb', but the process takes 2 input signals.
comb2 :: (a -> b -> c) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c)

-- | Behaves like 'comb', but the process takes 3 input signals.
comb3 :: (a -> b -> c -> d) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d)

-- | Behaves like 'comb', but the process takes 4 input signals.
comb4 :: (a -> b -> c -> d -> e) -> Signal (Subsig a) -> Signal (Subsig b) -> Signal (Subsig c) -> Signal (Subsig d) -> Signal (Subsig e)-}

comb  f x       = (f §- x ¤)
comb2 f x y     = (f §- x -§- y ¤)
comb3 f x y z   = (f §- x -§- y -§- z ¤)
comb4 f x y z q = (f §- x -§- y -§- z -§- q ¤)


-- DELAY

-- | The process constructor 'delay' delays the signal one event cycle by introducing an initial value at the beginning of the output signal. It is necessary to initialize feed-back loops.
delay :: (Tok a) -- ^Initial state
        -> Signal (Tok a) -- ^Input signal
        -> Signal (Tok a) -- ^Output signal

delay x xs = xs ->- x

