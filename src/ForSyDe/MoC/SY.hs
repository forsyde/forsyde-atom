{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY
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

module ForSyDe.MoC.SY where

import ForSyDe.Core
import Prelude hiding (filter, unzip, unzip3, zip, zip3)

-----------------------------------------------------------------------------
-- TYPE ALIAS
-----------------------------------------------------------------------------

type Sig a = Signal (AbstExt a)
type Arg a = AbstExt a
type Tok a = AbstExt a


-----------------------------------------------------------------------------
-- PRIMITIVE CONSTRUCTORS
-----------------------------------------------------------------------------

infixl 5 -§, §§, ->-
infixl 3 §-

(-§)  :: (Arg a -> b) -> Sig a -> Sig b
(§§)  :: Sig (Arg a -> b) -> Sig a -> Sig b
(§-)  :: Sig (Arg a) -> Sig a
(->-) :: Sig a -> Tok a -> Sig a
-----------------------------------------------------------------------------
f -§ NullS         = NullS
f -§ (x:-xs)       = Prst (f x) :- f -§ xs

_       §§ NullS   = NullS
NullS   §§ _       = NullS
(f:-fs) §§ (Abst:-xs) = Abst :- fs §§ xs


(§-) = id 
xs ->- i = i :- xs

-----------------------------------------------------------------------------
-- FUNCTION WRAPPERS
-----------------------------------------------------------------------------

infixl 5 $> --, $$
($>)  :: (a -> b) -> Arg a -> Arg b
_ $> Abst      = Abst
_ $> (Prst Abst) = Abst 
f $> (Prst x)   = Prst (f x)  
f $> (Prst (Prst x))   = Prst (f x) 
--(<_>) :: Arg (a -> b) -> Arg a -> Arg b

{-

psi  :: (a -> b)
     -> (Arg a -> Arg b)
psi2 :: (a -> b -> c)
     -> (Arg a -> Arg b -> Arg c)     
psi3 :: (a -> b -> c -> d)
     -> (Arg a -> Arg b -> Arg c -> Arg d)
psi4 :: (a -> b -> c -> d -> e)
     -> (Arg a -> Arg b -> Arg c -> Arg d -> Arg e)
psi5 :: (a -> b -> c -> d -> e -> f)
     -> (Arg a -> Arg b -> Arg c -> Arg d -> Arg e -> Arg f)
psi6 :: (a -> b -> c -> d -> e -> f -> g)
     -> (Arg a -> Arg b -> Arg c -> Arg d -> Arg e -> Arg f -> Arg f)
-----------------------------------------------------------------------------
psi  f a           = f <$> (Arg a) 
psi2 f a b         = f <$> (Arg a) <*> (Arg b)
psi3 f a b c       = f <$> (Arg a) <*> (Arg b) <*> (Arg c)
psi4 f a b c d     = f <$> (Arg a) <*> (Arg b) <*> (Arg c) <*> (Arg d)
psi5 f a b c d e   = f <$> (Arg a) <*> (Arg b) <*> (Arg c) <*> (Arg d) <*> (Arg e)
psi6 f a b c d e f = f <$> (Arg a) <*> (Arg b) <*> (Arg c) <*> (Arg d) <*> (Arg e) <*> (Arg f)

-----------------------------------------------------------------------------
-- PROCESS CONSTRUCTORS / PATTERNS
-----------------------------------------------------------------------------

comb  :: (a -> b) 
      -> Sig a -> Sig b 
comb2 :: (a -> b -> c)
      -> Sig a -> Sig b -> Sig c
comb3 :: (a -> b -> c -> d)
      -> Sig a -> Sig b -> Sig c -> Sig d
comb4 :: (a -> b -> c -> d -> e)
      -> Sig a -> Sig b -> Sig c -> Sig d -> Sig e
delay :: a
      -> Sig a -> Sig a

zip  :: Sig a -> Sig b
     -> Sig (a, b)
zip3 :: Sig a -> Sig b -> Sig c
     -> Sig (a, b, c)
zip4 :: Sig a -> Sig b -> Sig c -> Sig d
     -> Sig (a, b, c, d)
zip5 :: Sig a -> Sig b -> Sig c -> Sig d -> Sig e
     -> Sig (a, b, c, d, e)
zip6 :: Sig a -> Sig b -> Sig c -> Sig d -> Sig e -> Sig f
     -> Sig (a, b, c, d, e, f)
     
unzip  :: Sig (a, b)
       -> (Sig a, Sig b)
unzip3 :: Sig (a, b, c)
       -> (Sig a, Sig b, Sig c)
unzip4 :: Sig (a, b, c, d)
       -> (Sig a, Sig b, Sig c, Sig d)
unzip5 :: Sig (a, b, c, d, e)
       -> (Sig a, Sig b, Sig c, Sig d, Sig e)
unzip6 :: Sig (a, b, c, d, e, f)
       -> (Sig a, Sig b, Sig c, Sig d, Sig e, Sig f)

moore  :: (a -> b -> a) -> (a -> c) -> a
       -> Sig b -> Sig c
moore2 :: (a -> b -> c -> a) -> (a -> d) -> a
       -> Sig b -> Sig c -> Sig d
moore3 :: (a -> b -> c -> d -> a) -> (a -> e) -> a
       -> Sig b -> Sig c -> Sig d -> Sig e
mealy  :: (a -> b -> a) -> (a -> b -> c) -> a
       -> Sig b -> Sig c 
mealy2 :: (a -> b -> c -> a) -> (a -> b -> c -> d) -> a
       -> Sig b -> Sig c -> Sig d
mealy3 :: (a -> b -> c -> d -> a) -> (a -> b -> c -> d -> e) -> a
       -> Sig b -> Sig c -> Sig d -> Sig e
-----------------------------------------------------------------------------


comb  f s1          = (f -§ s1 §-)
comb2 f s1 s2       = (f -§ s1 §§ s2 §-)
comb3 f s1 s2 s3    = (f -§ s1 §§ s2 §§ s3 §-)
comb4 f s1 s2 s3 s4 = (f -§ s1 §§ s2 §§ s3 §§ s4 §-)

delay e s1 = s1 ->- e

zip  s1 s2             = (,)     -§ xs §§ ys
zip3 s1 s2 s3          = (,,)    -§ xs §§ ys §§ zs
zip4 s1 s2 s3 s4       = (,,,)   -§ xs §§ ys §§ zs §§ as
zip5 s1 s2 s3 s4 s5    = (,,,,)  -§ xs §§ ys §§ zs §§ as §§ bs
zip6 s1 s2 s3 s4 s5 s6 = (,,,,,) -§ xs §§ ys §§ zs §§ as §§ bs §§ cs

unzip s1 = ((\(x,_) -> x) <$> s1,
            (\(_,x) -> x) <$> s1)
unzip3 c1 c2 c3 s1
  = ((\(x,_,_) -> x) <$> s1 ¤ c1,
     (\(_,x,_) -> x) <$> s1 ¤ c2,
     (\(_,_,x) -> x) <$> s1 ¤ c3)
unzip4 c1 c2 c3 c4 s1
  = ((\(x,_,_,_) -> x) <$> s1 ¤ c1,
     (\(_,x,_,_) -> x) <$> s1 ¤ c2,
     (\(_,_,x,_) -> x) <$> s1 ¤ c3,
     (\(_,_,_,x) -> x) <$> s1 ¤ c4)
unzip5 c1 c2 c3 c4 c5 s1
  = ((\(x,_,_,_,_) -> x) <$> s1 ¤ c1,
     (\(_,x,_,_,_) -> x) <$> s1 ¤ c2,
     (\(_,_,x,_,_) -> x) <$> s1 ¤ c3,
     (\(_,_,_,x,_) -> x) <$> s1 ¤ c4,
     (\(_,_,_,_,x) -> x) <$> s1 ¤ c5)  
unzip6 c1 c2 c3 c4 c5 c6 s1
  = ((\(x,_,_,_,_,_) -> x) <$> s1 ¤ c1,
     (\(_,x,_,_,_,_) -> x) <$> s1 ¤ c2,
     (\(_,_,x,_,_,_) -> x) <$> s1 ¤ c3,
     (\(_,_,_,x,_,_) -> x) <$> s1 ¤ c4,
     (\(_,_,_,_,x,_) -> x) <$> s1 ¤ c5,
     (\(_,_,_,_,_,x) -> x) <$> s1 ¤ c6)  


moore ns od mem xs = od -§ s
  where s = ns -§ s §§ xs ->- mem
moore2 ns od mem xs ys = od -§ s
  where s = ns -§ s §§ xs §§ ys ->- mem
moore3 ns od mem xs ys zs = od -§ s
  where s = ns -§ s §§ xs §§ ys §§ zs ->- mem

mealy ns od mem xs = od -§ s §§ xs
  where s = ns -§ s §§ xs ->- mem
mealy2 ns od mem xs ys = od -§ s §§ xs §§ ys
  where s = ns -§ s §§ xs §§ ys ->- mem
mealy3 ns od mem xs ys zs = od -§ s §§ xs §§ ys §§ zs
  where s = ns -§ s §§ xs §§ ys §§ zs ->- mem




-- FILTER, FILL, HOLD

-- | The process constructor 'filter' discards the values who do not fulfill a predicate given by a predicate function and replaces them with absent events.
filter :: (a -> Bool) -- Predicate function
         -> Sig a -- Input signal
         -> Sig (Arg a) -- Output signal

-- | The process constructor 'fill' creates a process that 'fills' a signal with present values by replacing absent values with a given value. The output signal is not any more of the type 'Arg'.
fill :: a -- ^Default value
       -> Sig (Arg a) -- ^Absent extended input signal
       -> Sig a -- ^Output signal

-- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing absent values by the preceding present value. Only in cases, where no preceding value exists, the absent value is replaced by a default value. The output signal is not any more of the type 'Arg'.
hold :: a -- ^Default value
       -> Sig (Arg a) -- ^Absent extended input signal
       -> Sig a -- ^Output signa

filter p = (-§) (\x -> if p x == True then Prst x else Abst)
fill   a = (-§) (replaceAbst a)
  where replaceAbst a' Abst     = a'
        replaceAbst _  (Prst x) = x
hold   a xs = s
  where s = holdf -§ (s ->- a) §§ xs
        holdf a' Abst     = a'
        holdf _  (Prst x) = x

-}
