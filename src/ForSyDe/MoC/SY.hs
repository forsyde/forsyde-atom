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
import Prelude               hiding (filter, unzip, unzip3, zip, zip3)


-----------------------------------------------------------------------------
-- PRIMITIVE CONSTRUCTORS
-----------------------------------------------------------------------------

infixl 5 -§, §§, ->-
infixl 3 §-

(-§)  :: (a -> b) -> Signal a -> Signal b
(§§)  :: Signal (a -> b) -> Signal a -> Signal b
(§-)  :: Signal a -> Signal a
(->-) :: Signal a -> a -> Signal a
-----------------------------------------------------------------------------
(-§) = (<$>) 
(§§) = (<*>)
(§-) = id 
xs ->- i = i :- xs

-----------------------------------------------------------------------------
-- FUNCTION WRAPPERS
-----------------------------------------------------------------------------
psi  :: (a -> b)
     -> (AbstExt a -> AbstExt b)
psi2 :: (a -> b -> c)
     -> (AbstExt a -> AbstExt b -> AbstExt c)     
psi3 :: (a -> b -> c -> d)
     -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d)
psi4 :: (a -> b -> c -> d -> e)
     -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d -> AbstExt e)
psi5 :: (a -> b -> c -> d -> e -> f)
     -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d -> AbstExt e -> AbstExt f)
psi6 :: (a -> b -> c -> d -> e -> f -> g)
     -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d -> AbstExt e -> AbstExt f -> AbstExt f)
-----------------------------------------------------------------------------
psi 

--- GO HOME AT 15.00 !!!!

-----------------------------------------------------------------------------
-- PROCESS CONSTRUCTORS / PATTERNS
-----------------------------------------------------------------------------

comb  :: (a -> b) 
      -> Signal a -> Signal b 
comb2 :: (a -> b -> c)
      -> Signal a -> Signal b -> Signal c
comb3 :: (a -> b -> c -> d)
      -> Signal a -> Signal b -> Signal c -> Signal d
comb4 :: (a -> b -> c -> d -> e)
      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
delay :: a
      -> Signal a -> Signal a

zip  :: Signal a -> Signal b
     -> Signal (a,b)
zip3 :: Signal a -> Signal b -> Signal c
     -> Signal (a,b,c)
zip4 :: Signal a -> Signal b -> Signal c -> Signal d
     -> Signal (a,b,c,d)
zip5 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e
     -> Signal (a,b,c,d,e)
zip6 :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
     -> Signal (a,b,c,d,e,f)
     
unzip  :: Signal (a,b)
       -> (Signal a,Signal b)
unzip3 :: Signal (a, b, c)
       -> (Signal a, Signal b, Signal c)
unzip4 :: Signal (a,b,c,d)
       -> (Signal a,Signal b,Signal c,Signal d)
unzip5 :: Signal (a,b,c,d,e)
       -> (Signal a,Signal b,Signal c,Signal d,Signal e)
unzip6 :: Signal (a,b,c,d,e,f)
       -> (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f)

moore  :: (a -> b -> a) -> (a -> c) -> a
       -> Signal b -> Signal c
moore2 :: (a -> b -> c -> a) -> (a -> d) -> a
       -> Signal b -> Signal c -> Signal d
moore3 :: (a -> b -> c -> d -> a) -> (a -> e) -> a
       -> Signal b -> Signal c -> Signal d -> Signal e
mealy  :: (a -> b -> a) -> (a -> b -> c) -> a
       -> Signal b -> Signal c 
mealy2 :: (a -> b -> c -> a) -> (a -> b -> c -> d) -> a
       -> Signal b -> Signal c -> Signal d
mealy3 :: (a -> b -> c -> d -> a) -> (a -> b -> c -> d -> e) -> a
       -> Signal b -> Signal c -> Signal d -> Signal e
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

unzip s1 
  = ((\(x,_) -> x) <$> s1 ¤ c1,
     (\(_,x) -> x) <$> s1 ¤ c2)
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
         -> Signal a -- Input signal
         -> Signal (AbstExt a) -- Output signal

-- | The process constructor 'fill' creates a process that 'fills' a signal with present values by replacing absent values with a given value. The output signal is not any more of the type 'AbstExt'.
fill :: a -- ^Default value
       -> Signal (AbstExt a) -- ^Absent extended input signal
       -> Signal a -- ^Output signal

-- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing absent values by the preceding present value. Only in cases, where no preceding value exists, the absent value is replaced by a default value. The output signal is not any more of the type 'AbstExt'.
hold :: a -- ^Default value
       -> Signal (AbstExt a) -- ^Absent extended input signal
       -> Signal a -- ^Output signa

filter p = (-§) (\x -> if p x == True then Prst x else Abst)
fill   a = (-§) (replaceAbst a)
  where replaceAbst a' Abst     = a'
        replaceAbst _  (Prst x) = x
hold   a xs = s
  where s = holdf -§ (s ->- a) §§ xs
        holdf a' Abst     = a'
        holdf _  (Prst x) = x

