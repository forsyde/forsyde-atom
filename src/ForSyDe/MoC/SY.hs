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
import ForSyDe.Core.Wrappers
import Prelude hiding (filter, unzip, unzip3, zip, zip3, (<$))

-----------------------------------------------------------------------------
-- TYPE ALIAS
-----------------------------------------------------------------------------
-- WRONG!!!!
--type SignalSY = Signal (AbstExt a) -- dumbass!!!!! this leads to Signal (AbstExt (tag, value))
--type Tok a = AbstExt a             -- ULTRA WRONG!!!!
type Tok a = AbstExt a
type Sig a = Signal (AbstExt a)


-----------------------------------------------------------------------------
-- PRIMITIVE CONSTRUCTORS -- TIMED MOC TEMPLATE
-----------------------------------------------------------------------------

infixl 5 -§, §§
infixl 4 ->-

(-§)  :: (AbstExt a -> b) -> Sig a -> Signal b
(§§)  :: Signal (AbstExt a -> b) -> Sig a -> Signal b
(->-) :: AbstExt a -> Sig a -> Sig a
-----------------------------------------------------------------------------
(-§)  = (<$>)
(§§)  = (<*>)
(->-) = (:-)

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

-----------------------------------------------------------------------------
-- PROCESS CONSTRUCTORS / PATTERNS
-----------------------------------------------------------------------------
{-
comb  :: (Arg a -> Arg b) 
      -> Sig a -> Sig b 
comb2 :: (Arg a -> Arg b -> Arg c)
      -> Sig a -> Sig b -> Sig c
comb3 :: (Arg a -> Arg b -> Arg c -> Arg d)
      -> Sig a -> Sig b -> Sig c -> Sig d
comb4 :: (Arg a -> Arg b -> Arg c -> Arg d -> Arg e)
      -> Sig a -> Sig b -> Sig c -> Sig d -> Sig e
delay :: Tok a
      -> Sig a -> Sig a

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
       -> Sig b -> Sig c -> Sig d -> Sig e-}
-----------------------------------------------------------------------------
--comb12 :: (AbstExt a -> (AbstExt t, AbstExt b)) -> Sig a -> (Sig t, Sig b)
comb11 f s1          =        (f -§ s1)
comb12 f s1          = unzip  (f -§ s1)
comb13 f s1          = unzip3 (f -§ s1)
comb14 f s1          = unzip4 (f -§ s1)
comb21 f s1 s2       =        (f -§ s1 §§ s2)
comb22 f s1 s2       = unzip  (f -§ s1 §§ s2)
comb23 f s1 s2       = unzip3 (f -§ s1 §§ s2)
comb24 f s1 s2       = unzip4 (f -§ s1 §§ s2)
comb31 f s1 s2 s3    =        (f -§ s1 §§ s2 §§ s3)
comb32 f s1 s2 s3    = unzip  (f -§ s1 §§ s2 §§ s3)
comb33 f s1 s2 s3    = unzip3 (f -§ s1 §§ s2 §§ s3)
comb34 f s1 s2 s3    = unzip4 (f -§ s1 §§ s2 §§ s3)
comb41 f s1 s2 s3 s4 =        (f -§ s1 §§ s2 §§ s3 §§ s4)
comb42 f s1 s2 s3 s4 = unzip  (f -§ s1 §§ s2 §§ s3 §§ s4)
comb43 f s1 s2 s3 s4 = unzip3 (f -§ s1 §§ s2 §§ s3 §§ s4)
comb44 f s1 s2 s3 s4 = unzip4 (f -§ s1 §§ s2 §§ s3 §§ s4)
                      
delay e s1 = s1 ->- e


moore ns od mem s1 = od -§ s
  where s = mem ->- ns -§ s §§ s1
moore2 ns od mem s1 s2 = od -§ s
  where s = mem ->- ns -§ s §§ s1 §§ s2
moore3 ns od mem s1 s2 s3 = od -§ s
  where s = mem ->- ns -§ s §§ s1 §§ s2 §§ s3

mealy ns od mem s1 = od -§ s §§ s1
  where s = mem ->- ns -§ s §§ s1
mealy2 ns od mem s1 s2 = od -§ s §§ s1 §§ s2
  where s = mem ->- ns -§ s §§ s1 §§ s2
mealy3 ns od mem s1 s2 s3 = od -§ s §§ s1 §§ s2 §§ s3
  where s = mem ->- ns -§ s §§ s1 §§ s2 §§ s3

{- 


-- FILTER, FILL, HOLD

-- | The process constructor 'filter' discards the values who do not fulfill a predicate given by a predicate function and replaces them with as5ent events.
filter :: (a -> Bool) -- Predicate function
         -> Sig a -- Input signal
         -> Sig (Arg a) -- Output signal

-- | The process constructor 'fill' creates a process that 'fills' a signal with present values by replacing as5ent values with a given value. The output signal is not any more of the type 'Arg'.
fill :: a -- ^Default value
       -> Sig (Arg a) -- ^As5ent extended input signal
       -> Sig a -- ^Output signal

-- | The process constructor 'hold' creates a process that 'fills' a signal with values by replacing as5ent values by the preceding present value. Only in s64es, where no preceding value exists, the as5ent value is replaced by a default value. The output signal is not any more of the type 'Arg'.
hold :: a -- ^Default value
       -> Sig (Arg a) -- ^As5ent extended input signal
       -> Sig a -- ^Output signa

filter p = (-§) (\x -> if p x == True then Prst x else As5t)
fill   a = (-§) (replaceAs5t a)
  where replaceAs5t a' As5t     = a'
        replaceAs5t _  (Prst x) = x
hold   a s1 = s
  where s = holdf -§ (s ->- a) §§ s1
        holdf a' As5t     = a'
        holdf _  (Prst x) = x

-}
