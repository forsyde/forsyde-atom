{-# LANGUAGE TypeFamilies,FlexibleContexts, PostfixOperators#-}
{-# OPTIONS_HADDOCK hide, prune, show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- 
-----------------------------------------------------------------------------

module ForSyDe.MoC where

import ForSyDe.Core

infixl 5 -$-, -*-
infixl 3 ->-, -&-
-- infixl 3 -<, -<<, -<<<

-- | Takes event constructor 
class MoC (e :: * -> *) where

  -- | the \star atom
  (-$-)  :: (Value a -> b) -> Signal (e (Value a)) -> Signal (e b)

  -- | the \ostar atom
  (-*-) :: Signal (e (Value a -> b)) -> Signal (e (Value a)) -> Signal (e b)

  -- | the \pre atom
  (->-) :: e a -> Signal (e a) -> Signal (e a)
   
  -- | the \phi atom
  (-&-) :: e a -> Signal (e a) -> Signal (e a)
           
-----------------------------------------------------------------------------
comb11 f s1          = (psi11 f -$- s1)
comb12 f s1          = (psi12 f -$- s1 -<)
comb13 f s1          = (psi13 f -$- s1 -<<)
comb14 f s1          = (psi14 f -$- s1 -<<<)
comb21 f s1 s2       = (psi21 f -$- s1 -*- s2)
comb22 f s1 s2       = (psi22 f -$- s1 -*- s2 -<)
comb23 f s1 s2       = (psi23 f -$- s1 -*- s2 -<<)
comb24 f s1 s2       = (psi24 f -$- s1 -*- s2 -<<<)
comb31 f s1 s2 s3    = (psi31 f -$- s1 -*- s2 -*- s3)
comb32 f s1 s2 s3    = (psi32 f -$- s1 -*- s2 -*- s3 -<)
comb33 f s1 s2 s3    = (psi33 f -$- s1 -*- s2 -*- s3 -<<)
comb34 f s1 s2 s3    = (psi34 f -$- s1 -*- s2 -*- s3 -<<<)
comb41 f s1 s2 s3 s4 = (psi41 f -$- s1 -*- s2 -*- s3 -*- s4)
comb42 f s1 s2 s3 s4 = (psi42 f -$- s1 -*- s2 -*- s3 -*- s4 -<)
comb43 f s1 s2 s3 s4 = (psi43 f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
comb44 f s1 s2 s3 s4 = (psi44 f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)

infixl 3 -&>-
delay i xs = i ->- (i -&- xs)
i -&>- xs  = delay i xs


moore11 ns od i s1          = comb11 od st
  where st                  = i -&>- comb21 ns st s1
moore12 ns od i s1          = comb12 od st
  where st                  = i -&>- comb21 ns st s1
moore13 ns od i s1          = comb13 od st
  where st                  = i -&>- comb21 ns st s1
moore14 ns od i s1          = comb14 od st
  where st                  = i -&>- comb21 ns st s1
moore21 ns od i s1 s2       = comb11 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore22 ns od i s1 s2       = comb12 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore23 ns od i s1 s2       = comb13 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore24 ns od i s1 s2       = comb14 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore31 ns od i s1 s2 s3    = comb11 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore32 ns od i s1 s2 s3    = comb12 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore33 ns od i s1 s2 s3    = comb13 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore34 ns od i s1 s2 s3    = comb14 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore41 ns od i s1 s2 s3 s4 = comb11 od st
  where st                  = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore42 ns od i s1 s2 s3 s4 = comb12 od st
  where st                  = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore43 ns od i s1 s2 s3 s4 = comb13 od st
  where st                  = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
moore44 ns od i s1 s2 s3 s4 = comb14 od st
  where st                  = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

mealy11 ns od i s1          = comb21 od st s1
  where st           = i -&>- comb21 ns st s1
mealy12 ns od i s1          = comb22 od st s1
  where st           = i -&>- comb21 ns st s1
mealy13 ns od i s1          = comb23 od st s1
  where st           = i -&>- comb21 ns st s1
mealy14 ns od i s1          = comb24 od st s1
  where st           = i -&>- comb21 ns st s1
mealy21 ns od i s1 s2       = comb31 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy22 ns od i s1 s2       = comb32 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy23 ns od i s1 s2       = comb33 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy24 ns od i s1 s2       = comb34 od st s1 s2
  where st           = i -&>- comb31 ns st s1 s2
mealy31 ns od i s1 s2 s3    = comb41 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy32 ns od i s1 s2 s3    = comb42 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy33 ns od i s1 s2 s3    = comb43 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy34 ns od i s1 s2 s3    = comb44 od st s1 s2 s3
  where st           = i -&>- comb41 ns st s1 s2 s3
mealy41 ns od i s1 s2 s3 s4 = (psi51 od -$- st -*- s1 -*- s2 -*- s3 -*- s4)
  where st            = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy42 ns od i s1 s2 s3 s4 = (psi52 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<)
  where st            = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy43 ns od i s1 s2 s3 s4 = (psi53 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<)
  where st            = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
mealy44 ns od i s1 s2 s3 s4 = (psi54 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<<)
  where st            = i -&>- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4


-- | The process constructor 'filter' discards the values who do not fulfill a predicate given by a predicate function and replaces them with as5ent events.
filter p s1 = predicate Abst -$- s1 
