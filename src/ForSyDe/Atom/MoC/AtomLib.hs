{-# LANGUAGE PostfixOperators, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_HADDOCK show-extensions, prune, hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.Atom
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the atoms and process constructors for the
-- synchronization layer.
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.AtomLib where

import ForSyDe.Atom.Behavior
import ForSyDe.Atom.Signal
import ForSyDe.Atom.Utility

infixl 5 -$-, -*-
infixl 3 ->-, -&-

-- | This is a type class defining the synchronization layer atoms.
-- Each model of computation exposes its tag system through an unique
-- event constructor (that defines /T/ &#215; /V/) as an instance of
-- this class.
--
-- In order to simplify the mathematical notations, we shall consider
-- the following function as the typeonstructor for a MoC-bound
-- signal:
--
-- <<includes/figs/signal-constructor-formula.png>>
--
class MoC e where
  type Param e
  
  -- | This is a rough equivalent of the @(\<$\>)@ functor operator,
  -- mapping a function on extended values to a MoC-bound signal
  -- (signal of tagged events). Its mathematical definition is:
  --
  -- <<includes/figs/star-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-$-) :: (Param e, [Value a] -> b) -> Signal (e [Value a]) -> (Param e, Signal (e b))

  -- | This is a rough equivalent of the @(\<*\>)@ applicative functor
  -- operator, used for synchronizing and applying a signal of
  -- functions (bound to a MoC) with a signal of values (bound to the
  -- same MoC). Its mathematical signature is:
  --
  -- <<includes/figs/ostar-atom-formula.png>>
  --
  -- The reason why &#946; is not extended is to allow for the
  -- composition of generic process constructors with arbitrary number
  -- of arguments.
  (-*-) :: (Param e, Signal (e ([Value a] -> b))) -> Signal (e [Value a]) -> (Param e, Signal (e b))

  -- | Since ForSyDe signals are modeled similar to
  -- <https://www.cs.ox.ac.uk/files/3378/PRG56.pdf Bird lists>, the
  -- inherent execution model of ForSyDe process networks is the
  -- dataflow network (Kahn process network), as shown by Reekie in
  -- his <http://ptolemy.eecs.berkeley.edu/~johnr/papers/thesis.html process nets>.
  -- Therefore it is of utmost importance to provide an atom which
  -- provides an initial event (token) to avoid execution deadlock in
  -- feedback loops (self-referential processes). Concretely, the
  -- @-\>-@ atom prepends an event at the head of a signal. Its
  -- signature used in mathematical formulas is:
  --
  -- <<includes/figs/pre-atom-formula.png>>
  (->-) :: e a -> Signal (e a) -> Signal (e a)
   
  -- | Another property, this time derived from the tagged signal
  -- model, requires the processes to be /monotonic/
  -- (order-preserving) in order to preserve determinancy. Therefore
  -- we introduce the @(-&-)@ atom as means to manipulate the tags in
  -- a signal in a manner which respects monotonicity. Its behavior
  -- could be described as "shifting the phase of a signal with a
  -- positiveonstant", thus preserving its characteristic function
  -- intact. Its signature used in mathematical formulas is:
  --
  -- <<includes/figs/phi-atom-formula.png>>
  (-&-) :: e a -> Signal (e a) -> Signal (e a)
  fromEvent :: e a -> a

-- class Event e where
--   event     :: a -> e a
--   fromEvent :: e a -> a


instance (Eq a, MoC e)  => Eq (Signal (e [a])) where
  a == b = flat a == flat b
    where flat = concat . map fromEvent . fromSignal

infixl 3 -¤, -<, -<<, -<<<, -<<<<, -<<<<<, -<<<<<<, -<<<<<<<, -<<<<<<<<
(-¤)        (_,s) =  s
(-<)        (_,s) = (s ||<)
(-<<)       (_,s) = (s ||<<)
(-<<<)      (_,s) = (s ||<<<)
(-<<<<)     (_,s) = (s ||<<<<)
(-<<<<<)    (_,s) = (s ||<<<<<)
(-<<<<<<)   (_,s) = (s ||<<<<<<)
(-<<<<<<<)  (_,s) = (s ||<<<<<<<)
(-<<<<<<<<) (_,s) = (s ||<<<<<<<<)



infixl 3 -&>-
delay i xs = i ->- (i -&- xs)
i -&>- xs = delay i xs


comb11 f s1                      = (f -$- s1 -¤)
comb21 f s1 s2                   = (f -$- s1 -*- s2 -¤)
comb31 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -¤)
comb41 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -¤)
comb51 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -¤)
comb61 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -¤)
comb71 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -¤)
comb81 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -¤)

comb12 f s1                      = (f -$- s1 -<)
comb22 f s1 s2                   = (f -$- s1 -*- s2 -<)
comb32 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -<)
comb42 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -<)
comb52 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -<)
comb62 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -<)
comb72 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -<)
comb82 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -<)

comb13 f s1                      = (f -$- s1 -<<)
comb23 f s1 s2                   = (f -$- s1 -*- s2 -<<)
comb33 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -<<)
comb43 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
comb53 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -<<)
comb63 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -<<)
comb73 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -<<)
comb83 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -<<)

comb14 f s1                      = (f -$- s1 -<<<)
comb24 f s1 s2                   = (f -$- s1 -*- s2 -<<<)
comb34 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -<<<)
comb44 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)
comb54 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -<<<)
comb64 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -<<<)
comb74 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -<<<)
comb84 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -<<<)

state11 ns i s1          =        comb21 ns st s1 
  where st               = i -&>- comb21 ns st s1 
state21 ns i s1 s2       =        comb31 ns st s1 s2
  where st               = i -&>- comb31 ns st s1 s2
state31 ns i s1 s2 s3    =        comb41 ns st s1 s2 s3
  where st               = i -&>- comb41 ns st s1 s2 s3
state41 ns i s1 s2 s3 s4 =        comb51 ns st s1 s2 s3 s4
  where st               = i -&>- comb51 ns st s1 s2 s3 s4

state12 ns (i1,i2) s1          = let (ns1,ns2) = comb32 ns st1 st2 s1
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
state22 ns (i1,i2) s1 s2       = let (ns1,ns2) = comb42 ns st1 st2 s1 s2
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
state32 ns (i1,i2) s1 s2 s3    = let (ns1,ns2) = comb52 ns st1 st2 s1 s2 s3
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
state42 ns (i1,i2) s1 s2 s3 s4 = let (ns1,ns2) = comb62 ns st1 st2 s1 s2 s3 s4
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)

state13 ns (i1,i2,i3) s1          = let (ns1,ns2,ns3) = comb43 ns st1 st2 st3 s1
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
state23 ns (i1,i2,i3) s1 s2       = let (ns1,ns2,ns3) = comb53 ns st1 st2 st3 s1 s2
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
state33 ns (i1,i2,i3) s1 s2 s3    = let (ns1,ns2,ns3) = comb63 ns st1 st2 st3 s1 s2 s3
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
state43 ns (i1,i2,i3) s1 s2 s3 s4 = let (ns1,ns2,ns3) = comb73 ns st1 st2 st3 s1 s2 s3 s4
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)

state14 ns (i1,i2,i3,i4) s1          = let (ns1,ns2,ns3,ns4) = comb54 ns st1 st2 st3 st4 s1
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
state24 ns (i1,i2,i3,i4) s1 s2       = let (ns1,ns2,ns3,ns4) = comb64 ns st1 st2 st3 st4 s1 s2
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
state34 ns (i1,i2,i3,i4) s1 s2 s3    = let (ns1,ns2,ns3,ns4) = comb74 ns st1 st2 st3 st4 s1 s2 s3
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
state44 ns (i1,i2,i3,i4) s1 s2 s3 s4 = let (ns1,ns2,ns3,ns4) = comb84 ns st1 st2 st3 st4 s1 s2 s3 s4
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)

stated01 ns i             = st 
  where st                = i -&>- comb11 ns st 
stated11 ns i s1          = st
  where st                = i -&>- comb21 ns st s1 
stated21 ns i s1 s2       = st
  where st                = i -&>- comb31 ns st s1 s2
stated31 ns i s1 s2 s3    = st
  where st                = i -&>- comb41 ns st s1 s2 s3
stated41 ns i s1 s2 s3 s4 = st
  where st                = i -&>- comb51 ns st s1 s2 s3 s4

stated02 ns (i1,i2)             = let (ns1,ns2) = comb22 ns st1 st2
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated12 ns (i1,i2) s1          = let (ns1,ns2) = comb32 ns st1 st2 s1
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated22 ns (i1,i2) s1 s2       = let (ns1,ns2) = comb42 ns st1 st2 s1 s2
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated32 ns (i1,i2) s1 s2 s3    = let (ns1,ns2) = comb52 ns st1 st2 s1 s2 s3
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
stated42 ns (i1,i2) s1 s2 s3 s4 = let (ns1,ns2) = comb62 ns st1 st2 s1 s2 s3 s4
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)

stated03 ns (i1,i2,i3)             = let (ns1,ns2,ns3) = comb33 ns st1 st2 st3
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated13 ns (i1,i2,i3) s1          = let (ns1,ns2,ns3) = comb43 ns st1 st2 st3 s1
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated23 ns (i1,i2,i3) s1 s2       = let (ns1,ns2,ns3) = comb53 ns st1 st2 st3 s1 s2
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated33 ns (i1,i2,i3) s1 s2 s3    = let (ns1,ns2,ns3) = comb63 ns st1 st2 st3 s1 s2 s3
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
stated43 ns (i1,i2,i3) s1 s2 s3 s4 = let (ns1,ns2,ns3) = comb73 ns st1 st2 st3 s1 s2 s3 s4
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)

stated04 ns (i1,i2,i3,i4)             = let (ns1,ns2,ns3,ns4) = comb44 ns st1 st2 st3 st4
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated14 ns (i1,i2,i3,i4) s1          = let (ns1,ns2,ns3,ns4) = comb54 ns st1 st2 st3 st4 s1
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated24 ns (i1,i2,i3,i4) s1 s2       = let (ns1,ns2,ns3,ns4) = comb64 ns st1 st2 st3 st4 s1 s2
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated34 ns (i1,i2,i3,i4) s1 s2 s3    = let (ns1,ns2,ns3,ns4) = comb74 ns st1 st2 st3 st4 s1 s2 s3
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
stated44 ns (i1,i2,i3,i4) s1 s2 s3 s4 = let (ns1,ns2,ns3,ns4) = comb84 ns st1 st2 st3 st4 s1 s2 s3 s4
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)

moore11 ns od i s1          =        comb11 od st
  where st                  = i -&>- comb21 ns st s1
moore12 ns od i s1          =        comb12 od st
  where st                  = i -&>- comb21 ns st s1
moore13 ns od i s1          =        comb13 od st
  where st                  = i -&>- comb21 ns st s1
moore14 ns od i s1          =        comb14 od st
  where st                  = i -&>- comb21 ns st s1
moore21 ns od i s1 s2       =        comb11 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore22 ns od i s1 s2       =        comb12 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore23 ns od i s1 s2       =        comb13 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore24 ns od i s1 s2       =        comb14 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore31 ns od i s1 s2 s3    =        comb11 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore32 ns od i s1 s2 s3    =        comb12 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore33 ns od i s1 s2 s3    =        comb13 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore34 ns od i s1 s2 s3    =        comb14 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore41 ns od i s1 s2 s3 s4 =        comb11 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore42 ns od i s1 s2 s3 s4 =        comb12 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore43 ns od i s1 s2 s3 s4 =        comb13 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore44 ns od i s1 s2 s3 s4 =        comb14 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4

mealy11 ns od i s1          =        comb21 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy12 ns od i s1          =        comb22 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy13 ns od i s1          =        comb23 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy14 ns od i s1          =        comb24 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy21 ns od i s1 s2       =        comb31 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy22 ns od i s1 s2       =        comb32 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy23 ns od i s1 s2       =        comb33 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy24 ns od i s1 s2       =        comb34 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy31 ns od i s1 s2 s3    =        comb41 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy32 ns od i s1 s2 s3    =        comb42 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy33 ns od i s1 s2 s3    =        comb43 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy34 ns od i s1 s2 s3    =        comb44 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy41 ns od i s1 s2 s3 s4 =        comb51 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy42 ns od i s1 s2 s3 s4 =        comb52 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy43 ns od i s1 s2 s3 s4 =        comb53 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy44 ns od i s1 s2 s3 s4 =        comb54 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
