{-# LANGUAGE PostfixOperators, TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the core entities of the MoC/synchronization
-- layer: atom interfaces and process constructors as patterns of
-- atoms. It does /NOT/ export any implementation or instantiation of
-- a specific MoC.
--
-- __IMPORTANT!!!__ Most of the multi-parameter higher-order functions
-- provided by the library API are named along the lines of
-- @functionMN@ where @M@ represents the number of __/curried/__
-- inputs (i.e. @a1 -> a2 -> ... -> aM@), while @N@ represents the
-- number of __/tupled/__ outputs (i.e. @(b1,b2,...,bN)@). To avoid
-- repetition we shall only provide documentation for functions with 2
-- inputs and 2 outputs (i.e. @function22@).
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC(
  
  -- * Atoms
  
  MoC(..),

  -- * Process constructors

  -- | As shown in the documentation of "ForSyDe.Atom" process
  -- constructors are implemented as compositions of MoC atoms. Also,
  -- in order to avoid working with signals of tuples and for process
  -- network to reflect the passed functions, we use @unzip@ utilities
  -- (defined in "ForSyDe.Core.Utility").
  --
  -- Due to Haskell's strict type system and the implementation
  -- mechanisms, we need to provide separate constructors @processXY@,
  -- where @process@ is the process constructor type, @X@ is the
  -- number of inputs and @Y@ is the number of outputs. This module
  -- provides constructors with @ X = [0..4]@ and @ Y = [1..4]@. If
  -- needed, the designer is free to implement her own constructor by
  -- following the atom composition rules in the source code.

  delay, (-&>-),
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  comb51, comb52, comb53, comb54,

  state11, state12, state13, state14,
  state21, state22, state23, state24,
  state31, state32, state33, state34,
  state41, state42, state43, state44,

  stated01, stated02, stated03, stated04,
  stated11, stated12, stated13, stated14,
  stated21, stated22, stated23, stated24,
  stated31, stated32, stated33, stated34,
  stated41, stated42, stated43, stated44,
  
  moore11, moore12, moore13, moore14,
  moore21, moore22, moore23, moore24,
  moore31, moore32, moore33, moore34,
  moore41, moore42, moore43, moore44,

  mealy11, mealy12, mealy13, mealy14,
  mealy21, mealy22, mealy23, mealy24,
  mealy31, mealy32, mealy33, mealy34,
  mealy41, mealy42, mealy43, mealy44,

  -- * Utilities
  
  ctxt11, ctxt21, ctxt31, ctxt41, ctxt51, ctxt61, ctxt71, ctxt81, 
  ctxt12, ctxt22, ctxt32, ctxt42, ctxt52, ctxt62, ctxt72, ctxt82, 
  ctxt13, ctxt23, ctxt33, ctxt43, ctxt53, ctxt63, ctxt73, ctxt83, 
  ctxt14, ctxt24, ctxt34, ctxt44, ctxt54, ctxt64, ctxt74, ctxt84,
  warg, wres,
  (-*<), (-*<<), (-*<<<), (-*<<<<), (-*<<<<<), (-*<<<<<<), (-*<<<<<<<), (-*<<<<<<<<),
  )
  where

import ForSyDe.Atom.MoC.Stream
import ForSyDe.Atom.Utility

infixl 5 -.-, -*-
infixl 3 -<-, -*, -&-

-- | This is a type class defining the MoC layer atoms. Each model of
-- computation exposes its tag system through an unique event
-- constructor as an instance of this class, which defines /T/ &#215;
-- /V/.
class (Functor e) => MoC e where

  -- |<<docfiles/figs/eqs-moc-atom-function.png>>
  --
  -- This is a type family alias for a context-bound function passed
  -- as an argument to a MoC atom. In the most simple case it can be
  -- regarded as an enhanced '->' type operator. While hindering
  -- expressiveness, this implementation choice certainly has its
  -- advantages in avoiding unnecessary or redundant type constructors
  -- for context-less MoCs (e.g. all __timed__ MoCs). Aliases are
  -- replaced at compile time, thus not trading run-time performance,
  -- while redundant type constructors (see version 0.1.1 and prior)
  -- still need to be evaluated.
  type Fun e a b

  -- |<<docfiles/figs/eqs-moc-atom-result.png>>
  --
  -- As with 'Fun', this alias hides a context-bound value
  -- (e.g. result). While through their definition, the co-existence
  -- of 'Fun' and 'Res' is redundant and there exist more explicit
  -- definitions (see version 0.1.1 and prior), this implementation
  -- choice is justified by the benefit in run-time performance.
  type Res e b
  
  -- |<<docfiles/figs/eqs-moc-atom-dot.png>>
  --
  -- This atom is mapping a function on values (in the presence of a
  -- context) to a signal, i.e. stream of tagged events. As ForSyDe
  -- deals with /determinate/, /functional/ processes, this atom
  -- defines the (only) /behavior/ of a process in rapport to one
  -- input signal. 
  (-.-) :: Fun e a b -> Stream (e a) -> Stream (e b)
  
  -- | <<docfiles/figs/eqs-moc-atom-star.png>>
  -- 
  -- This atom synchronizes two signals, one carrying functions on
  -- values (in the presence of a context), and the other containing
  -- values, during which it applies the former on the latter. As
  -- concerning the process created, this atom defines a /relation/
  -- between two signals.
  (-*-) :: Stream (e (Fun e a b)) -> Stream (e a) -> Stream (e b)

  -- | <<docfiles/figs/eqs-moc-atom-post.png>>
  -- 
  -- Artifficial /utility/ which drops the context and/or partitioning
  -- yielding a clean signal type. 
  (-*)  :: Stream (e (Res e b)) -> Stream (e b)


  -- | <<docfiles/figs/eqs-moc-atom-pre.png>>
  -- 
  -- This atom appends a (partition of) events at the begining of a
  -- signal. This atom is necessary to ensure /complete partial order/
  -- of a signal and assures the /least upper bound/ necessary for
  -- example in the evaluation of feedback loops.
  --
  -- Notice the difference between the formal and the implemented type
  -- signatures. In the implementation the value/partition is wrapped
  -- inside an event type to enable smooth composition. You might also
  -- notice the explicit list type for the "initial event". This is
  -- due to the agreed implementation convention chosen to represent a
  -- /"partition"/ for __untimed__ MoCs (i.e. a grouping of events
  -- with the same partial order in relation to, for example, a
  -- process firing). In case of __timed__ MoCs (where the order
  -- between events is total), the list would /need/ to be a
  -- singleton, i.e. the list constructor is ignored. The explicit
  -- list type is needed and could not be masked behind a type alias
  -- due to the fact that aliases are not injective, thus hindering
  -- type inference in case of delayed feedback loops.
  (-<-) :: e [a] -> Stream (e a) -> Stream (e a)
   
  -- | <<docfiles/figs/eqs-moc-atom-phi.png>>
  -- 
  -- This atom allows the manipulation of tags in a signal in a
  -- restrictive way which preserves /monotonicity/ and /continuity/
  -- in a process, namely by “phase-shifting” all tags in a signal
  -- with the appropriate metric corresponding to each MoC. Thus
  -- it preserves the characteristic function intact.
  --
  -- As with the '-<-' atom, we can justify the type signature for
  -- smooth composition, although the only information required for
  -- this atom are tags which are encapsulated in the event type
  -- constructor.
  (-&-) :: e [a] -> Stream (e a) -> Stream (e a)

infixl 3 -&>-
-- | <<docfiles/figs/eqs-moc-pattern-delay.png>>
--   <<docfiles/figs/moc-pattern-delay.png>>
--
-- The 'delay' process provides both initial token(s) and shifts the
-- phase of the signal. In other words, it "delays" a signal with
-- one or several events. 
--
-- There is also an infix variant (@infixl 3@). To justify the first
-- argument, see the documentation of the '-<-' atom.
--
-- > delay, (-&>-),
delay i xs = i -<- (i -&- xs)
i -&>- xs = delay i xs          

-- |  #comb22f# /(*) to be read / @a1 -> a2 -> (b1, b2)@ /where each/
-- /argument and result might be individually wrapped with a context/
-- /and might also express a partition./
--
-- <<docfiles/figs/eqs-moc-pattern-comb.png>>
-- <<docfiles/figs/moc-pattern-comb.png>>
--
-- The @comb@ processes takes care of synchronization between signals
-- and maps combinatorial functions on their event values. This
-- library provides the following patterns/process constructors:
--
-- > comb11, comb12, comb13, comb14,
-- > comb21, comb22, comb23, comb24,
-- > comb31, comb32, comb33, comb34,
-- > comb41, comb42, comb43, comb44,
-- > comb51, comb52, comb53, comb54,
comb22 :: (MoC e)
       => (Fun e a1 (Fun e a2 (Res e b1, Res e b2)))
       -- ^ (<#comb22f *>)
       -> Stream (e a1)                  -- ^ first input signal
       -> Stream (e a2)                  -- ^ second input signal
       -> (Stream (e b1), Stream (e b2)) -- ^ two output signals
comb11 f s1                      = (f -.- s1 -*)
comb21 f s1 s2                   = (f -.- s1 -*- s2 -*)
comb31 f s1 s2 s3                = (f -.- s1 -*- s2 -*- s3 -*)
comb41 f s1 s2 s3 s4             = (f -.- s1 -*- s2 -*- s3 -*- s4 -*)
comb51 f s1 s2 s3 s4 s5          = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*)
comb61 f s1 s2 s3 s4 s5 s6       = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*)
comb71 f s1 s2 s3 s4 s5 s6 s7    = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*)
comb81 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -*)
comb12 f s1                      = (f -.- s1 -*<)
comb22 f s1 s2                   = (f -.- s1 -*- s2 -*<)
comb32 f s1 s2 s3                = (f -.- s1 -*- s2 -*- s3 -*<)
comb42 f s1 s2 s3 s4             = (f -.- s1 -*- s2 -*- s3 -*- s4 -*<)
comb52 f s1 s2 s3 s4 s5          = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<)
comb62 f s1 s2 s3 s4 s5 s6       = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<)
comb72 f s1 s2 s3 s4 s5 s6 s7    = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<)
comb82 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -*<)
comb13 f s1                      = (f -.- s1 -*<<)
comb23 f s1 s2                   = (f -.- s1 -*- s2 -*<<)
comb33 f s1 s2 s3                = (f -.- s1 -*- s2 -*- s3 -*<<)
comb43 f s1 s2 s3 s4             = (f -.- s1 -*- s2 -*- s3 -*- s4 -*<<)
comb53 f s1 s2 s3 s4 s5          = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<<)
comb63 f s1 s2 s3 s4 s5 s6       = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<<)
comb73 f s1 s2 s3 s4 s5 s6 s7    = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<<)
comb83 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -*<<)
comb14 f s1                      = (f -.- s1 -*<<<)
comb24 f s1 s2                   = (f -.- s1 -*- s2 -*<<<)
comb34 f s1 s2 s3                = (f -.- s1 -*- s2 -*- s3 -*<<<)
comb44 f s1 s2 s3 s4             = (f -.- s1 -*- s2 -*- s3 -*- s4 -*<<<)
comb54 f s1 s2 s3 s4 s5          = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<<<)
comb64 f s1 s2 s3 s4 s5 s6       = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<<<)
comb74 f s1 s2 s3 s4 s5 s6 s7    = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<<<)
comb84 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -.- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -*<<<)

-- |
-- #state22ns# /(*) meaning / @st1 -> st2 -> a1 -> a2 -> (st1,st2)@
-- /where each argument and result might be individually wrapped/
-- /with a context and might also express a partition./
--
-- #state22i# /(**) see the documentation for '-<-' for justification/
-- /of the type/
--
-- <<docfiles/figs/eqs-moc-pattern-state.png>>
-- <<docfiles/figs/moc-pattern-state.png>>
--
-- The @state@ processes generate process networks corresponding to a
-- simple state machine like in the graph above. This library provides
-- the following patterns/process constructors:
--
-- > state11, state12, state13, state14,
-- > state21, state22, state23, state24,
-- > state31, state32, state33, state34,
-- > state41, state42, state43, state44,
state22 :: MoC e
        => Fun e st1 (Fun e st2 (Fun e a1 (Fun e a2 (Res e st1, Res e st2))))
        -- ^ next state function (<#state22ns *>)
        -> (e [st1], e [st2])
        -- ^ initial state(s) (<#state22i **>)
        -> Stream (e a1)
        -- ^ first input signal
        -> Stream (e a2)
        -- ^ second input signal
        -> (Stream (e st1), Stream (e st2))
        -- ^ output signals mirroring the next state(s).
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

-- | 
-- #stated22ns# /(*) meaning / @st1 -> st2 -> a1 -> a2 -> (st1,st2)@
-- /where each argument and result might be individually wrapped/
-- /with a context and might also express a partition./
--
-- #stated22i# /(**) see the documentation for '-<-' for justification/
-- /of the type/
--
-- <<docfiles/figs/eqs-moc-pattern-stated.png>>
-- <<docfiles/figs/moc-pattern-stated.png>>
-- 
-- The @state@ processes generate process networks corresponding to a
-- simple state machine like in the graph above. The difference
-- between 'state22' and 'stated22' is that the latter outputs the
-- current state rather than the next one. There exists a variant with
-- 0 input signals, in which case the process is a signal
-- generator. This library provides the following patterns/process
-- constructors:
--
-- > stated01, stated02, stated03, stated04,
-- > stated11, stated12, stated13, stated14,
-- > stated21, stated22, stated23, stated24,
-- > stated31, stated32, stated33, stated34,
-- > stated41, stated42, stated43, stated44,
stated22 :: MoC e
        => Fun e st1 (Fun e st2 (Fun e a1 (Fun e a2 (Res e st1, Res e st2))))
        -- ^ next state function (<#stated22ns *>)
        -> (e [st1], e [st2])
        -- ^ initial state(s) (<#stated22i **>)
        -> Stream (e a1)
        -- ^ first input signal
        -> Stream (e a2)
        -- ^ second input signal
        -> (Stream (e st1), Stream (e st2))
        -- ^ output signals mirroring the next state(s).
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
                                            
-- |
-- #moore22ns# /(*) meaning / @st -> a1 -> a2 -> st @ /where each/
-- /argument and result might be individually wrapped with a context/
-- /and might also express a partition./
--
-- #moore22od# /(**) meaning / @st -> (b1, b2) @ /where each argument/
-- /and result might be individually wrapped with a context and might/
-- /also express a partition./
--
-- #moore22i# /(***) see the documentation for '-<-' for justification/
-- /of the type/
--
-- <<docfiles/figs/eqs-moc-pattern-moore.png>>
-- <<docfiles/figs/moc-pattern-moore.png>>
--
-- @moore@ processes model Moore state machines. You can choose one of
-- the pre-defined patterns:
--  
-- > moore11, moore12, moore13, moore14,
-- > moore21, moore22, moore23, moore24,
-- > moore31, moore32, moore33, moore34,
-- > moore41, moore42, moore43, moore44,
moore22 :: MoC e
        => Fun e st (Fun e a1 (Fun e a2 (Res e st)))
        -- ^ next state function (<#moore22ns *>)
        -> Fun e st (Res e b1, Res e b2)
        -- ^ output decoder (<#moore22od **>)
        -> e [st]
        -- ^ intial state (<#moore22i ***>)
        -> Stream (e a1)
        -- ^ first input signal
        -> Stream (e a2)
        -- ^ second input signal
        -> (Stream (e b1), Stream (e b2))
        -- ^ output signals
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

-- |
-- #mealy22ns# /(*) meaning / @st -> a1 -> a2 -> st @ /where each/
-- /argument and result might be individually wrapped with a context/
-- /and might also express a partition./
--
-- #mealy22od# /(**) meaning / @st -> a1 -> a2 -> (b1, b2) @ /where/
-- /each argument and result might be individually wrapped with a/
-- /context and might also express a partition./
--
-- #mealy22i# /(***) see the documentation for '-<-' for justification/
-- /of the type/
--
-- <<docfiles/figs/eqs-moc-pattern-mealy.png>>
-- <<docfiles/figs/moc-pattern-mealy.png>>
--
-- @mealy@ processes model Mealy state machines. You can choose one of
-- the pre-defined patterns:
--  
-- > mealy11, mealy12, mealy13, mealy14,
-- > mealy21, mealy22, mealy23, mealy24,
-- > mealy31, mealy32, mealy33, mealy34,
-- > mealy41, mealy42, mealy43, mealy44,
mealy22 :: MoC e
        => Fun e st (Fun e a1 (Fun e a2 (Res e st)))
        -- ^ next state function (<#mealy22ns *>)
        -> Fun e st (Fun e a1 (Fun e a2 (Res e b1, Res e b2)))
        -- ^ output decoder (<#mealy22od **>)
        -> e [st]
        -- ^ intial state (<#mealy22i ***>)
        -> Stream (e a1)
        -- ^ first input signal
        -> Stream (e a2)
        -- ^ second input signal
        -> (Stream (e b1), Stream (e b2))
        -- ^ output signals
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



-- -- | Utilities for extending the '-*' atom for dealing with tupled
-- -- outputs. Implemented are the following:
-- --
-- -- > -*<, -*<<, -*<<<, -*<<<<, -*<<<<<, -*<<<<<<, -*<<<<<<<, -*<<<<<<<<,
-- (-*<) :: Untimed e
--       => Stream (e ((Ctxt e, Part e b), (Ctxt e, Part e b1)))
--       -- ^ partitioned output tupled with (production) context
--       -> (Stream (e b), Stream (e b1))
--       -- ^ correct (unpartitioned) tupled signals 

infixl 3 -*<, -*<<, -*<<<, -*<<<<, -*<<<<<, -*<<<<<<, -*<<<<<<<, -*<<<<<<<<
(-*<) p        = ((-*),(-*))                                    $$        (p ||<)  
(-*<<) p       = ((-*),(-*),(-*))                               $$$       (p ||<<)
(-*<<<) p      = ((-*),(-*),(-*),(-*))                          $$$$      (p ||<<<)
(-*<<<<) p     = ((-*),(-*),(-*),(-*),(-*))                     $$$$$     (p ||<<<<)
(-*<<<<<) p    = ((-*),(-*),(-*),(-*),(-*),(-*))                $$$$$$    (p ||<<<<<)
(-*<<<<<<) p   = ((-*),(-*),(-*),(-*),(-*),(-*),(-*))           $$$$$$$   (p ||<<<<<<)
(-*<<<<<<<) p  = ((-*),(-*),(-*),(-*),(-*),(-*),(-*),(-*))      $$$$$$$$  (p ||<<<<<<<)
(-*<<<<<<<<) p = ((-*),(-*),(-*),(-*),(-*),(-*),(-*),(-*),(-*)) $$$$$$$$$ (p ||<<<<<<<<)


-- | Attaches a context parameter to a function agument (e.g
-- consumption rates in SDF). Used as kernel function in defining
-- e.g. 'ctxt22'.
warg :: c -> (a -> b) -> (c, a -> b)
warg c f = (c, \x -> f x)

-- | Attaches a context parameter to a function's result (e.g
-- production rates in SDF). Used as kernel function in defining
-- e.g. 'ctxt22'.
wres :: p -> b -> (p, b)
wres p x = (p, x)

-- |
-- <<docfiles/figs/eqs-moc-atom-context.png>>
--
-- Wraps a function with the context needed by some MoCs for their
-- patterns (e.g. rates for SDF).
--
-- > ctxt11, ctxt21, ctxt31, ctxt41, ctxt51, ctxt61, ctxt71, ctxt81, 
-- > ctxt12, ctxt22, ctxt32, ctxt42, ctxt52, ctxt62, ctxt72, ctxt82, 
-- > ctxt13, ctxt23, ctxt33, ctxt43, ctxt53, ctxt63, ctxt73, ctxt83, 
-- > ctxt14, ctxt24, ctxt34, ctxt44, ctxt54, ctxt64, ctxt74, ctxt84,
ctxt22 :: (ctx, ctx)  -- ^ argument contexts (e.g. consumption rates in SDF)
       -> (ctx, ctx)  -- ^ result contexts (e.g. production rates in SDF)
       -> (a1 -> a2 -> (b1, b2))
          -- ^ function on values/partitions of values
       -> (ctx, a1 -> (ctx, a2 -> ((ctx, b1), (ctx, b2))))
          -- ^ context-wrapped form of the previous function

ctxt11 (c1)                      p f = warg c1 $ wres p . f
ctxt21 (c1,c2)                   p f = warg c1 $ ctxt11  c2 p . f
ctxt31 (c1,c2,c3)                p f = warg c1 $ ctxt21 (c2,c3) p . f
ctxt41 (c1,c2,c3,c4)             p f = warg c1 $ ctxt31 (c2,c3,c4) p . f
ctxt51 (c1,c2,c3,c4,c5)          p f = warg c1 $ ctxt41 (c2,c3,c4,c5) p . f
ctxt61 (c1,c2,c3,c4,c5,c6)       p f = warg c1 $ ctxt51 (c2,c3,c4,c5,c6) p . f
ctxt71 (c1,c2,c3,c4,c5,c6,c7)    p f = warg c1 $ ctxt61 (c2,c3,c4,c5,c6,c7) p . f
ctxt81 (c1,c2,c3,c4,c5,c6,c7,c8) p f = warg c1 $ ctxt71 (c2,c3,c4,c5,c6,c7,c8) p . f

ctxt12 (c1)                 (p1,p2) f = warg c1 $ ($$) (wres p1, wres p2) . f
ctxt22 (c1,c2)                   ps f = warg c1 $ ctxt12  c2 ps . f
ctxt32 (c1,c2,c3)                ps f = warg c1 $ ctxt22 (c2,c3) ps . f
ctxt42 (c1,c2,c3,c4)             ps f = warg c1 $ ctxt32 (c2,c3,c4) ps . f
ctxt52 (c1,c2,c3,c4,c5)          ps f = warg c1 $ ctxt42 (c2,c3,c4,c5) ps . f
ctxt62 (c1,c2,c3,c4,c5,c6)       ps f = warg c1 $ ctxt52 (c2,c3,c4,c5,c6) ps . f
ctxt72 (c1,c2,c3,c4,c5,c6,c7)    ps f = warg c1 $ ctxt62 (c2,c3,c4,c5,c6,c7) ps . f
ctxt82 (c1,c2,c3,c4,c5,c6,c7,c8) ps f = warg c1 $ ctxt72 (c2,c3,c4,c5,c6,c7,c8) ps . f

ctxt13 (c1)              (p1,p2,p3) f = warg c1 $ ($$$) (wres p1, wres p2, wres p3) . f
ctxt23 (c1,c2)                   ps f = warg c1 $ ctxt13  c2 ps . f
ctxt33 (c1,c2,c3)                ps f = warg c1 $ ctxt23 (c2,c3) ps . f
ctxt43 (c1,c2,c3,c4)             ps f = warg c1 $ ctxt33 (c2,c3,c4) ps . f
ctxt53 (c1,c2,c3,c4,c5)          ps f = warg c1 $ ctxt43 (c2,c3,c4,c5) ps . f
ctxt63 (c1,c2,c3,c4,c5,c6)       ps f = warg c1 $ ctxt53 (c2,c3,c4,c5,c6) ps . f
ctxt73 (c1,c2,c3,c4,c5,c6,c7)    ps f = warg c1 $ ctxt63 (c2,c3,c4,c5,c6,c7) ps . f
ctxt83 (c1,c2,c3,c4,c5,c6,c7,c8) ps f = warg c1 $ ctxt73 (c2,c3,c4,c5,c6,c7,c8) ps . f

ctxt14 (c1)           (p1,p2,p3,p4) f = warg c1 $ ($$$$) (wres p1, wres p2, wres p3, wres p4) . f
ctxt24 (c1,c2)                   ps f = warg c1 $ ctxt14  c2 ps . f
ctxt34 (c1,c2,c3)                ps f = warg c1 $ ctxt24 (c2,c3) ps . f
ctxt44 (c1,c2,c3,c4)             ps f = warg c1 $ ctxt34 (c2,c3,c4) ps . f
ctxt54 (c1,c2,c3,c4,c5)          ps f = warg c1 $ ctxt44 (c2,c3,c4,c5) ps . f
ctxt64 (c1,c2,c3,c4,c5,c6)       ps f = warg c1 $ ctxt54 (c2,c3,c4,c5,c6) ps . f
ctxt74 (c1,c2,c3,c4,c5,c6,c7)    ps f = warg c1 $ ctxt64 (c2,c3,c4,c5,c6,c7) ps . f
ctxt84 (c1,c2,c3,c4,c5,c6,c7,c8) ps f = warg c1 $ ctxt74 (c2,c3,c4,c5,c6,c7,c8) ps . f
