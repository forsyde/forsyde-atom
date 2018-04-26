{-# LANGUAGE PostfixOperators, TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC
-- Copyright   :  (c) George Ungureanu, 2015-2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the core entities of the MoC layer: interfaces
-- for atoms and process constructors as patterns of atoms. It does
-- /NOT/ export any implementation or instance of any specific
-- MoC. For an overview about atoms, layers and patterns, please refer
-- to the "ForSyDe.Atom" module documentation, and for an overview of
-- the MoC layer entities refer to <ForSyDe-Atom.html#g:3 the MoC layer section>.
--
-- The MoCs currently implemented in ForSyDe-Atom are shown in the
-- <#i:MoC instances section>, and they can be used in designs by
-- importing their respective modules:
--
-- * "ForSyDe.Atom.MoC.CT"
-- * "ForSyDe.Atom.MoC.DE"
-- * "ForSyDe.Atom.MoC.SY"
-- * "ForSyDe.Atom.MoC.SDF"
--
-- __IMPORTANT!!!__
-- see the <ForSyDe-Atom.html#naming_conv naming convention> rules
-- on how to interpret, use and develop your own constructors.
----------------------------------------------------------------------

module ForSyDe.Atom.MoC(
  
  -- * Atoms
  
  MoC(..),

  -- * Process constructors

  -- | Process constructors are defined as patterns of MoC
  -- atoms. Check the <ForSyDe-Atom.html#naming_conv naming convention>
  -- of the API in the page description.
  
  delay, (-&>-),
  
  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,
  comb51, comb52, comb53, comb54,
  comb61, comb62, comb63, comb64,
  comb71, comb72, comb73, comb74,
  comb81, comb82, comb83, comb84,

  reconfig11, reconfig12, reconfig13, reconfig14,
  reconfig21, reconfig22, reconfig23, reconfig24,
  reconfig31, reconfig32, reconfig33, reconfig34,
  reconfig41, reconfig42, reconfig43, reconfig44,
  reconfig51, reconfig52, reconfig53, reconfig54,
  reconfig61, reconfig62, reconfig63, reconfig64,
  reconfig71, reconfig72, reconfig73, reconfig74,
  reconfig81, reconfig82, reconfig83, reconfig84,

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

-- | This is a type class defining interfaces for the MoC layer
-- atoms. Each model of computation exposes its tag system through a
-- unique event type which is an instance of this class, defining
-- \( T \times V \).
--
-- #context# To express all possible MoCs which can be described using
-- a /tagged/ /signal/ /model/ we need to capture the most general
-- form of their atoms. This means that under the same interface we
-- need to describe atoms with different execution regimes. Recall
-- that atoms are in principle applicative functors which act upon one
-- input signal at a time and partially apply functions. Depending on
-- the execution regime, these function might or might not need
-- additional parameters to determine the behavior for evaluating each
-- argument. These additional parameters we call, in loose terms, as
-- the /execution context/.
--
-- [execution context] Additional information which, paired with a
--   function, completely determines the behavior of a MoC atom
--   (i.e. process). 
--
-- <<fig/eqs-moc-atom-formatted-func.png>>
--
-- The left-hand side expression above shows the most general notation
-- used to describe a function with /n/ inputs of (possibly different)
-- types \(\alpha\) and /m/ outputs of (possibly different) types
-- \(\beta\) executed in context \(\Gamma\). The right-hand side
-- expression shows that in ForSyDe-Atom context is associated with
-- each and every argument in order to enable the applicative
-- mechanisms. Depending on the MoC, \(\breve\alpha\) would translate
-- to:
--
-- <<fig/eqs-moc-atom-formatted-arg.png>>
--
-- One example of execution context is the consumption and production
-- rates of for data flow MoCs (e.g. 'ForSyDe.Atom.MoC.SDF.SDF'). In
-- this case the passed functions are defined over "sequences" or
-- "partitions" of events, i.e. groupings of events with the same
-- partial order in relation to a process firing.
--
-- While in the example above the execution context can be extracted
-- from the type information, working with type-level parameters is
-- not a trivial task in Haskell, especially if we want to describe a
-- general and extensible type class. This is why we have chosen a
-- pragmatic approach in implementing the 'MoC' class:
--
-- * any (possible) Cartesian product which denotes a partition of
--   /&#945;/ is represented using a recursive type, namely a list
--   [/&#945;/].
--
-- * as the execution context cannot (or can hardly) be extracted from
--   the recursive type, in the most general case we pass both context
--   /and/ argument as a pair (see each instance in particular). To
--   aid in pairing contexts with each argument in a function, the
--   general purpose @ctxt@ utilities are provided (see 'ctxt22').
--
-- * this artifice is masked using the generic type families 'Fun' and
--   'Ret'.
class (Applicative e) => MoC e where

  -- | This is a type family alias(*) for a context-bound function passed
  -- as an argument to a MoC atom. It can be regarded as an enhanced
  -- @->@ type operator, specific to each MoC. 
  -- 
  -- <<fig/eqs-moc-atom-function.png>>
  --
  -- /(*) While hiding the explicit definition of arguments, this/
  -- /implementation choice certainly has its advantages in avoiding/
  -- /unnecessary or redundant type constructors (see version 0.1.1 and/
  -- /prior). Aliases are replaced at compile time, thus not affecting/
  -- /run-time performance./
  type Fun e a b

  -- | Like 'Fun', this alias hides a context-bound value
  -- (e.g. function return). This alias is needed for utilities to
  -- recreate clean types again (see '-*').
  -- 
  -- <<fig/eqs-moc-atom-result.png>>
  type Ret e b
  
  -- | This atom is mapping a function on values (in the presence of a
  -- context) to a signal, i.e. stream of tagged events. As ForSyDe
  -- deals with /determinate/, /functional/ processes, this atom
  -- defines the (only) /behavior/ of a process in rapport to one
  -- input signal <ForSyDe-Atom.html#lee98 [Lee98]>.
  --
  -- <<fig/eqs-moc-atom-dot.png>>
  (-.-) :: Fun e a b -> Stream (e a) -> Stream (e b)
  
  -- | This atom synchronizes two signals, one carrying functions on
  -- values (in the presence of a context), and the other containing
  -- values. During the synchronization it applies the function(s)
  -- carried by the former signal on the values carried by the
  -- latter. This atom defines a /relation/ between two signals
  -- <ForSyDe-Atom.html#lee98 [Lee98]>.
  -- 
  -- <<fig/eqs-moc-atom-star.png>>
  (-*-) :: Stream (e (Fun e a b)) -> Stream (e a) -> Stream (e b)

  -- | Artificial /utility/ which drops the context and/or partitioning
  -- yielding a clean signal type.
  --
  -- <<fig/eqs-moc-atom-post.png>>
  (-*)  :: Stream (e (Ret e b)) -> Stream (e b)

  -- | This atom appends a (partition of) events at the beginning of a
  -- signal. This atom is necessary to ensure /complete partial order/
  -- of a signal and assures the /least upper bound/ necessary for
  -- example in the evaluation of feedback loops
  -- <ForSyDe-Atom.html#lee98 [Lee98]>.
  --
  -- <<fig/eqs-moc-atom-pre.png>>
  --
  -- Notice the difference between the formal and the implemented type
  -- signatures. In the implementation the value/partition is wrapped
  -- inside an event type to enable smooth composition. You might also
  -- notice the type for the "initial event(s)" as being wrapped
  -- inside a signal constructor. This allows defining an DSL for this
  -- layer which is centered around signals exclusively, while also
  -- enabling to define atoms as homomorphisms to certain extent
  -- <ForSyDe-Atom.html#bird97 [Bird97]>. Certain MoCs might have
  -- additional constraints on the first operand to be finite.
  (-<-) :: Stream (e a) -> Stream (e a) -> Stream (e a)
   
  -- | This atom allows the manipulation of tags in a signal in a
  -- restrictive way which preserves /monotonicity/ and /continuity/
  -- in a process <ForSyDe-Atom.html#lee98 [Lee98]>, namely by
  -- “phase-shifting” all tags in a signal with the appropriate metric
  -- corresponding to each MoC. Thus it preserves the characteristic
  -- function intact <ForSyDe-Atom.html#sander04 [Sander04]>.
  --
  -- <<fig/eqs-moc-atom-phi.png>>
  -- 
  -- As with the '-<-' atom, we can justify the type signature for
  -- smooth composition and the definition of atoms as homomorphisms
  -- to certain extent. This in turn allows the interpretation of the
  -- '-&-' operator as "aligning the phases" of two signals: the
  -- second operand is aligned based on the first.
  (-&-) :: Stream (e a) -> Stream (e a) -> Stream (e a)

infixl 3 -&>-
-- | <<fig/eqs-moc-pattern-delay.png>>
--   <<fig/moc-pattern-delay.png>>
--
-- The 'delay' process provides both initial token(s) and shifts the
-- phase of the signal. In other words, it "delays" a signal with
-- one or several events.
--
-- There is also an infix variant '-&>-' (@infixl 3@). To justify the
-- first argument, see the documentation of the '-<-' atom.
delay i xs = i -<- (i -&- xs)
i -&>- xs = delay i xs          

-- |  #comb22f# /(*) to be read / @a1 -> a2 -> (b1, b2)@ /where each/
-- /argument and result might be individually wrapped with a context/
-- /and might also express a partition./
--
-- <<fig/eqs-moc-pattern-comb.png>>
-- <<fig/moc-pattern-comb.png>>
--
-- The @comb@ processes takes care of synchronization between signals
-- and maps combinatorial functions on their event values. 
--
-- This library exports constructors of type @comb[1-8][1-4]@.
comb22 :: (MoC e)
       => (Fun e a1 (Fun e a2 (Ret e b1, Ret e b2)))
       -- ^ combinational function (<#comb22f *>)
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

-- |  #reconfig22f# /(*) to be read / @a1 -> a2 -> (b1, b2)@ /where each/
-- /argument and result might be individually wrapped with a context/
-- /and might also express a partition./
--
-- <<fig/eqs-moc-pattern-reconfig.png>>
-- <<fig/moc-pattern-reconfig.png>>
--
-- The @reconfig@ processes constructs adaptive processes, where the
-- first signal carries functions, and it is synchronized with all the
-- other signals. 
--
-- This library exports constructors of type @reconfig[1-8][1-4]@.
reconfig22 :: (MoC e)
       => Stream (e (Fun e a1 (Fun e a2 (Ret e b1, Ret e b2))))
       -- ^ signal carrying functions (<#reconfig22f *>)
       -> Stream (e a1)                  -- ^ first input signal
       -> Stream (e a2)                  -- ^ second input signal
       -> (Stream (e b1), Stream (e b2)) -- ^ two output signals
reconfig11 sf s1                      = (sf -*- s1 -*)
reconfig21 sf s1 s2                   = (sf -*- s1 -*- s2 -*)
reconfig31 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*)
reconfig41 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*)
reconfig51 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*)
reconfig61 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*)
reconfig71 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*)
reconfig81 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -*)
reconfig12 sf s1                      = (sf -*- s1 -*<)
reconfig22 sf s1 s2                   = (sf -*- s1 -*- s2 -*<)
reconfig32 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*<)
reconfig42 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*<)
reconfig52 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<)
reconfig62 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<)
reconfig72 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<)
reconfig82 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -*<)
reconfig13 sf s1                      = (sf -*- s1 -*<<)
reconfig23 sf s1 s2                   = (sf -*- s1 -*- s2 -*<<)
reconfig33 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*<<)
reconfig43 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*<<)
reconfig53 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<<)
reconfig63 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<<)
reconfig73 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<<)
reconfig83 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -*<<)
reconfig14 sf s1                      = (sf -*- s1 -*<<<)
reconfig24 sf s1 s2                   = (sf -*- s1 -*- s2 -*<<<)
reconfig34 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*<<<)
reconfig44 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*<<<)
reconfig54 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<<<)
reconfig64 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<<<)
reconfig74 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<<<)
reconfig84 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -*<<<)

-- |
-- #state22ns# /(*) meaning / @st1 -> st2 -> a1 -> a2 -> (st1,st2)@
-- /where each argument and result might be individually wrapped/
-- /with a context and might also express a partition./
--
-- #state22i# /(**) see the documentation for '-<-' for justification/
-- /of the type/
--
-- <<fig/eqs-moc-pattern-state.png>>
-- <<fig/moc-pattern-state.png>>
--
-- The @state@ processes generate process networks corresponding to a
-- simple state machine like in the graph above. 
--
-- This library exports constructors of type @state[1-4][1-4]@.
state22 :: MoC e
        => Fun e st1 (Fun e st2 (Fun e a1 (Fun e a2 (Ret e st1, Ret e st2))))
        -- ^ next state function (<#state22ns *>)
        -> (Stream (e st1), Stream (e st2))
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
-- <<fig/eqs-moc-pattern-stated.png>>
-- <<fig/moc-pattern-stated.png>>
-- 
-- The @state@ processes generate process networks corresponding to a
-- simple state machine like in the graph above. The difference
-- between 'state22' and 'stated22' is that the latter outputs the
-- current state rather than the next one. There exists a variant with
-- 0 input signals, in which case the process is a signal
-- generator.
--
-- This library exports constructors of type @stated[0-4][1-4]@.
stated22 :: MoC e
        => Fun e st1 (Fun e st2 (Fun e a1 (Fun e a2 (Ret e st1, Ret e st2))))
        -- ^ next state function (<#stated22ns *>)
        -> (Stream (e st1), Stream (e st2))
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
-- <<fig/eqs-moc-pattern-moore.png>>
-- <<fig/moc-pattern-moore.png>>
--
-- The @moore@ processes model Moore state machines.
--  
-- This library exports constructors of type @moore[1-4][1-4]@.
moore22 :: MoC e
        => Fun e st (Fun e a1 (Fun e a2 (Ret e st)))
        -- ^ next state function (<#moore22ns *>)
        -> Fun e st (Ret e b1, Ret e b2)
        -- ^ output decoder (<#moore22od **>)
        -> Stream (e st)
        -- ^ initial state (<#moore22i ***>)
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
-- <<fig/eqs-moc-pattern-mealy.png>>
-- <<fig/moc-pattern-mealy.png>>
--
-- The @mealy@ processes model Mealy state machines.
--  
-- This library exports constructors of type @mealy[1-4][1-4]@.
mealy22 :: MoC e
        => Fun e st (Fun e a1 (Fun e a2 (Ret e st)))
        -- ^ next state function (<#mealy22ns *>)
        -> Fun e st (Fun e a1 (Fun e a2 (Ret e b1, Ret e b2)))
        -- ^ output decoder (<#mealy22od **>)
        -> Stream (e st)
        -- ^ initial state (<#mealy22i ***>)
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



-- | Attaches a context parameter to a function argument (e.g
-- consumption rates in SDF). Used as kernel function in defining
-- e.g. 'ctxt22'.
warg :: c -> (a -> b) -> (c, a -> b)
warg c f = (c, \x -> f x)

-- | Attaches a context parameter to a function's result (e.g
-- production rates in SDF). Used as kernel function in defining
-- e.g. 'ctxt22'.
wres :: p -> b -> (p, b)
wres p x = (p, x)

-- | <<fig/eqs-moc-atom-context.png>>
--
-- Wraps a function with the context needed by some MoCs for their
-- constructors (e.g. rates in SDF).
--
-- This library exports wrappers of type @ctxt[1-8][1-4]@.
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


infixl 3 -*<, -*<<, -*<<<, -*<<<<, -*<<<<<, -*<<<<<<, -*<<<<<<<, -*<<<<<<<<
-- | Utilities for extending the '-*' atom for dealing with tupled
-- outputs. This library exports operators of form @-*<{1,8}@.
(-*<) :: MoC e => Stream (e (Ret e b1, Ret e b2)) -> (Stream (e b1), Stream (e b2))
(-*<) p        = ((-*),(-*))                                    $$        (p ||<)  
(-*<<) p       = ((-*),(-*),(-*))                               $$$       (p ||<<)
(-*<<<) p      = ((-*),(-*),(-*),(-*))                          $$$$      (p ||<<<)
(-*<<<<) p     = ((-*),(-*),(-*),(-*),(-*))                     $$$$$     (p ||<<<<)
(-*<<<<<) p    = ((-*),(-*),(-*),(-*),(-*),(-*))                $$$$$$    (p ||<<<<<)
(-*<<<<<<) p   = ((-*),(-*),(-*),(-*),(-*),(-*),(-*))           $$$$$$$   (p ||<<<<<<)
(-*<<<<<<<) p  = ((-*),(-*),(-*),(-*),(-*),(-*),(-*),(-*))      $$$$$$$$  (p ||<<<<<<<)
(-*<<<<<<<<) p = ((-*),(-*),(-*),(-*),(-*),(-*),(-*),(-*),(-*)) $$$$$$$$$ (p ||<<<<<<<<)
