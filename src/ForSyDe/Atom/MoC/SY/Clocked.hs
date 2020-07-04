{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK prune #-}
---------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.SY.Clocked
-- Copyright   :  (c) George Ungureanu, 2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a small experimental library for modeling multi-clock rate SY systems in
-- the style of Lustre <ForSyDe-Atom.html#halbwachs91 [Halbwachs91]>. It is
-- implemented mainly on top of the "ForSyDe.Atom.MoC.SY" library, re-using it to the
-- full extent, and it represents Lustre language constructs as specific
-- patterns. Instead of developing and representing an inherent clock calculus like
-- Lustre, this library merely propagates "no-ops" as absent
-- (i.e. 'ForSyDe.Atom.ExB.Absent.Abst') events and utilises the algebra of
-- "ForSyDe.Atom.ExB" to model clock rates. 
--
-- Internally, a "ForSyDe.Atom.MoC.SY.Clocked" process constructor represents
-- implicitly a "ForSyDe.Atom.MoC.SY" MoC layer wrapping a "ForSyDe.Atom.ExB.Absent"
-- behavior extension layer. Under these asumptions the "ForSyDe.Atom.MoC.SY.Clocked"
-- models can be interpreted as follows:
--
-- * the host "ForSyDe.Atom.MoC.SY" processes operate at the basic clock rate. I.e. a
--   signal where all events are present is active in the basic time scale.
--
-- * the wrapped "ForSyDe.Atom.ExB" behaviors operate on and propagate absent/present
--   events based on the operating clock rate. 'ForSyDe.Atom.ExB.Absent.Abst' means
--   that an event "did not occur". Combining a present with an absent event in any
--   regular operation (see 'comb22') would violate the clock constraints of
--   well-formed systems (see <ForSyDe-Atom.html#halbwachs91 [Halbwachs91]>) and would
--   throw a runtime error.
--
-- * Although "ForSyDe.Atom.MoC.SY.Clocked" is hosted on "ForSyDe.Atom.MoC.SY", the
--   'ForSyDe.Atom.ExB.Absent.Abst' "value" has no true meaning in a pure SY reactive
--   system. Therefore to interface between these two SY domains should always be made
--   through a 'toDE'/'fromDE' interface. __OBS:__ these interfaces are still
--   experimental and higly unsafe especially when considering feedback loops.
---------------------------------------------------------------------
module ForSyDe.Atom.MoC.SY.Clocked (
  Signal,

  -- * Lustre constructs

  -- | These are process constructors which imitate the behavior of Lustre constructs
  -- according to our "ForSyDe.Atom.MoC.SY.Clocked" definition.
  pre, (->-), when, current,

  -- * Auxiliary constructs

  -- | These are utility process constructors used in combination with the main
  -- constructs.
  is, whenIs1, whenIs2, whenIs3,
  whenPres1, whenPres2, whenPres3,
  filter, fill,

  -- * Regular constructors

  -- | These are the regular process constructors defined as patterns in the
  -- "ForSyDe.Atom.MoC" module, but adapted for the absent-extended behaviors.  
  delay,

  comb11, comb12, comb13, comb14,
  comb21, comb22, comb23, comb24,
  comb31, comb32, comb33, comb34,
  comb41, comb42, comb43, comb44,

  stated11, stated12, stated13, 
  stated21, stated22, stated23, 
  stated31, stated32, stated33, 

  state11, state12, state13, 
  state21, state22, state23, 
  state31, state32, state33,
  
  moore11, moore12, moore13, 
  moore21, moore22, moore23, 
  moore31, moore32, moore33, 

  mealy11, mealy12, mealy13, 
  mealy21, mealy22, mealy23, 
  mealy31, mealy32, mealy33
  
  ) where

import qualified ForSyDe.Atom.ExB as B
import           ForSyDe.Atom.ExB.Absent (AbstExt(..))
import           ForSyDe.Atom.MoC.SY.Core hiding (Signal)
import qualified ForSyDe.Atom.MoC.SY.Lib as SY
import           ForSyDe.Atom.MoC.Stream
import           ForSyDe.Atom.Utility.Tuple
import           Prelude hiding (filter)

-- | Alias showing that in the "ForSyDe.Atom.MoC.SY.Clocked" domain, a signal is
-- considered a stream of absent-extended SY events.
type Signal a = Stream (SY (AbstExt a))

infixl 3 ->-

------- WHEN, FILTER, FILL, HOLD -------

-- | The @->@ operator in Lustre.
--
-- >>> let s1 = signal [Prst 1, Prst 2, Prst 3, Prst 4, Prst 5]
-- >>> unit (Prst 10) ->- s1
-- {10,2,3,4,5}
(->-) :: Signal a -> Signal a -> Signal a
a ->- b = headS a :- tailS b

-- | The @pre@ construct in Lustre. __OBS:__ instead of /nil/, the first value is an
-- 'Abst'. /TODO:/ unclear if this breaks the SY assumption.
--
-- >>> let s1 = signal [Prst 1, Prst 2, Prst 3, Prst 4, Prst 5]
-- >>> pre s1
-- {⟂,1,2,3,4,5}
pre :: Signal a -> Signal a
pre a = SY Abst :- a

-- | Determines the existence of events in the (left) signal based on the (right)
-- signal of boolean values (conditions). In Lustre this construct is used to generate
-- other clock rates.
--
-- >>> let s1   = (signal . map Prst) [1,2,3,4,5]
-- >>> let pred = signal [Prst False, Abst, Prst True, Prst False, Prst True]
-- >>> s1 `when` pred
-- {⟂,⟂,3,⟂,5}
when :: Signal a    -- ^ Input signal
     -> Signal Bool -- ^ Signal of predicates
     -> Signal a    -- ^ Output signal
when s p = SY.comb21 B.filter p s

-- | Holds the last non-absent value in a signal, like the @current@ construct in
-- Lustre. __OBS:__ instead of /nil/, the first value is user-defined.
--
-- >>> let s1   = signal [Abst, Abst, Prst 1, Prst 2, Abst, Prst 3]
-- >>> current 0 s1
-- {0,0,1,2,2,3}
current :: a
        -- ^ Value to fill with in case there was no previous value
        -> Signal a -- ^ Input
        -> Signal a -- ^ Output
current i = SY.comb11 B.extend . SY.state11 B.degen i

-----------------------------------------------------------

-- | Applies a predicate function on a signal of absent-extended events.
--
-- >>> let s1   = signal $ map Prst [1,2,3,4,5]
-- >>> s1 `is` (>3)
-- {False,False,False,True,True}
--
-- It is useful in combination with 'when'.
--
-- >>> let s1 = (signal . map Prst) [1,2,3,4,5]
-- >>> let s2 = (signal . map Prst) [11,22,33,44,55]
-- >>> s2 `when` (s1 `is` (>3))
-- {⟂,⟂,⟂,44,55}
is :: Signal a -> (a -> Bool) -> Signal Bool
is  s p = SY.comb11 (B.res11 p) s


-- | Serializes 'when' with 'is' for up to three input signals.
--
-- Constructors @whenIs[1-3]@
--
-- >>> let s1 = (signal . map Prst) [1,2,3,4,5]
-- >>> let s2 = (signal . map Prst) [11,22,33,44,55]
-- >>> whenIs2 (>3) s1 s1 s2
-- ({⟂,⟂,⟂,4,5},{⟂,⟂,⟂,44,55})
whenIs2 :: (a -> Bool)
        -> Signal a
        -> Signal b1 -> Signal b2
        -> (Signal b1, Signal b2)
whenIs1 :: (a -> Bool) -> Signal a
        -> Signal b1 -> Signal b1
whenIs3 :: (a -> Bool) -> Signal a
        -> Signal b1 -> Signal b2 -> Signal b3
        -> (Signal b1, Signal b2, Signal b3)
        
whenIs1 p = SY.comb21 (\a b1 -> B.filter (B.res11 p a) b1)
whenIs2 p = SY.comb32 filterF
  where filterF a b1 b2 
          = let tupd = B.res21 (,) b1 b2
            in (B.filter (B.res11 p a) tupd |<)
whenIs3 p = SY.comb43 filterF
  where filterF a b1 b2 b3
          = let tupd = B.res31 (,,) b1 b2 b3
            in (B.filter (B.res11 p a) tupd |<<)

-- | Same as 'whenIs2' but triggering the output events only based on the presence of
-- the first input rather than a boolean.
--
-- >>> let s1 = signal $ map Prst [1,2,3,4,5]
-- >>> let sp = signal [Prst 1, Prst 1, Abst, Prst 1, Abst]
-- >>> whenPres1 sp s1
-- {1,2,⟂,4,⟂}
whenPres2 = whenIs2 (const True)
whenPres1 = whenIs1 (const True)
whenPres3 = whenIs3 (const True)


-- | Filters out values to 'Abst' if they do not fulfill a predicate function,
-- i.e. applies @whenIs1@ on itself.
--
-- >>> let s1   = (signal . map Prst) [1,2,3,4,5]
-- >>> filter (>3) s1
-- {⟂,⟂,⟂,4,5}
filter :: (a -> Bool) -- ^ Predicate function
       -> Signal a    -- ^ Input signal
       -> Signal a    -- ^ Output signal
filter p s = whenIs1 p s s


-- | Fills absent events with a user-defined value.
--
-- >>> let s1   = signal [Abst, Abst, Prst 1, Prst 2, Abst, Prst 3]
-- >>> fill 0 s1
-- {0,0,1,2,0,3}
fill :: a        -- ^ Value to fill with
     -> Signal a -- ^ Input
     -> Signal a -- ^ Output
fill x = SY.comb11 (B.extend . B.degen x)


------------------------------------------------------------------

-- | The @delay@ process "delays" a signal with one event. Serializes a 'pre' and a
-- '->-'.
--
-- >>> let s = signal [Prst 1,Prst 2,Prst 3,Abst,Prst 5]
-- >>> delay 0 s
-- {0,1,2,3,⟂,5}
delay :: a         -- ^ initial value
      -> Signal a  -- ^ input signal
      -> Signal a  -- ^ output signal
delay = SY.delay . Prst

-- | @comb@ processes map combinatorial functions on signals on synchronized input
-- signals. It implements the @comb@ pattern (see 'ForSyDe.Atom.MoC.comb22'), and
-- implicitly applies a resolution between absent-extended events (see
-- 'ForSyDe.Atom.ExB.res22')
-- 
-- Constructors: @comb[1-4][1-4]@.
--
-- >>> let s1 = (signal . map Prst) [1..]
-- >>> let s2 = signal [Prst 1,Prst 1,Abst,Prst 1,Prst 1]
-- >>>  comb11 (+1) s2
-- {2,2,⟂,2,2}
-- >>> comb22 (\a b-> (a+b,a-b)) s2 s2
-- ({2,2,⟂,2,2},{0,0,⟂,0,0})
--
-- Combining two signals at different clock rates throws a runtime error:
--
-- > comb22 (\a b-> (a+b,a-b)) s1 s2
-- > ({2,3,*** Exception: [ExB.Absent] Illegal occurrence of an absent and present event
comb22 :: (a1 -> a2 -> (b1, b2)) -- ^ function on values
       -> Signal a1              -- ^ first input signal
       -> Signal a2              -- ^ second input signal
       -> (Signal b1, Signal b2) -- ^ two output signals
comb11 :: (a1 -> b1)
       -> Signal a1 -> Signal b1
comb12 :: (a1 -> (b1, b2))
       -> Signal a1 -> (Signal b1, Signal b2)
comb13 :: (a1 -> (b1, b2, b3))
       -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
comb14 :: (a1 -> (b1, b2, b3, b4))
       -> Signal a1 -> (Signal b1, Signal b2, Signal b3, Signal b4)
comb21 :: (a1 -> a2 -> b1)
       -> Signal a1 -> Signal a2 -> Signal b1
comb23 :: (a1 -> a2 -> (b1, b2, b3))
       -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
comb24 :: (a1 -> a2 -> (b1, b2, b3, b4))
       -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3, Signal b4)
comb31 :: (a1 -> a2 -> a3 -> b1)
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
comb32 :: (a1 -> a2 -> a3 -> (b1, b2))
       -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
comb33 :: (a1 -> a2 -> a3 -> (b1, b2, b3))
       -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)
comb34 :: (a1 -> a2 -> a3 -> (b1, b2, b3, b4))
       -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3, Signal b4)
comb41 :: (a1 -> a2 -> a3 -> a4 -> b1)
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> Signal b1
comb42 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2)
comb43 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3)
comb44 :: (a1 -> a2 -> a3 -> a4 -> (b1, b2, b3, b4))
       -> Signal a1 -> Signal a2 -> Signal a3 -> Signal a4 -> (Signal b1, Signal b2, Signal b3, Signal b4)

comb11 = SY.comb11 . B.res11 
comb12 = SY.comb12 . B.res12 
comb13 = SY.comb13 . B.res13 
comb14 = SY.comb14 . B.res14
comb21 = SY.comb21 . B.res21
comb22 = SY.comb22 . B.res22
comb23 = SY.comb23 . B.res23
comb24 = SY.comb24 . B.res24
comb31 = SY.comb31 . B.res31
comb32 = SY.comb32 . B.res32
comb33 = SY.comb33 . B.res33
comb34 = SY.comb34 . B.res34
comb41 = SY.comb41 . B.res41
comb42 = SY.comb42 . B.res42
comb43 = SY.comb43 . B.res43
comb44 = SY.comb44 . B.res44

-- internal: reconstructs a degenerated signal based on an observation signal
react1 rs = whenPres1 rs . SY.comb11 B.extend
react2 rs = (><) (whenPres2 rs) . ($$) (ext,ext)
  where ext = SY.comb11 B.extend
react3 rs = (><<) (whenPres3 rs) . ($$$) (ext,ext,ext)
  where ext = SY.comb11 B.extend
  
-- | @state@ is a state machine without an output decoder, that reacts
-- instantaneously. It implements the @state@ pattern (see
-- 'ForSyDe.Atom.MoC.state22'), and operates on 'ForSyDe.Atom.ExB.degen'erated
-- absent-extended values (see 'ForSyDe.Atom.ExB.ignore22') in order to propagate
-- absent events properly and not raise absent-present resolution errors.
--
-- Constructors: @state[1-3][1-3]@.
--
-- >>> let ones = takeS 8 $ SY.constant1 (Prst 1)
-- >>> let b = (signal . map Prst) [False,True,False,False,True,True,False,True]
-- >>> ones
-- {1,1,1,1,1,1,1,1}
-- >>> b
-- {False,True,False,False,True,True,False,True}
-- >>> state11 (+) 1 ones
-- {2,3,4,5,6,7,8,9}
-- >>> state11 (+) 1 (ones `when` b)
-- {⟂,2,⟂,⟂,3,4,⟂,5}
-- >>> state11 (+) 1 ones `when` b
-- {⟂,3,⟂,⟂,6,7,⟂,9}
state22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2))
           -- ^ next state function
           -> (b1, b2)
           -- ^ initial state values
           -> Signal a1
           -- ^ first input signal
           -> Signal a2
           -- ^ second input signal
           -> (Signal b1, Signal b2) -- ^ output signals
state11 :: (b1 -> a1 -> b1) -> b1
        -> Signal a1 -> Signal b1
state12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> (Signal b1, Signal b2)
state13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
state21 :: (b1 -> a1 -> a2 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal b1
state23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
state31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
state32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
state33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)

state11 ns i s1 = react1 s1 $ SY.state11 (B.ignore11 ns) i s1
state12 ns i s1 = react2 s1 $ SY.state12 (B.ignore12 ns) i s1
state13 ns i s1 = react3 s1 $ SY.state13 (B.ignore13 ns) i s1
state21 ns i s1 = (react1 s1 . SY.state21 (B.ignore21 ns) i s1)
state22 ns i s1 = (react2 s1 . SY.state22 (B.ignore22 ns) i s1)
state23 ns i s1 = (react3 s1 . SY.state23 (B.ignore23 ns) i s1)
state31 ns i s1 s2 = (react1 s1 . SY.state31 (B.ignore31 ns) i s1 s2)
state32 ns i s1 s2 = (react2 s1 . SY.state32 (B.ignore32 ns) i s1 s2)
state33 ns i s1 s2 = (react3 s1 . SY.state33 (B.ignore33 ns) i s1 s2)

------- STATED -------

-- | @stated@ is a state machine without an output decoder. It implements the @stated@
-- pattern (see 'ForSyDe.Atom.MoC.stated22'), and operates on
-- 'ForSyDe.Atom.ExB.degen'erated absent-extended values (see
-- 'ForSyDe.Atom.ExB.ignore22') in order to propagate absent events properly and not
-- raise absent-present resolution errors.
--
-- Constructors: @stated[1-3][1-3]@.
--
-- >>> let ones = takeS 8 $ SY.constant1 (Prst 1)
-- >>> let b = (signal . map Prst) [False,True,False,False,True,True,False,True]
-- >>> ones
-- {1,1,1,1,1,1,1,1}
-- >>> b
-- {False,True,False,False,True,True,False,True}
-- >>> stated11 (+) 1 ones
-- {1,2,3,4,5,6,7,8}
-- >>> stated11 (+) 1 (ones `when` b)
-- {⟂,1,⟂,⟂,2,3,⟂,4}
-- >>> stated11 (+) 1 ones `when` b
-- {⟂,2,⟂,⟂,5,6,⟂,8}
stated22 :: (b1 -> b2 -> a1 -> a2 -> (b1, b2))
           -- ^ next stated function
           -> (b1, b2)
           -- ^ initial stated values
           -> Signal a1
           -- ^ first input signal
           -> Signal a2
           -- ^ second input signal
           -> (Signal b1, Signal b2) -- ^ output signals
stated11 :: (b1 -> a1 -> b1) -> b1
        -> Signal a1 -> Signal b1
stated12 :: (b1 -> b2 -> a1 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> (Signal b1, Signal b2)
stated13 :: (b1 -> b2 -> b3 -> a1 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
stated21 :: (b1 -> a1 -> a2 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal b1
stated23 :: (b1 -> b2 -> b3 -> a1 -> a2 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
stated31 :: (b1 -> a1 -> a2 -> a3 -> b1) -> b1
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
stated32 :: (b1 -> b2 -> a1 -> a2 -> a3 -> (b1, b2)) -> (b1, b2)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
stated33 :: (b1 -> b2 -> b3 -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> (b1, b2, b3)
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)

stated11 ns i s1    = react1 s1 $ SY.stated11 (B.ignore11 ns) i s1
stated12 ns i s1    = react2 s1 $ SY.stated12 (B.ignore12 ns) i s1
stated13 ns i s1    = react3 s1 $ SY.stated13 (B.ignore13 ns) i s1
stated21 ns i s1    = react1 s1 . SY.stated21 (B.ignore21 ns) i s1
stated22 ns i s1    = react2 s1 . SY.stated22 (B.ignore22 ns) i s1
stated23 ns i s1    = react3 s1 . SY.stated23 (B.ignore23 ns) i s1
stated31 ns i s1 s2 = react1 s1 . SY.stated31 (B.ignore31 ns) i s1 s2
stated32 ns i s1 s2 = react2 s1 . SY.stated32 (B.ignore32 ns) i s1 s2
stated33 ns i s1 s2 = react3 s1 . SY.stated33 (B.ignore33 ns) i s1 s2


------- MOORE -------

-- | @moore@ processes model Moore state machines. It implements the @moore@ patterns
-- (see 'ForSyDe.Atom.MoC.moore22'), and operates on absent-extended values (see
-- 'stated22').
--
-- Constructors: @moore[1-3][1-3]@.
--
-- >>> let s1 = signal [Prst 1,Prst 2,Abst,Abst,Prst 3]  
-- >>> moore11 (+) (+1) 1 s1
-- {2,3,⟂,⟂,5}
moore22 :: (st -> a1 -> a2 -> st)
           -- ^ next state function
           -> (st -> (b1, b2))
           -- ^ output decoder
           -> st
           -- ^ initial state
           -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2)
moore11 :: (st -> a1 -> st) -> (st -> b1) -> st
        -> Signal a1 -> Signal b1
moore12 :: (st -> a1 -> st) -> (st -> (b1, b2)) -> st
        -> Signal a1 -> (Signal b1, Signal b2)
moore13 :: (st -> a1 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
moore21 :: (st -> a1 -> a2 -> st) -> (st -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal b1
moore23 :: (st -> a1 -> a2 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
moore31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
moore32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
moore33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)

moore11 ns od i s1    = react1 s1 $ SY.moore11 (B.ignore11 ns) od i s1
moore12 ns od i s1    = react2 s1 $ SY.moore12 (B.ignore11 ns) od i s1
moore13 ns od i s1    = react3 s1 $ SY.moore13 (B.ignore11 ns) od i s1
moore21 ns od i s1    = react1 s1 . SY.moore21 (B.ignore21 ns) od i s1
moore22 ns od i s1    = react2 s1 . SY.moore22 (B.ignore21 ns) od i s1
moore23 ns od i s1    = react3 s1 . SY.moore23 (B.ignore21 ns) od i s1
moore31 ns od i s1 s2 = react1 s1 . SY.moore31 (B.ignore31 ns) od i s1 s2
moore32 ns od i s1 s2 = react2 s1 . SY.moore32 (B.ignore31 ns) od i s1 s2
moore33 ns od i s1 s2 = react3 s1 . SY.moore33 (B.ignore31 ns) od i s1 s2


------- MEALY -------

-- | @mealy@ processes model Mealy state machines. It implements the @mealy@ pattern
-- (see 'ForSyDe.Atom.MoC.mealy22'), and operates on absent-extended values (see
-- 'stated22').
--
-- Constructors: @mealy[1-3][1-3]@.
--
-- >>> let s1 = signal [Prst 1,Prst 2,Abst,Abst,Prst 3]  
-- >>> mealy11 (+) (+) 1 s1
-- {2,4,⟂,⟂,7}
mealy22 :: (st -> a1 -> a2 -> st)
           -- ^ next state function
           -> (st -> a1 -> a2 -> (b1, b2))
           -- ^ outpt decoder
           -> st
           -- ^ initial state
           -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2)
mealy11 :: (st -> a1 -> st) -> (st -> a1 -> b1) -> st
        -> Signal a1 -> Signal b1
mealy12 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2)) -> st
        -> Signal a1 -> (Signal b1, Signal b2)
mealy13 :: (st -> a1 -> st) -> (st -> a1 -> (b1, b2, b3)) -> st
        -> Signal a1 -> (Signal b1, Signal b2, Signal b3)
mealy21 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal b1
mealy23 :: (st -> a1 -> a2 -> st) -> (st -> a1 -> a2 -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> (Signal b1, Signal b2, Signal b3)
mealy31 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> b1) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> Signal b1
mealy32 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2)
mealy33 :: (st -> a1 -> a2 -> a3 -> st) -> (st -> a1 -> a2 -> a3 -> (b1, b2, b3)) -> st
        -> Signal a1 -> Signal a2 -> Signal a3 -> (Signal b1, Signal b2, Signal b3)

mealy11 ns od i s1       = comb21 od (stated11 ns i s1) s1
mealy12 ns od i s1       = comb22 od (stated11 ns i s1) s1
mealy13 ns od i s1       = comb23 od (stated11 ns i s1) s1
mealy21 ns od i s1 s2    = comb31 od (stated21 ns i s1 s2) s1 s2
mealy22 ns od i s1 s2    = comb32 od (stated21 ns i s1 s2) s1 s2
mealy23 ns od i s1 s2    = comb33 od (stated21 ns i s1 s2) s1 s2
mealy31 ns od i s1 s2 s3 = comb41 od (stated31 ns i s1 s2 s3) s1 s2 s3
mealy32 ns od i s1 s2 s3 = comb42 od (stated31 ns i s1 s2 s3) s1 s2 s3
mealy33 ns od i s1 s2 s3 = comb43 od (stated31 ns i s1 s2 s3) s1 s2 s3


---------------- INTERFACES -------------------
