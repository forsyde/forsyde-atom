{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.DE.Lib
-- Copyright   :  (c) George Ungureanu, KTH/ICT/E 2015-2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a set of helpers for properly instantiating
-- process network patterns as process constructors.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Atom.MoC.DE.React.Lib where

import ForSyDe.Atom.MoC (
  MoC(..), (-&>-), (-*<), (-*<<), (-*<<<),
  ctxt11, ctxt21, ctxt31, ctxt41, ctxt51, ctxt61, ctxt71, ctxt81, 
  ctxt12, ctxt22, ctxt32, ctxt42, ctxt52, ctxt62, ctxt72, ctxt82, 
  ctxt13, ctxt23, ctxt33, ctxt43, ctxt53, ctxt63, ctxt73, ctxt83, 
  ctxt14, ctxt24, ctxt34, ctxt44, ctxt54, ctxt64, ctxt74, ctxt84,
  ) 
import qualified ForSyDe.Atom.MoC as MoC
import ForSyDe.Atom.MoC.DE.React.Core
import ForSyDe.Atom.Utility.Tuple

import qualified ForSyDe.Atom.MoC.SY   as SY 
import qualified ForSyDe.Atom.MoC.SY.Clocked  as SYC 

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.Stream (takeS)
-- >>> import ForSyDe.Atom.Utility.Plot

------- DELAY -------

-- | The @delay@ process "delays" a signal with one
-- event. Instantiates the 'ForSyDe.Atom.MoC.delay' pattern defined in
-- "ForSyDe.Atom.MoC".
--
-- >>> let s = readSignalBase t "{1@0, 2@2, 3@6, 4@8, 5@9}" :: SignalBase t Int
-- >>> delay 3 0 s
-- {0@0s,1@3s,2@5s,3@9s,4@11s,5@12s}
-- 
-- <<fig/moc-de-pattern-delay.png>>
delay :: (Num t, Ord t)
      => t          -- ^ time delay
      -> a          -- ^ initial value
      -> SignalBase t a   -- ^ input signal
      -> SignalBase t a   -- ^ output signal

delay t v = MoC.delay (unit (t, v))

-- | Similar to the previous, but this is the raw instantiation of the
-- 'ForSyDe.Atom.MoC.delay' pattern from "ForSyDe.Atom.MoC". It
-- "borrows" the first event from one signal and appends it at the
-- head of another signal.
--
-- >>> let s1 = readSignalBase t "{1@0, 2@2, 3@6, 4@8, 5@9}" :: SignalBase t Int
-- >>> let s2 = readSignalBase t "{3@0, 4@4, 5@5, 6@8, 7@9}" :: SignalBase t Int
-- >>> delay' s1 s2
-- {1@0s,3@2s,4@6s,5@7s,6@10s,7@11s}
--
-- <<fig/moc-de-pattern-delayp.png>>
delay' ::(Num t, Ord t)
       => SignalBase t a   -- ^ signal "borrowing" the initial event
       -> SignalBase t a   -- ^ input signal
       -> SignalBase t a   -- ^ output signal

delay' = MoC.delay

--------COMB --------

-- | @comb@ processes map combinatorial functions on signals and take
-- care of synchronization between input signals. It instantiates the
-- @comb@ pattern (see 'ForSyDe.Atom.MoC.comb22' defined in
-- "ForSyDe.Atom.MoC").
-- 
-- Constructors: @comb[1-4][1-4]@.
--
-- >>> let s1 = infinite 1
-- >>> let s2 = readSignalBase t "{1@0, 2@2, 3@6, 4@8, 5@9}" :: SignalBase t Int
-- >>> comb11 (+1) s2
-- {2@0s,3@2s,4@6s,5@8s,6@9s}
-- >>> comb22 (\a b-> (a+b,a-b)) s1 s2
-- ({2@0s,3@2s,4@6s,5@8s,6@9s},{0@0s,-1@2s,-2@6s,-3@8s,-4@9s})
--
-- <<fig/moc-de-pattern-comb.png>>
comb22 :: (Num t, Ord t)
       => ([a1] -> [a2] -> ([b1], [b2]))     -- ^ function on values
       -> SignalBase t a1                    -- ^ first input signal
       -> SignalBase t a2                    -- ^ second input signal
       -> (SignalBase t b1, SignalBase t b2) -- ^ two output signals
comb11 :: (Ord t, Num t)
       => ([a1] -> [b1])
       -> SignalBase t a1 -> SignalBase t b1                                
comb12 :: (Num t, Ord t)
       => ([a1] -> ([b1], [b2]))
       -> SignalBase t a1 -> (SignalBase t b1, SignalBase t b2)                          
comb13 :: (Num t, Ord t)
       => ([a1] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      
comb14 :: (Num t, Ord t)
       => ([a1] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)                  
comb21 :: (Ord t, Num t)
       => ([a1] -> [a2] -> [b1])
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t b1                          
comb23 :: (Num t, Ord t)
       => ([a1] -> [a2] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> SignalBase t a2 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                
comb24 :: (Num t, Ord t)
       => ([a1] -> [a2] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> SignalBase t a2 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)            
comb31 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [b1])
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t b1                    
comb32 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> ([b1], [b2]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> (SignalBase t b1, SignalBase t b2)              
comb33 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)          
comb34 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)     
comb41 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> [b1])
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> SignalBase t b1              
comb42 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> (SignalBase t b1, SignalBase t b2)        
comb43 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)    
comb44 :: (Num t, Ord t)
       => ([a1] -> [a2] -> [a3] -> [a4] -> ([b1], [b2], [b3], [b4]))
       -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 -> SignalBase t a4 -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)

comb11 f s1 =
  (detect s1 -? (\t -> ctxt11 (fromList1 t) () f)
    -*- s1 -*)
comb12 f s1 =
  (detect s1 -? (\t -> ctxt12 (fromList1 t) ((),()) f)
    -*- s1 -*<)
comb13 f s1 =
  (detect s1 -? (\t -> ctxt13 (fromList1 t) ((),(),()) f)
    -*- s1 -*<<)
comb14 f s1 =
  (detect s1 -? (\t -> ctxt14 (fromList1 t) ((),(),(),()) f)
    -*- s1 -*<<<)
comb21 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt21 (fromList2 t) () f)
    -*- s1 -*- s2 -*)
comb22 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt22 (fromList2 t) ((),()) f)
    -*- s1 -*- s2 -*<)
comb23 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt23 (fromList2 t) ((),(),()) f)
    -*- s1 -*- s2 -*<<)
comb24 f s1 s2 =
  (detect s1 -?- s2 -? (\t -> ctxt24 (fromList2 t) ((),(),(),()) f)
    -*- s1 -*- s2 -*<<<)
comb31 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt31 (fromList3 t) () f)
    -*- s1 -*- s2 -*- s3 -*)
comb32 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt32 (fromList3 t) ((),()) f)
    -*- s1 -*- s2 -*- s3 -*<)
comb33 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt33 (fromList3 t) ((),(),()) f)
    -*- s1 -*- s2 -*- s3 -*<<)
comb34 f s1 s2 s3 =
  (detect s1 -?- s2 -?- s3 -? (\t -> ctxt34 (fromList3 t) ((),(),(),()) f)
    -*- s1 -*- s2 -*- s3 -*<<<)
comb41 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt41 (fromList4 t) () f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*)
comb42 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt42 (fromList4 t) ((),()) f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*<)
comb43 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt43 (fromList4 t) ((),(),()) f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*<<)
comb44 f s1 s2 s3 s4 =
  (detect s1 -?- s2 -?- s3 -?- s4 -? (\t -> ctxt44 (fromList4 t) ((),(),(),()) f)
    -*- s1 -*- s2 -*- s3 -*- s4 -*<<<)

------- CONSTANT -------

-- | A signal generator which keeps a value constant. As compared with
-- the 'ForSyDe.Atom.MoC.SY.SY', it just constructs an infinite signal
-- with constant value (i.e. a signal with one event starting from
-- time 0).
--
-- Constructors: @constant[1-4]@.
--
-- >>> constant1 2
-- {2@0s}
--
-- <<fig/moc-de-pattern-constant.png>>
constant2 :: (Num t, Ord t)
    => (b1, b2)         -- ^ values to be repeated
          -> (SignalBase t b1, SignalBase t b2) -- ^ generated signals
constant1 :: (Num t, Ord t)
    => b1 -> SignalBase t b1                                
constant3 :: (Num t, Ord t)
    => (b1, b2, b3)
          -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)
constant4 :: (Num t, Ord t)
    => (b1, b2, b3, b4)
          -> (SignalBase t b1, SignalBase t b2, SignalBase t b3, SignalBase t b4)

constant1 = infinite
constant2 = ($$) (infinite,infinite)
constant3 = ($$$) (infinite,infinite,infinite)
constant4 = ($$$$) (infinite,infinite,infinite,infinite)

------- GENERATE -------

-- | A signal generator based on a function and a kernel value. It is
-- actually an instantiation of the @stated0X@ constructor (check
-- 'ForSyDe.Atom.MoC.stated22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @generate[1-4]@.
--
-- >>> let (s1,s2) = generate2 (\a b -> (a+1,b+2)) ((3,1),(1,2))
-- >>> takeS 5 s1
-- {1@0s,2@3s,2@4s,2@5s,3@6s}
-- >>> takeS 7 s2
-- {2@0s,4@1s,6@2s,8@3s,10@4s,12@5s,14@6s}
--
-- <<fig/moc-de-pattern-generate.png>>
generate2 :: (Num t, Ord t)
          =>  (b1 -> b2 -> (b1, b2))
          -- ^ function to generate next value
          -> ((t, b1), (t, b2))
          -- ^ kernel values tupled with their generation rate.
          -> (SignalBase t b1, SignalBase t b2) -- ^ generated signals
generate1 :: (Num t, Ord t)
          =>  (b1 -> b1) -> (t, b1)
          -> SignalBase t b1                                
generate3 :: (Num t, Ord t)
          =>  (b1 -> b2 -> b3 -> (b1, b2, b3))
          -> ((t, b1), (t, b2), (t, b3))
          -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      

generate1 = stated01
generate2 = stated02
generate3 = stated03


------- STATE -------

-- | @state@ is a state machine without an output decoder. It is an
-- instantiation of the @state@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.stated22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @stated[1-4][1-4]@.
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: Signal Int
-- >>> takeS 7 $ stated11 (+) (6,1) s
-- {1@0s,2@6s,3@8s,5@12s,7@14s,8@15s,10@18s}
--
-- <<fig/moc-de-pattern-state.png>>
stated22 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> [a2] -> (b1, b2))
         -- ^ next state function
         -> ((t, b1), (t, b2))
         -- ^ initial state values tupled with their initial delay
         -> SignalBase t a1
         -- ^ first input signal
         -> SignalBase t a2
         -- ^ second input signal
         -> (SignalBase t b1, SignalBase t b2) -- ^ output signals
stated11 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> b1)
         -> (t, b1)
         -> SignalBase t a1
         -> SignalBase t b1 
stated12 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> (b1, b2))
         -> ((t, b1), (t, b2))
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2) 
stated13 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> (b1, b2, b3))
         -> ((t, b1), (t, b2), (t, b3))
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 
stated21 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> b1)
         -> (t, b1)
         -> SignalBase t a1 -> SignalBase t a2
         -> SignalBase t b1 
stated31 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> [a3] -> b1)
         -> (t, b1)
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> SignalBase t b1 

stated01 ns i          = st 
  where st             = unit i -&>- comb11 (li101 ns) st 
stated11 ns i s1       = st
  where st             = unit i -&>- comb21 (li111 ns) st s1 
stated21 ns i s1 s2    = st
  where st             = unit i -&>- comb31 (li121 ns) st s1 s2
stated31 ns i s1 s2 s3 = st
  where st             = unit i -&>- comb41 (li131 ns) st s1 s2 s3
stated02 ns (i1,i2)
  = let (ns1,ns2) = comb22 (li202 ns) st1 st2
        (st1,st2) = (unit i1 -&>- ns1, unit i2 -&>- ns2)
    in  (st1,st2)
stated12 ns (i1,i2) s1
  = let (ns1,ns2) = comb32 (li212 ns) st1 st2 s1
        (st1,st2) = (unit i1 -&>- ns1, unit i2 -&>- ns2)
    in  (st1,st2)
stated22 ns (i1,i2) s1 s2
  = let (ns1,ns2) = comb42 (li222 ns) st1 st2 s1 s2
        (st1,st2) = (unit i1 -&>- ns1, unit i2 -&>- ns2)
    in  (st1,st2)
stated03 ns (i1,i2,i3)
  = let (ns1,ns2,ns3) = comb33 (li303 ns) st1 st2 st3
        (st1,st2,st3) = (unit i1 -&>- ns1, unit i2 -&>- ns2, unit i3 -&>- ns3)
    in  (st1,st2,st3)
stated13 ns (i1,i2,i3) s1
  = let (ns1,ns2,ns3) = comb43 (li313 ns) st1 st2 st3 s1
        (st1,st2,st3) = (unit i1 -&>- ns1, unit i2 -&>- ns2, unit i3 -&>- ns3)
    in  (st1,st2,st3)

------- STATED -------

-- | @state@ is a state machine without an output decoder. It is an
-- instantiation of the @state@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.state22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @state[1-4][1-4]@.
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: Signal Int  
-- >>> takeS 7 $ state11 (+) (6,1) s
-- {1@0s,2@6s,3@8s,5@12s,7@14s,8@15s,10@18s}
--
-- <<fig/moc-de-pattern-state.png>>
state22 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> [a2] -> (b1, b2))
         -- ^ next state function
         -> ((t, b1), (t, b2))
         -- ^ initial state values tupled with their initial delay
         -> SignalBase t a1
         -- ^ first input signal
         -> SignalBase t a2
         -- ^ second input signal
         -> (SignalBase t b1, SignalBase t b2) -- ^ output signals
state11 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> b1)
         -> (t, b1)
         -> SignalBase t a1
         -> SignalBase t b1 
state12 :: (Num t, Ord t)
         =>  (b1 -> b2 -> [a1] -> (b1, b2))
         -> ((t, b1), (t, b2))
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2) 
state13 :: (Num t, Ord t)
         =>  (b1 -> b2 -> b3 -> [a1] -> (b1, b2, b3))
         -> ((t, b1), (t, b2), (t, b3))
         -> SignalBase t a1
         -> (SignalBase t b1, SignalBase t b2, SignalBase t b3) 
state21 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> b1)
         -> (t, b1)
         -> SignalBase t a1 -> SignalBase t a2
         -> SignalBase t b1 
state31 :: (Num t, Ord t)
         =>  (b1 -> [a1] -> [a2] -> [a3] -> b1)
         -> (t, b1)
         -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
         -> SignalBase t b1 

state11 ns i s1 = comb21 (li111 ns) st s1 
  where st = unit i -&>- comb21 (li111 ns) st s1 
state21 ns i s1 s2 = comb31 (li121 ns) st s1 s2
  where st  = unit i -&>- comb31 (li121 ns) st s1 s2
state31 ns i s1 s2 s3 = comb41 (li131 ns) st s1 s2 s3
  where st = unit i -&>- comb41 (li131 ns) st s1 s2 s3
state12 ns (i1,i2) s1
  = let (ns1,ns2) = comb32 (li212 ns) st1 st2 s1
        (st1,st2) = (unit i1 -&>- ns1, unit i2 -&>- ns2)
    in  (ns1,ns2)
state22 ns (i1,i2) s1 s2
  = let (ns1,ns2) = comb42 (li222 ns) st1 st2 s1 s2
        (st1,st2) = (unit i1 -&>- ns1, unit i2 -&>- ns2)
    in  (ns1,ns2)
state13 ns (i1,i2,i3) s1
  = let (ns1,ns2,ns3) = comb43 (li313 ns) st1 st2 st3 s1
        (st1,st2,st3) = (unit i1 -&>- ns1, unit i2 -&>- ns2, unit i3 -&>- ns3)
    in  (ns1,ns2,ns3)

------- MOORE -------

-- | @moore@ processes model Moore state machines. It is an
-- instantiation of the @moore@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.moore22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @moore[1-4][1-4]@
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: Signal Int  
-- >>> takeS 7 $ moore11 (+) (+1) (6,1) s
-- {2@0s,3@6s,4@8s,6@12s,8@14s,9@15s,11@18s}
--
-- <<fig/moc-de-pattern-moore.png>>          
moore22 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -- ^ next state function
        -> (st -> ([b1], [b2]))
        -- ^ output decoder
        -> (t, st)
        -- ^ initial state: tag and value
        -> SignalBase t a1 -> SignalBase t a2 -> (SignalBase t b1, SignalBase t b2)
moore11 :: (Num t, Ord t)
        =>  (st -> [a1] -> st)
        -> (st -> [b1])
        -> (t, st)
        -> SignalBase t a1
        -> SignalBase t b1                                
moore12 :: (Num t, Ord t)
        =>  (st -> [a1] -> st)
        -> (st -> ([b1], [b2]))
        -> (t, st)
        -> SignalBase t a1
        -> (SignalBase t b1, SignalBase t b2)                          
moore13 :: (Num t, Ord t)
        =>  (st -> [a1] -> st)
        -> (st -> ([b1], [b2], [b3]))
        -> (t, st)
        -> SignalBase t a1
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      
moore21 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -> (st -> [b1])
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2
        -> SignalBase t b1                          
moore23 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -> (st -> ([b1], [b2], [b3]))
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                
moore31 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st)
        -> (st -> [b1])
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> SignalBase t b1                    
moore32 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st)
        -> (st -> ([b1], [b2]))
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> (SignalBase t b1, SignalBase t b2)              
moore33 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st)
        -> (st -> ([b1], [b2], [b3]))
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)          

moore11 ns od i s1          =             comb11 (li1 od) st
  where st                  = unit i -&>- comb21 (li111 ns) st s1
moore12 ns od i s1          =             comb12 (li1 od) st
  where st                  = unit i -&>- comb21 (li111 ns) st s1
moore13 ns od i s1          =             comb13 (li1 od) st
  where st                  = unit i -&>- comb21 (li111 ns) st s1
moore21 ns od i s1 s2       =             comb11 (li1 od) st
  where st                  = unit i -&>- comb31 (li121 ns) st s1 s2
moore22 ns od i s1 s2       =             comb12 (li1 od) st
  where st                  = unit i -&>- comb31 (li121 ns) st s1 s2
moore23 ns od i s1 s2       =             comb13 (li1 od) st
  where st                  = unit i -&>- comb31 (li121 ns) st s1 s2
moore31 ns od i s1 s2 s3    =             comb11 (li1 od) st
  where st                  = unit i -&>- comb41 (li131 ns) st s1 s2 s3
moore32 ns od i s1 s2 s3    =             comb12 (li1 od) st
  where st                  = unit i -&>- comb41 (li131 ns) st s1 s2 s3
moore33 ns od i s1 s2 s3    =             comb13 (li1 od) st
  where st                  = unit i -&>- comb41 (li131 ns) st s1 s2 s3

------- MEALY -------

-- | @mealy@ processes model Mealy state machines. It is an
-- instantiation of the @mealy@ MoC constructor (see
-- 'ForSyDe.Atom.MoC.mealy22' defined in "ForSyDe.Atom.MoC").
--
-- Constructors: @mealy[1-4][1-4]@
--
-- >>> let s = readSignalBase t "{1@0, 2@2, 3@6, 4@8, 5@9}" :: SignalBase t Int  
-- >>> takeS 7 $ mealy11 (+) (-) (6,1) s
-- {0@0s,-1@2s,-1@6s,-1@8s,-2@9s,0@12s,2@14s}
--
-- <<fig/moc-de-pattern-mealy.png>>
mealy22 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st)
        -- ^ next state function
        -> (st -> [a1] -> [a2] -> ([b1], [b2]))
        -- ^ outpt decoder
        -> (t, st)
        -- ^ initial state: tag and value
        -> SignalBase t a1 -> SignalBase t a2
        -> (SignalBase t b1, SignalBase t b2)
mealy11 :: (Num t, Ord t)
        =>  (st -> [a1] -> st) 
        -> (st -> [a1] -> [b1]) 
        -> (t, st)
        -> SignalBase t a1
        -> SignalBase t b1                                
mealy12 :: (Num t, Ord t)
        =>  (st -> [a1] -> st) 
        -> (st -> [a1] -> ([b1], [b2])) 
        -> (t, st)
        -> SignalBase t a1 
        -> (SignalBase t b1, SignalBase t b2)                          
mealy13 :: (Num t, Ord t)
        =>  (st -> [a1] -> st) 
        -> (st -> [a1] -> ([b1], [b2], [b3])) 
        -> (t, st)
        -> SignalBase t a1 
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                      
mealy21 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st) 
        -> (st -> [a1] -> [a2] -> [b1]) 
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2
        -> SignalBase t b1                          
mealy23 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> st) 
        -> (st -> [a1] -> [a2] -> ([b1], [b2], [b3])) 
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2 
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)                
mealy31 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st) 
        -> (st -> [a1] -> [a2] -> [a3] -> [b1]) 
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3
        -> SignalBase t b1  
mealy32 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st) 
        -> (st -> [a1] -> [a2] -> [a3] -> ([b1], [b2])) 
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 
        -> (SignalBase t b1, SignalBase t b2)              
mealy33 :: (Num t, Ord t)
        =>  (st -> [a1] -> [a2] -> [a3] -> st) 
        -> (st -> [a1] -> [a2] -> [a3] -> ([b1], [b2], [b3])) 
        -> (t, st)
        -> SignalBase t a1 -> SignalBase t a2 -> SignalBase t a3 
        -> (SignalBase t b1, SignalBase t b2, SignalBase t b3)          
mealy11 ns od i s1          =             comb21 (li1 od) st s1
  where st                  = unit i -&>- comb21 (li111 ns) st s1
mealy12 ns od i s1          =             comb22 (li1 od) st s1
  where st                  = unit i -&>- comb21 (li111 ns) st s1
mealy13 ns od i s1          =             comb23 (li1 od) st s1
  where st                  = unit i -&>- comb21 (li111 ns) st s1
mealy21 ns od i s1 s2       =             comb31 (li1 od) st s1 s2
  where st                  = unit i -&>- comb31 (li121 ns) st s1 s2
mealy22 ns od i s1 s2       =             comb32 (li1 od) st s1 s2
  where st                  = unit i -&>- comb31 (li121 ns) st s1 s2
mealy23 ns od i s1 s2       =             comb33 (li1 od) st s1 s2
  where st                  = unit i -&>- comb31 (li121 ns) st s1 s2
mealy31 ns od i s1 s2 s3    =             comb41 (li1 od) st s1 s2 s3
  where st                  = unit i -&>- comb41 (li131 ns) st s1 s2 s3
mealy32 ns od i s1 s2 s3    =             comb42 (li1 od) st s1 s2 s3
  where st                  = unit i -&>- comb41 (li131 ns) st s1 s2 s3
mealy33 ns od i s1 s2 s3    =             comb43 (li1 od) st s1 s2 s3
  where st                  = unit i -&>- comb41 (li131 ns) st s1 s2 s3


------------------------------
