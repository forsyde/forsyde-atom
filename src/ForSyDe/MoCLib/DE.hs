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

module ForSyDe.MoCLib.DE where

import ForSyDe.Core
import ForSyDe.Core.MoC
import ForSyDe.Core.Signal
import qualified ForSyDe.MoCLib.SY as SY

-----------------------------------------------------------------------------
data DE a = DE Int a deriving (Show)

instance MoC DE where
  _ -$- NullS = NullS
  f -$- (x:-xs) = fmap f x :- f -$- xs
  ---------------------
  NullS -*- _ = NullS
  _ -*- NullS = NullS
  (DE tf f :- fs) -*- (DE tx x :- xs)
    | tf == 0 && tx == 0 = DE tf (f x) :- comb (DE tf f) (DE tx x) fs xs
    | otherwise          = error "Signals must start from tag 0"
    where comb (DE ptf pf) (DE ptx px) s1@(DE tf f :- fs) s2@(DE tx x :- xs)
            | tf == tx = DE tf ( f  x) :- comb (DE  tf  f) (DE  tx  x) fs xs
            | tf <  tx = DE tf ( f px) :- comb (DE  tf  f) (DE ptx px) fs s2
            | tf >  tx = DE tx (pf  x) :- comb (DE ptf pf) (DE  tx  x) s1 xs
          comb _ (DE ptx px) (DE tf f :- fs) NullS
            = DE tf (f px) :- comb (DE tf f) (DE ptx px) fs NullS
          comb (DE ptf pf) _ NullS (DE tx x :- xs)
            = DE tx (pf x) :- comb (DE ptf pf) (DE tx x) NullS xs
          comb _ _ NullS NullS = NullS
  ---------------------
  (DE _ v) ->- xs = DE 0 v :- xs
  ---------------------
  (DE t _) -&- xs = (\(DE t1 v) -> DE (t1 + t) v) <$> xs


-- instance Show a => Show (DE a) where
--   showsPrec _ (DE t x) = (++) (show x)

-- instance Read a => Read (DE a) where
--   readsPrec _ s  = [(DE t x, r) | (x, r) <- reads s]

instance Functor DE where
  fmap f (DE t a) = DE t (f a)

-- instance Applicative DE where
--   pure a = DE a   
--   (DE t a) <*> (DE u b) = SY (a b)


signalDE l = signal $ (\(t,v) -> DE t (value v)) <$> l
valueDE (t,v) = DE t (value v)



-- -----------------------------------------------------------------------------

-- phi :: Signal (DE Int) -> Signal (DE a) -> Signal (DE a)
-- phi NullS xs = xs
-- phi _ NullS  = NullS
-- phi s1@(DE tp psk:-ss) (DE tx x:-xs)
--   | tp <= tx = DE (tx + psk) x :- ss `phi` (phaseshift psk <$> xs)
--   | tp >  tx = DE tx x         :- s1 `phi` xs
--   where phaseshift ppsk (DE t g) = DE (t + ppsk) g



-- infixl 5 -&-
-- (-&-) :: (Int -> Int) -> Signal (DE a) -> Signal (DE a)
-- f -&- xs = timef 0 f xs
--   where timef pt f (DE t x:-xs) | pt <= f t = DE (f t) x :- timef (f t) f xs
--                                    | otherwise = error "Time causality broken!"
--         timef _  _ NullS = NullS


-- wait :: (Int -> Int) -> Signal (DE a) -> Signal (DE a)
-- wait f xs = timef 0 f xs
--   where timef pt f (DE t x:-xs) | f pt <= f t  = DE (f  t) x :- timef (f  t) f xs
--                                    | otherwise    = DE (f pt) x :- timef (f pt) f xs
--         timef _  _ NullS = NullS

-- --timeMachine :: (Int -> Int -> Int) -> (Int -> Int) -> Int -> Signal (DE a) -> Signal (DE a)
-- timeMachine ns od s1 = syr2de newtags vals
--   where (tags, vals) = de2syr s1
--         newtags = tailS $ SY.moore11 ns od 0 tags


-- -- infixl 3 #
-- -- (#) :: Signal (DE Bool) -> Signal (DE (a -> a)) 
-- -- (#) s = fmap (\(DE t x) -> if x == D True then DE t (D id) else DE t U) s

-- -----------------------------------------------------------------------------

-- ----------------------
-- ---- CONSTRUCTORS ----
-- ----------------------

-- comb11 f s1          = (psi11 f -$- s1)
-- comb12 f s1          = (psi12 f -$- s1 -<)
-- comb13 f s1          = (psi13 f -$- s1 -<<)
-- comb14 f s1          = (psi14 f -$- s1 -<<<)
-- comb21 f s1 s2       = (psi21 f -$- s1 -*- s2)
-- comb22 f s1 s2       = (psi22 f -$- s1 -*- s2 -<)
-- comb23 f s1 s2       = (psi23 f -$- s1 -*- s2 -<<)
-- comb24 f s1 s2       = (psi24 f -$- s1 -*- s2 -<<<)
-- comb31 f s1 s2 s3    = (psi31 f -$- s1 -*- s2 -*- s3)
-- comb32 f s1 s2 s3    = (psi32 f -$- s1 -*- s2 -*- s3 -<)
-- comb33 f s1 s2 s3    = (psi33 f -$- s1 -*- s2 -*- s3 -<<)
-- comb34 f s1 s2 s3    = (psi34 f -$- s1 -*- s2 -*- s3 -<<<)
-- comb41 f s1 s2 s3 s4 = (psi41 f -$- s1 -*- s2 -*- s3 -*- s4)
-- comb42 f s1 s2 s3 s4 = (psi42 f -$- s1 -*- s2 -*- s3 -*- s4 -<)
-- comb43 f s1 s2 s3 s4 = (psi43 f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
-- comb44 f s1 s2 s3 s4 = (psi44 f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)

-- delay s1 xs = s1 ->- xs

-- moore11 ns od i s1          = comb11 od st
--   where st                  = i ->- comb21 ns st s1
-- moore12 ns od i s1          = comb12 od st
--   where st                  = i ->- comb21 ns st s1
-- moore13 ns od i s1          = comb13 od st
--   where st                  = i ->- comb21 ns st s1
-- moore14 ns od i s1          = comb14 od st
--   where st                  = i ->- comb21 ns st s1
-- moore21 ns od i s1 s2       = comb11 od st
--   where st                  = i ->- comb31 ns st s1 s2
-- moore22 ns od i s1 s2       = comb12 od st
--   where st                  = i ->- comb31 ns st s1 s2
-- moore23 ns od i s1 s2       = comb13 od st
--   where st                  = i ->- comb31 ns st s1 s2
-- moore24 ns od i s1 s2       = comb14 od st
--   where st                  = i ->- comb31 ns st s1 s2
-- moore31 ns od i s1 s2 s3    = comb11 od st
--   where st                  = i ->- comb41 ns st s1 s2 s3
-- moore32 ns od i s1 s2 s3    = comb12 od st
--   where st                  = i ->- comb41 ns st s1 s2 s3
-- moore33 ns od i s1 s2 s3    = comb13 od st
--   where st                  = i ->- comb41 ns st s1 s2 s3
-- moore34 ns od i s1 s2 s3    = comb14 od st
--   where st                  = i ->- comb41 ns st s1 s2 s3
-- moore41 ns od i s1 s2 s3 s4 = comb11 od st
--   where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
-- moore42 ns od i s1 s2 s3 s4 = comb12 od st
--   where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
-- moore43 ns od i s1 s2 s3 s4 = comb13 od st
--   where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
-- moore44 ns od i s1 s2 s3 s4 = comb14 od st
--   where st                  = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

-- mealy11 ns od i s1          = comb21 od st s1
--   where st            = i ->- comb21 ns st s1
-- mealy12 ns od i s1          = comb22 od st s1
--   where st            = i ->- comb21 ns st s1
-- mealy13 ns od i s1          = comb23 od st s1
--   where st            = i ->- comb21 ns st s1
-- mealy14 ns od i s1          = comb24 od st s1
--   where st            = i ->- comb21 ns st s1
-- mealy21 ns od i s1 s2       = comb31 od st s1 s2
--   where st            = i ->- comb31 ns st s1 s2
-- mealy22 ns od i s1 s2       = comb32 od st s1 s2
--   where st            = i ->- comb31 ns st s1 s2
-- mealy23 ns od i s1 s2       = comb33 od st s1 s2
--   where st            = i ->- comb31 ns st s1 s2
-- mealy24 ns od i s1 s2       = comb34 od st s1 s2
--   where st            = i ->- comb31 ns st s1 s2
-- mealy31 ns od i s1 s2 s3    = comb41 od st s1 s2 s3
--   where st            = i ->- comb41 ns st s1 s2 s3
-- mealy32 ns od i s1 s2 s3    = comb42 od st s1 s2 s3
--   where st            = i ->- comb41 ns st s1 s2 s3
-- mealy33 ns od i s1 s2 s3    = comb43 od st s1 s2 s3
--   where st            = i ->- comb41 ns st s1 s2 s3
-- mealy34 ns od i s1 s2 s3    = comb44 od st s1 s2 s3
--   where st            = i ->- comb41 ns st s1 s2 s3
-- mealy41 ns od i s1 s2 s3 s4 = (psi51 od -$- st -*- s1 -*- s2 -*- s3 -*- s4)
--   where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
-- mealy42 ns od i s1 s2 s3 s4 = (psi52 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<)
--   where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
-- mealy43 ns od i s1 s2 s3 s4 = (psi53 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<)
--   where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
-- mealy44 ns od i s1 s2 s3 s4 = (psi54 od -$- st -*- s1 -*- s2 -*- s3 -*- s4 -<<<)
--   where st             = i ->- psi51 ns -$- st -*- s1 -*- s2 -*- s3 -*- s4

-- de2syr s1          = funzip2 $ (\(DE t a) -> (t,a)) <$> s1
-- de2syr2 s1 s2       = (tag, funzip2 sigs)
--   where (tag, sigs) = funzip2 $ (\(DE t a) -> (t,a)) <$> ((,)   -$- s1 -*- s2)
-- -- de2syr3 s1 s2 s3    = (tag, funzip3 sigs)
-- --   where (tag, sigs) = funzip2 $ (\(DE t a) -> (t,a)) <$> ((,,)  -$- s1 -*- s2 -*- s3)
-- -- de2syr4 s1 s2 s3 s4 = (tag, funzip4 sigs)
-- --   where (tag, sigs) = funzip2 $ (\(DE t a) -> (t,a)) <$> ((,,,) -$- s1 -*- s2 -*- s3 -*- s4)

-- syr2de tags s1          = (\t a -> (DE t a)) <$> tags <*> s1
-- syr2de2 tags s1 s2       = funzip2 $ (\t a b -> (DE t a, DE t b)) <$> tags <*> s1 <*> s2
-- -- syr2de3 tags s1 s2 s3    = funzip3 $ (\t a b c -> (DE t a, DE t b, DE t c))
-- --                            <$> tags <*> s1 <*> s2 <*> s2
-- -- syr2de4 tags s1 s2 s3 s4 = funzip4 $ (\t a b c d -> (DE t a, DE t b, DE t c, DE t d))
-- --                            <$> tags <*> s1 <*> s2 <*> s3 <*> s4


-- filt sels s = (#) -$- sels -*- s

-- store buff s1 = (>¤) -$- buff -*- s1


-- -------------------------------------------------------------------


                 
-- controller 0 0 = (1, 1)
-- controller 0 1 = (1, 0)
-- controller 1 0 = (0, 1)
-- controller 1 1 = (0, 1)


-- r = signal [DE 0 (D 1), DE 3 (D 0), DE 5 (D 1)]
-- l = signal [DE 0 (D 1), DE 4 (D 0)] 
