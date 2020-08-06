 {-# OPTIONS_HADDOCK hide, show-extensions #-}

module ForSyDe.Atom.MoC.DE.Hybrid where

import           ForSyDe.Atom.MoC
import qualified ForSyDe.Atom.MoC.DE.Core      as DE (Signal)
import qualified ForSyDe.Atom.MoC.DE.Interface as DE
import qualified ForSyDe.Atom.MoC.SY.Core      as SY (Signal)
import qualified ForSyDe.Atom.MoC.SY.Interface as SY
import           ForSyDe.Atom.Utility.Tuple

import qualified ForSyDe.Atom.MoC.SY.Lib

import Prelude hiding ((<>))

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.SY.Lib as SY
-- >>> import ForSyDe.Atom.MoC.DE.Core (readSignal, signal)

------- HYBRID PROCESSES -------

-- | Embeds a 'ForSyDe.Atom.MoC.SY.SY' process inside a
-- 'ForSyDe.Atom.MoC.DE.DE' environment. Internally, it synchronizes
-- the input signals, translates them to SY, feeds them to a SY
-- process and translates the result back to DE using the same input
-- tags. Seen from outside, this process behaves like a DE process
-- with "instantaneous response", even for feedback loops.
--
-- Constructors: @embedSY[1-4][1-4]@.
--
-- For the following example, see the difference between its output
-- and the one of 'ForSyDe.Atom.MoC.DE.stated22'
--
-- >>> let s = readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: DE.Signal Int
-- >>> embedSY11 (SY.stated11 (+) 1) s
-- {1@0s,2@2s,4@6s,7@8s,11@9s}
--
-- <<fig/moc-de-pattern-embedsy.png>>
embedSY22 :: (SY.Signal a1 -> SY.Signal a2
              -> (SY.Signal b1, SY.Signal b2))
          -- ^ 'ForSyDe.Atom.MoC.SY.SY' process
          -> DE.Signal a1 -- ^ first input DE signal
          -> DE.Signal a2 -- ^ second input DE signal 
          -> (DE.Signal b1, DE.Signal b2)
          -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals

embedSY11 syproc de1 = let (ts, sy1) = DE.toSY1 de1
                       in  SY.toDE ts $     syproc sy1 
embedSY12 syproc de1 = let (ts, sy1) = DE.toSY1 de1
                       in  SY.toDE2 ts ><   syproc sy1
embedSY13 syproc de1 = let (ts, sy1) = DE.toSY1 de1
                       in  SY.toDE3 ts ><<  syproc sy1
embedSY14 syproc de1 = let (ts, sy1) = DE.toSY1 de1
                       in  SY.toDE4 ts ><<< syproc sy1

embedSY21 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE ts $     syproc sy1 sy2
embedSY22 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE2 ts ><   syproc sy1 sy2
embedSY23 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE3 ts ><<  syproc sy1 sy2
embedSY24 syproc de1 de2 = let (ts, sy1, sy2) = DE.toSY2 de1 de2
                           in  SY.toDE4 ts ><<< syproc sy1 sy2

embedSY31 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE ts $     syproc sy1 sy2 sy3
embedSY32 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE2 ts ><   syproc sy1 sy2 sy3
embedSY33 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE3 ts ><<  syproc sy1 sy2 sy3
embedSY34 syproc de1 de2 de3 = let (ts, sy1, sy2, sy3) = DE.toSY3 de1 de2 de3
                               in  SY.toDE4 ts ><<< syproc sy1 sy2 sy3

embedSY41 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE ts $     syproc sy1 sy2 sy3 sy4
embedSY42 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE2 ts ><   syproc sy1 sy2 sy3 sy4
embedSY43 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE3 ts ><<  syproc sy1 sy2 sy3 sy4
embedSY44 syproc de1 de2 de3 de4 = let (ts, sy1, sy2, sy3, sy4) = DE.toSY4 de1 de2 de3 de4
                                   in  SY.toDE4 ts ><<< syproc sy1 sy2 sy3 sy4
