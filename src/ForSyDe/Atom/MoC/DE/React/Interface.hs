{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_HADDOCK hide #-}

module ForSyDe.Atom.MoC.DE.React.Interface where

import           ForSyDe.Atom.MoC.Stream (Stream(..))
import           ForSyDe.Atom.Utility.Tuple

import qualified ForSyDe.Atom.ExB               as AE
import qualified ForSyDe.Atom.ExB.Absent        as AE
import qualified ForSyDe.Atom.MoC.DE            as DE
import qualified ForSyDe.Atom.MoC.DE.React.Core as RE
import qualified ForSyDe.Atom.MoC.DE.React.Lib  as RE
import qualified ForSyDe.Atom.MoC.SY            as SY
import qualified ForSyDe.Atom.MoC.SY.Clocked    as SYC

------- DOCTEST SETUP -------

-- $setup
-- >>> import ForSyDe.Atom.MoC.SY.Lib as SY
-- >>> import ForSyDe.Atom.MoC.DE.Core (readSignal, signal)

------------------------------

-- | Synchronizes a (set of) 'ForSyDe.Atom.MoC.RE.RE' signal(s) an
-- strips off their explicit tags, outputting the equivalent
-- 'ForSyDe.Atom.MoC.SY.SY' signal(s), tupled with an SY signal
-- carrying the timestamps for the synchronization points.
--
-- Constructors: @toSY[1-4]@
--
-- >>> let s1 = DE.infinite 1
-- >>> let s2 = DE.readSignal "{1@0, 2@2, 3@6, 4@8, 5@9}" :: RE.SignalBase t Int
-- >>> toSY2 s1 s2
-- ({0s,2s,6s,8s,9s},{1,1,1,1,1},{1,2,3,4,5})
--
-- <<fig/moc-de-tosy.png>>
toSYC2 :: (Num t, Ord t)
       => RE.SignalBase t a             -- ^ first input DE signal
       -> RE.SignalBase t b             -- ^ second input DE signal
       -> (SY.Signal t, SYC.Signal a, SYC.Signal b)
       -- ^ signal carrying timestamps tupled with the two output
       -- 'ForSyDe.Atom.MoC.SYC.SY' signals
toSYC1  :: (Num t, Ord t)
        => RE.SignalBase t a
        -> (SY.Signal t, SYC.Signal a)
toSYC3 :: (Num t, Ord t)
       => RE.SignalBase t a -> RE.SignalBase t b -> RE.SignalBase t c
       -> (SY.Signal t, SYC.Signal a, SYC.Signal b, SYC.Signal c)
toSYC4 :: (Num t, Ord t)
       => RE.SignalBase t a -> RE.SignalBase t b -> RE.SignalBase t c
       -> RE.SignalBase t d
       -> (SY.Signal t, SYC.Signal a, SYC.Signal b, SYC.Signal c, SYC.Signal d)

splitTs s = ((\(RE.RE t a) -> (SY.SY t, SY.SY a)) <$> s |<)
toSYC1 = splitTs . (fmap . fmap) AE.Prst
toSYC2 s1 s2
  = let (sy1,sy2) = (splitTs,splitTs) $$ RE.comb22 syncf s1 s2
    in  (fst,snd,snd) $$$ (sy1,sy1,sy2)
  where syncf []  []  = ([AE.Abst],   [AE.Abst]  )
        syncf [x] []  = ([AE.Prst x], [AE.Abst]  )
        syncf []  [x] = ([AE.Abst],   [AE.Prst x])
        syncf [x] [y] = ([AE.Prst x], [AE.Prst y])
toSYC3 s1 s2 s3
  = let (sy1,sy2,sy3) = (splitTs,splitTs,splitTs) $$$ RE.comb33 syncf s1 s2 s3
    in  (fst,snd,snd,snd) $$$$ (sy1,sy1,sy2,sy3)  
  where syncf x y z  = (lToA x, lToA y, lToA z)
        lToA []  = [AE.Abst]
        lToA [x] = [AE.Prst x]
toSYC4 s1 s2 s3 s4  
  = let (sy1,sy2,sy3,sy4) = (splitTs,splitTs,splitTs,splitTs)
                            $$$$ RE.comb44 syncf s1 s2 s3 s4
    in  (fst,snd,snd,snd,snd) $$$$$ (sy1,sy1,sy2,sy3,sy4) 
  where syncf x y z a = (lToA x, lToA y, lToA z, lToA a)
        lToA []  = [AE.Abst]
        lToA [x] = [AE.Prst x]

-- | Wraps explicit timestamps to a (set of) 'ForSyDe.Atom.MoC.SY.SY'
-- signal(s), rendering the equivalent synchronized
-- 'ForSyDe.Atom.MoC.DE.DE' signal(s).
--
-- __OBS:__ cleaning behavior!
--
-- Constructors: @fromSY@, @fromSY2@, @fromSY3@, @fromSY4@.
--
-- >>> let s1 = SY.signal [0,3,4,6,9]
-- >>> let s2 = SY.signal [1,2,3,4,5]
-- >>> fromSY s1 s2
-- {1@0s,2@3s,3@4s,4@6s,5@9s}
--
-- <<fig/moc-sy-tode.png>>
fromSYC2 :: (Num t, Ord t)
         => SY.Signal t
         -- ^ SY signal carrying 'ForSyDe.Atom.MoC.DE.DE' timestamps
         -> SYC.Signal a                -- ^ first input SYC signal
         -> SYC.Signal b                -- ^ second input SYC signal
         -> (RE.SignalBase t a, RE.SignalBase t b)
         -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals
fromSYC1 :: (Num t, Ord t)
         => SY.Signal t -> SYC.Signal a
         -> RE.SignalBase t a
fromSYC3 :: (Num t, Ord t)
         => SY.Signal t -> SYC.Signal a -> SYC.Signal b -> SYC.Signal c 
         -> (RE.SignalBase t a, RE.SignalBase t b, RE.SignalBase t c)
fromSYC4 :: (Num t, Ord t)
  => SY.Signal t -> SYC.Signal a -> SYC.Signal b -> SYC.Signal c -> SYC.Signal d 
  -> (RE.SignalBase t a, RE.SignalBase t b, RE.SignalBase t c, RE.SignalBase t d)
         
eventToDE (SY.SY t) (SY.SY a) = RE.RE t a
cleanAbst [AE.Prst x] = [x]
cleanAbst [AE.Abst]   = []

fromSYC1 ts s1          = RE.comb11 cleanAbst $ eventToDE <$> ts <*> s1
fromSYC2 ts s1 s2       = (fromSYC1 ts s1, fromSYC1 ts s2)
fromSYC3 ts s1 s2 s3    = (fromSYC1 ts s1, fromSYC1 ts s2, fromSYC1 ts s3)
fromSYC4 ts s1 s2 s3 s4 = (fromSYC1 ts s1, fromSYC1 ts s2, fromSYC1 ts s3,
                           fromSYC1 ts s4)

reToDE (RE.RE t a) = (DE.DE t a)
toDE1 s1 = fmap reToDE s1
toDE2 s1 s2 = (toDE1 s1, toDE1 s2)
toDE3 s1 s2 s3 = (toDE1 s1, toDE1 s2, toDE1 s3)
toDE4 s1 s2 s3 s4 = (toDE1 s1, toDE1 s2, toDE1 s3, toDE1 s4)

deToRE (DE.DE t a) = (RE.RE t a)
fromDE1 s1 = fmap deToRE s1
fromDE2 s1 s2 = (fromDE1 s1, fromDE1 s2)
fromDE3 s1 s2 s3 = (fromDE1 s1, fromDE1 s2, fromDE1 s3)
fromDE4 s1 s2 s3 s4 = (fromDE1 s1, fromDE1 s2, fromDE1 s3, fromDE1 s4)

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
embedSY22 :: (Num t, Ord t)
          => (SYC.Signal a1 -> SYC.Signal a2
              -> (SYC.Signal b1, SYC.Signal b2))
          -- ^ 'ForSyDe.Atom.MoC.SY.SY' process
          -> RE.SignalBase t a1 -- ^ first input DE signal
          -> RE.SignalBase t a2 -- ^ second input DE signal 
          -> (RE.SignalBase t b1, RE.SignalBase t b2)
          -- ^ two output 'ForSyDe.Atom.MoC.DE.DE' signals

embedSY11 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC1 ts $     syproc sy1 
embedSY12 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC2 ts ><   syproc sy1
embedSY13 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC3 ts ><<  syproc sy1
embedSY14 syproc de1
  = let (ts, sy1) = toSYC1 de1
    in  fromSYC4 ts ><<< syproc sy1

embedSY21 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC1 ts $     syproc sy1 sy2
embedSY22 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC2 ts ><   syproc sy1 sy2
embedSY23 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC3 ts ><<  syproc sy1 sy2
embedSY24 syproc de1 de2
  = let (ts, sy1, sy2) = toSYC2 de1 de2
    in  fromSYC4 ts ><<< syproc sy1 sy2

embedSY31 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC1 ts $     syproc sy1 sy2 sy3
embedSY32 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC2 ts ><   syproc sy1 sy2 sy3
embedSY33 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC3 ts ><<  syproc sy1 sy2 sy3
embedSY34 syproc de1 de2 de3
  = let (ts, sy1, sy2, sy3) = toSYC3 de1 de2 de3
    in  fromSYC4 ts ><<< syproc sy1 sy2 sy3

embedSY41 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC1 ts $     syproc sy1 sy2 sy3 sy4
embedSY42 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC2 ts ><   syproc sy1 sy2 sy3 sy4
embedSY43 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC3 ts ><<  syproc sy1 sy2 sy3 sy4
embedSY44 syproc de1 de2 de3 de4
  = let (ts, sy1, sy2, sy3, sy4) = toSYC4 de1 de2 de3 de4
    in  fromSYC4 ts ><<< syproc sy1 sy2 sy3 sy4


----------------------------------------------------

syncAndHold2 (i1,i2)
  = embedSY22 (\s1 s2 ->
                 (SYC.current i1 s1, SYC.current i2 s2))
syncAndHold3 (i1,i2,i3)
  = embedSY33 (\s1 s2 s3 ->
                 (SYC.current i1 s1, SYC.current i2 s2,
                   SYC.current i3 s3))
syncAndHold4 (i1,i2,i3,i4)
  = embedSY44 (\s1 s2 s3 s4 ->
                 (SYC.current i1 s1, SYC.current i2 s2,
                   SYC.current i3 s3, SYC.current i4 s4))

syncAndFill2 (i1,i2)
  = embedSY22 (\s1 s2 ->
                 (SYC.fill i1 s1, SYC.fill i2 s2))
syncAndFill3 (i1,i2,i3)
  = embedSY33 (\s1 s2 s3 ->
                 (SYC.fill i1 s1, SYC.fill i2 s2,
                   SYC.fill i3 s3))
syncAndFill4 (i1,i2,i3,i4)
  = embedSY44 (\s1 s2 s3 s4 ->
                 (SYC.fill i1 s1, SYC.fill i2 s2,
                   SYC.fill i3 s3, SYC.fill i4 s4))
