-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.TimeStamp
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements a timestamp data type, based on
-- "Data.Time.Clock". 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.TimeStamp where

import Data.Time.Clock ( DiffTime
                       , secondsToDiffTime
                       , picosecondsToDiffTime
                       )

-- | Alias for the type representing discrete time. It is inherently
-- quantizable, the quantum being a picosecond ( \(10^{-12}\)
-- seconds), thus it can be considered order-isomorphic with a set of
-- integers, i.e. between any two timestamps there is a finite number
-- of timestamps. Moreover, a timestamp can be easily translated into
-- a rational number representing fractions of a second, so the
-- conversion between timestamps (discrete time) and rationals
-- (analog/continuous time) is straightforward.
--
-- This type is used in the explicit tags of the
-- 'ForSyDe.Atom.MoC.DE.DE' MoC (and subsequently the discrete event
-- evaluation engine for simulating the 'ForSyDe.Atom.MoC.CT.CT' MoC).
type TimeStamp = DiffTime

-- | Specifies a timestamp in terms of picoseconds.
picosec  :: Integer -> TimeStamp
picosec  = picosecondsToDiffTime

-- | Specifies a timestamp in terms of nanoseconds.
nanosec  :: Integer -> TimeStamp
nanosec  = picosecondsToDiffTime . (*1000)

-- | Specifies a timestamp in terms of microseconds.
microsec :: Integer -> TimeStamp
microsec = picosecondsToDiffTime . (*1000000)

-- | Specifies a timestamp in terms of miliseconds.
milisec  :: Integer -> TimeStamp
milisec  = picosecondsToDiffTime . (*1000000000)

-- | Specifies a timestamp in terms of seconds.
sec      :: Integer -> TimeStamp
sec      = secondsToDiffTime

-- | Specifies a timestamp in terms of minutes.
minutes  :: Integer -> TimeStamp
minutes  = secondsToDiffTime . (*60) 

-- | Specifies a timestamp in terms of hours.
hours    :: Integer -> TimeStamp
hours    = secondsToDiffTime . (*3600)

-- | Converts a timestamp to a rational number, used for describing
-- continuous time.
toTime   :: TimeStamp -> Rational
toTime   = toRational

-- | 'TimeStamp' representation of the number &#960;. Converted from
-- the "Prelude" equivalent, which is 'Floating'.
pi :: TimeStamp 
pi  = realToFrac Prelude.pi


-- | reads @[n]s@ as a fraction of a second, where @[n]@ is a floating
-- point number.
instance Read DiffTime where
  readsPrec p x = [ (realToFrac tstamp,r)
                  | (tstamp,r) <- readNum x ] 
    where readNum ::[Char] -> [(Double, String)]
          readNum = reads . takeWhile (/='s')
