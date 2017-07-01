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

type TimeStamp = DiffTime

picosec  = picosecondsToDiffTime
nanosec  = picosecondsToDiffTime . (*1000)
microsec = picosecondsToDiffTime . (*1000000)
milisec  = picosecondsToDiffTime . (*1000000000)
sec      = secondsToDiffTime
minutes  = secondsToDiffTime . (*60) 
hours    = secondsToDiffTime . (*3600)

instance Read DiffTime where
  readsPrec p x = [ (realToFrac tstamp,r)
                  | (tstamp,r) <- readNum x ] 
    where readNum ::[Char] -> [(Double, String)]
          readNum = reads . takeWhile (/='s')
