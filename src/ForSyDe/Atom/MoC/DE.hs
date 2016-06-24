{-# LANGUAGE PostfixOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Atom.MoC.DE
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2016
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
 
-----------------------------------------------------------------------------

module ForSyDe.Atom.MoC.DE (
  module ForSyDe.Atom.MoC.DE.Core,
  module ForSyDe.Atom.MoC.DE.Lib
  ) where

import ForSyDe.Atom.MoC.DE.Core
import ForSyDe.Atom.MoC.DE.Lib


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
