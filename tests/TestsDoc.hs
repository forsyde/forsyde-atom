module Main where

import Test.DocTest

main = doctest
  [ "-isrc"
  , "src/ForSyDe/Atom/ExB/Absent.hs"
  , "src/ForSyDe/Atom/MoC/SY/Core.hs"
  , "src/ForSyDe/Atom/MoC/SY/Lib.hs"
  ]
