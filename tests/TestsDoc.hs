module Main where

import Test.DocTest

main = doctest
  [ "-isrc"
  , "src/ForSyDe/Atom/ExB/Absent.hs"
  , "src/ForSyDe/Atom/MoC/SY/Core.hs"
  , "src/ForSyDe/Atom/MoC/SY/Lib.hs"
  , "src/ForSyDe/Atom/MoC/SY/Interface.hs"
  , "src/ForSyDe/Atom/MoC/SY/Clocked.hs"
  , "src/ForSyDe/Atom/MoC/DE/Core.hs"
  , "src/ForSyDe/Atom/MoC/DE/Lib.hs"
  , "src/ForSyDe/Atom/MoC/DE/Interface.hs"
  , "src/ForSyDe/Atom/MoC/DE/Hybrid.hs"
  , "src/ForSyDe/Atom/MoC/DE/React/Core.hs"
  , "src/ForSyDe/Atom/MoC/DE/React/Lib.hs"
  , "src/ForSyDe/Atom/MoC/DE/React/LF.hs"
  , "src/ForSyDe/Atom/MoC/CT/Core.hs"
  , "src/ForSyDe/Atom/MoC/CT/Lib.hs"
  , "src/ForSyDe/Atom/MoC/CT/Interface.hs"
  , "src/ForSyDe/Atom/MoC/SDF/Core.hs"
  , "src/ForSyDe/Atom/MoC/SDF/SDF.hs"
  , "src/ForSyDe/Atom/MoC/SDF/SADF.hs"
  , "src/ForSyDe/Atom/MoC/SDF/CSDF.hs"
  , "src/ForSyDe/Atom/MoC/SDF/BDF.hs"
  , "src/ForSyDe/Atom/MoC/SDF/Interface.hs"
  , "src/ForSyDe/Atom/Skel/Vector/Core.hs"
  , "src/ForSyDe/Atom/Skel/Vector/Lib.hs"
  , "src/ForSyDe/Atom/Prob.hs"
  , "src/ForSyDe/Atom/Prob/Uniform.hs"
  , "src/ForSyDe/Atom/Prob/Normal.hs"
  ]
