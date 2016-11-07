runTestBench nsamp = do 
  let  inputSineWave  = testwave
       fft1SYResult   = SY.zipx test1FFTSY
       fft1DEResult   = (snd . DE.toSY . DE.zipx) test1FFTDE
       fft1CTResult   = (snd . DE.toSY . CT.toDE sampPer testWaveDE . CT.zipx) test1FFTCT
       fft1SDFResult  = SDF.zipx (V.fanout 1) test1FFTSDF
  saveInWave inputSineWave
  saveSigVec "out1SY.dat" 200  fft1SYResult 
  saveSigVec "out1DE.dat" 200  fft1DEResult
  saveSigVec "out1CT.dat" 200  fft1CTResult
  saveSigVec "out1SDF.dat" 200 fft1SDFResult


convertToEps nsamp inFile outFile = do
  let datFile = outpath ++ infile
      epsFile = outpath ++ outfile
      plotScript =
        "set yrange [0:" ++ nsamp ++ "]\n"
        ++ "set xrange [0:" ++ nBins ++ "]\n"
        ++ "set terminal postscript eps color\n"
        ++ "set output \"" ++ epsFile ++"\"\n"
        ++ "plot \'" ++ datFile ++ "\' \n"
  writeFile "script.gnuplot" plotScript
  _ <- system ("gnuplot -persist script.gnuplot")
