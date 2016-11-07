\ignore{
\begin{code}
{-# LANGUAGE PostfixOperators, TypeFamilies  #-}  
\end{code}
}

\section{An FFT network}
\label{sec:an-fft-network}

This example shows how to instantiate an FFT butterfly process network using skeleton layer constructors operating on \texttt{Vector} types, and simulates the system's response to signals of different MoCs.  It was inspired from \cite{reekie95}.

To be consistent with the current chapter, we shall separate the interconnection network expressed in terms of skeleton layer constructors, from the functional/behavioral/timing components further enforcing the practice of \emph{orthogonalization} of concerns in system design. Also, to demonstrate the elegance of this approach, we shall construct two different functionally-equivalent systems which expose different propoerties: a) the butterfly pattern as a network of interconnected processes communicating through (vectors of) signals; b) the FFT as a process operating on a signal (of vectors). Each of these two cases will be fed with signals of different MoCs to further demonstrate the independence of the atom (i.e. constructor) composition from the semantics carried by atoms themselves.

\begin{code}
module FFT where
\end{code}

Following are the loaded modules. We import most modules qualified to explicitly show to which layer or class a certain constructor belongs to. While, from a system designer's perspective, loading the \texttt{Behavior} and \texttt{MoC} modules seems redundant since there exist equivalent helpers for each MoC for instantiating the desired processes, we do want to show the interaction between (and actually the separation of) each layer. 

\begin{code}
import           ForSyDe.Atom
import qualified ForSyDe.Atom.Behavior        as B   (psi22, psi11)
import qualified ForSyDe.Atom.MoC             as MoC (comb22, comb11)
import qualified ForSyDe.Atom.MoC.SY          as SY  
import qualified ForSyDe.Atom.MoC.DE          as DE  
import qualified ForSyDe.Atom.MoC.CT          as CT  
import qualified ForSyDe.Atom.MoC.SDF         as SDF
import qualified ForSyDe.Atom.Skeleton.Vector as V
\end{code}

The \texttt{Data.Complex} module is needed for the complex arithmetic computations performed by a ``butterfly'' element. It provides the \texttt{cis} function for conversion towards a complex number, and the \texttt{magnitude} function for extracting the magnitude of a complex number.
\begin{code}
import Data.Complex
import Data.Ratio
import System.Process
\end{code}


\subsection{The butterfly network}
\label{sec:butterfly-network}

The Fast Fourier Transform (FFT) is an algorithm for computing the Discrete Fourier Transform (DFT), formulated by Cooley-Tukey as a ``divide-and-conquer'' algorithm computing \eqref{eq:dft}, where $N$ is the number of signal samples, also called \emph{bins}.

\begin{equation}\label{eq:dft}
  X_k = \sum_{n=0}^{N-1}x_n e^{-i2\pi k n / N}\quad\text{ where }\quad k=0,...,N-1 
\end{equation}

This algorithm is often implemented in an iterative manner for reasons of efficiency. Figure \ref{fig:fft-struct} shows an example diagram for visualizing this algorithm, and we shall encode precisely this diagram as a network of interconnected processes (or functions).

We start by computing a vector of ``twiddle'' factors, which are complex numbers situated on the unit circle. For $n=16$, the value of this vector is:
\begin{equation}\label{eq:twiddles}
  \vect{W_{16}^0,W_{16}^4,W_{16}^2,W_{16}^6,W_{16}^1,W_{16}^5,W_{16}^3,W_{16}^7}\quad\text{ where }\quad W_{N}^m = e^{-2\pi m / N}
\end{equation}
the actual vector is obtained by mapping the function \texttt{bW} over a vector of \texttt{indexes} and \texttt{bitrevers}ing the result.
\begin{code}
twiddles :: Integer -> V.Vector (Complex Double)
twiddles bN = (V.reverse . V.bitrev . V.take (bN `div` 2)) (bW `V.map11` V.indexes)
  where bW x = (cis . negate) (-2 * pi * fromInteger (x - 1) / fromInteger bN)
\end{code}
\begin{code}
fft :: (Complex Double -> a -> a -> (a, a)) -> Integer -> V.Vector a -> V.Vector a
fft butterfly k vs = (V.bitrev . (stage `V.pipe1` (V.iterate k (*2) 2))) vs
  where
    stage   w = V.concat . (segment `V.map21` (twiddles n)) . V.group w
    segment t = (<>)V.unduals . (<>)(butterfly t `V.map22`) . V.duals
    n         = V.length vs        -- length of input 
\end{code}

The \texttt{fft} network is instantiated with the function above. Its type signature is:
\begin{equation}\label{eq:fft-type}
  \mathtt{fft} : (\mathbb{C} \rightarrow \alpha^2 \rightarrow \alpha^2) \rightarrow \mathbb{N} \rightarrow \vect{\alpha} \rightarrow \vect{\alpha}  
\end{equation}
and, as it suggests, it is not concerned in the actual functionality implemented, rather it is just a skeleton (higher order function) which instantiates an interconnection network. Its arguments are:
\begin{itemize}
\item $\mathtt{vs}:\vect{\alpha}$ the vector of input samples, where $L(\mathtt{vs})=N$
\item $\mathtt{k}:\mathbb{N}$ the number of butterfly stages, which needs to satisfy the condition $2^{\mathtt{k}} = N$ 
\item $\mathtt{butterfly}:\mathbb{C} \rightarrow \alpha^2 \rightarrow \alpha^2$ which performs the computation suggested by Figure \ref{fig:fft-butterfly}, in different formats, depending on the test case.
\end{itemize}%
\begin{figure}[h]\centering
  \includegraphics[]{figs/fft_butterfly.pdf}
  \caption{A one-stage butterfly network}
  \label{fig:fft-butterfly}
\end{figure}

\begin{figure}[h!]
  \begin{subfigure}{\textwidth}
    \centering
    \includegraphics[scale=.83]{figs/fft_top.pdf}
    \caption{Top view of the \texttt{fft} process network}
    \label{fig:fft-top}
  \end{subfigure}
  \begin{subfigure}{\textwidth}
    \centering
    \includegraphics[scale=.83]{figs/fft_stage.pdf}
    \caption{The \texttt{fft} process network with explicit \texttt{stage}}
    \label{fig:fft-stage}
  \end{subfigure}
  \begin{subfigure}{\textwidth}
    \centering
    \includegraphics[scale=.83]{figs/fft_complete.pdf}
    \caption{The complete \texttt{fft} process network}
    \label{fig:fft-complete}
  \end{subfigure}
  \caption{A step-by-step depiction of the network created by the \texttt{fft} skeleton, interpreted as a process network}
\end{figure}

Figure~\ref{fig:fft-top} draws the top view of the \texttt{fft} function where the vector sequence $\vect{2,4,8,16,...,2^k}$ used to get the butterfly network width for each stage is generated using an \texttt{iterate} skeleton. A pipeline pattern is created by serializing \texttt{stage}, and its result is \texttt{bitrev}ersed. Drawing \texttt{stage} explicitly adds more details, like in Figure~\ref{fig:fft-stage}. Here the width \texttt{w} is used to group the vectors that enter the butterfly segments. Making \texttt{segment} also explicit and assuming that \texttt{butterfly} is a process, grants us the process network from Figure~\ref{fig:fft-complete}, where the $W$ coefficient corresponding to each butterfly segment is extracted from the \texttt{twiddles} vector.

So we have just shown a parametrizable constructor belonging to the skeleton layer which creates an FFT structure. In this sense we can consider it a template describing the structure of an FFT butterfly network. For example, Figure \ref{fig:fft-struct} shows an FFT network instance for $k=3$.

\begin{figure}[h]
  \centering
  \includegraphics[]{figs/fft_struct.pdf}
  \caption{An example instance of the FFT process network for $k=3$}
  \label{fig:fft-struct}
\end{figure}

\subsection{Case 1: Vector of Signals}
\label{sec:case-1:-vector}

The first case study treats the \texttt{fft} skeleton as creating a ForSyDe process network. In this sense, we can the $\boxed{f}$ blocks from Figure~\ref{fig:fft-struct} as being processes executing with the semantics inferred by a Model of Computation, and the arrows as being signals. Thus the type signatures for \texttt{fft} and \texttt{butterfly} become:
\begin{align*}
\mathtt{fft}& : \mathbb{N} 
    \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)} 
    \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)}\\
\mathtt{butterfly}& : \mathbb{C}
          \rightarrow \mathcal{S}(\mathbb{C}_e)^2 
          \rightarrow \mathcal{S}(\mathbb{C}_e)^2\\
\end{align*}
The $fft$ skeleton behaves as a process network constructor simply by passing \texttt{butterfly} as a well formed process, having the type signature above. In order to do so, we need to wrap the butterfly function from Figure~\ref{fig:fft-butterfly} with behavioral and timing semantics, i.e. to pass it to MoC/behavior layer constructors.

As proposed in the introduction, we intend to test the \texttt{fft} skeleton to execute based on different timing models, i.e. with different MoC semantics. While in theory a process' MoC could be \emph{only} inferred from the tag system of the signal(s) it operates on, in practice we do need to provide slightly different context information for each MoC. Most of these inconsistencies are due to propagated design decisions in the ForSyDe-Atom library in order to satisfy Haskell's strict type system. While we could just use the library-provided helpers for instantiating well-formed processes (i.e. \texttt{[MoC].comb22}), we want to show the different implications of each MoC by individually wrapping functions with behavioral semantics (i.e. the default behavior \texttt{psi22}), a context and passing the result to the generic MoC pattern \texttt{comb22}.
\begin{code}
butterflySY  :: Complex Double ->  SY.Sig (Complex Double) ->  SY.Sig (Complex Double) ->  (SY.Sig (Complex Double),  SY.Sig (Complex Double))
butterflyDE  :: Complex Double ->  DE.Sig (Complex Double) ->  DE.Sig (Complex Double) ->  (DE.Sig (Complex Double),  DE.Sig (Complex Double))
butterflyCT  :: Complex Double ->  CT.Sig (Complex Double) ->  CT.Sig (Complex Double) ->  (CT.Sig (Complex Double),  CT.Sig (Complex Double))
butterflySDF :: Complex Double -> SDF.Sig (Complex Double) -> SDF.Sig (Complex Double) -> (SDF.Sig (Complex Double), SDF.Sig (Complex Double))
butterflySY  w s0 s1 =  SY.wrap22             (B.psi22 (bffunc  w)) `MoC.comb22` s0 $s1
butterflyDE  w s0 s1 =  DE.wrap22             (B.psi22 (bffunc  w)) `MoC.comb22` s0 $s1
butterflyCT  w s0 s1 =  CT.wrap22             (B.psi22 (bffunc  w)) `MoC.comb22` s0 $s1
butterflySDF w s0 s1 = SDF.wrap22 (1,1) (1,1) (B.psi22 (bffuncU w)) `MoC.comb22` s0 $s1
\end{code}

The function wrapped by the \texttt{butterfly} network is \texttt{bffunc(U)} and needs to be specified differently depending on whether the MoC is timed or untimed. For timed MoCs, it is simply a function on values, but for untimed MoCs it needs to reflect the fact that it operates on multiple tokens, thus we express this in ForSyDe-Atom as a function on lists of values.

\begin{code}
bffunc  w  x0   x1  = let t = w * x1 in ( x0 + t,   x0 - t)
bffuncU w [x0] [x1] = let t = w * x1 in ([x0 + t], [x0 - t])
\end{code}

Now we can instantiate \texttt{fft} as a parallel process network with:

\begin{code}
fft1SY  = fft butterflySY
fft1DE  = fft butterflyDE
fft1CT  = fft butterflyCT
fft1SDF = fft butterflySDF
\end{code}

\subsection{Case 2: Signal of Vectors}
\label{sec:case-2:-signal}

The second case study treats \texttt{fft} as a function on vectors, executing with the timing semantics dictated by a specified MoC, i.e. wrapped in a MoC process constructor. This way, instead of exposing the parallelism at process network level, it treats it at data level, while synchronization is performed externally, as a block component.

For timed MoCs instantiating a process which performs \texttt{fft} on a vector of values is straightforward: simply pass the \texttt{bffunc} to the \texttt{fft} network, which in turn constitutes the function mapped on a signal by a \texttt{comb11} process constructor. We have shown in Section~\ref{sec:case-1:-vector} how to systematically wrap the function on values with different behavior/timing aspects. This time we will use the library-provided helpers to construct \texttt{comb11} processes with a default behavior.


\begin{code}
fft2SY k = SY.comb11 (fft bffunc k)
fft2DE k = DE.comb11 (fft bffunc k)
fft2CT k = CT.comb11 (fft bffunc k)
\end{code}

For untimed MoCs though, a more straightforward approach is to make use of the definition of actors as processes with a production/consumption rate for each output/input. In the case of SDF, we would simply have a process which consumes $2^k$ tokens from a signal (the input samples), and apply the \texttt{fft} function over them.

\begin{code}
fft2SDF k = SDF.comb11 (2^k, 2^k, V.fromVector . fft bffunc k . V.vector)
\end{code}

\begin{figure}[h]
  \centering
  \includegraphics[]{figs/fft_one_proc.pdf}
  \caption{The \texttt{fft} skeleton as a process function}
  \label{fig:fft-one-proc}
\end{figure}


\subsection{The testbench}
\label{sec:testbench}

To test the functionality of the FFT network/process, we need to provide (signals of) $N=2^k$ samples from a (multi-tone) wave. The simplest way to do this and to ensure consistency with our design is to take an infinite wave signal, and pass it through a ``sampler'' made of a systolic array of $N$ delays, like in Figure \ref{fig:fft-sampler}. For the timed processes in the second test case, all signals from the \texttt{sampler} stage are synchronized into one signal carrying vectors using the \texttt{zipx} interface.

\begin{code}
sampler delta k s = (V.fanoutn (2^k) delta) `V.systolic0` s
\end{code}

\begin{figure}[h!]
  \centering
  \includegraphics[]{figs/fft_sampler.pdf}
  \caption{The input sampler: for the FFT network \texttt{fft1} (left); for the process performing FFT \texttt{fft2} (right)}
  \label{fig:fft-sampler}
\end{figure}

Finally, in order to analyze the FFT output, the complex bins need to be converted into real numbers by calculating their magnitude. This way the output of the FFT is expected to see the amplitude of each tone contained by the input wave. We do that for each of our test cases:

\begin{code}
ampl1SY  vs = ( SY.comb11          magnitude  `V.map11`) vs 
ampl1DE  vs = ( DE.comb11          magnitude  `V.map11`) vs 
ampl1CT  vs = ( CT.comb11          magnitude  `V.map11`) vs 
ampl1SDF vs = (SDF.comb11 (1,1,map magnitude) `V.map11`) vs

ampl2SY  s  =   SY.comb11         (magnitude  `V.map11`) s
ampl2DE  s  =   DE.comb11         (magnitude  `V.map11`) s
ampl2CT  s  =   CT.comb11         (magnitude  `V.map11`) s
ampl2SDF s  =  SDF.comb11 (1,1,map magnitude)      s
\end{code}

Putting it all together, the testbench and DUT for signals of different MoCs serializes the simple sampler, the FFT network and sink, as in the following listing. Notice that the parameter \texttt{k} is used both for specifying the number of FFT stages and for providing the right amount of sample signals. The test case 2 for SDF does not need an input sampler stage since it consumes the right amount of samples as part of its specification. With the \texttt{delta} process we will control the sampling period, as we shall see in the next section. 
\begin{code}
testbench1SY  k delta = ampl1SY  . fft1SY  k . sampler delta k
testbench1DE  k delta = ampl1DE  . fft1DE  k . sampler delta k
testbench1CT  k delta = ampl1CT  . fft1CT  k . sampler delta k
testbench1SDF k delta = ampl1SDF . fft1SDF k . sampler delta k

testbench2SY  k delta = ampl2SY  . fft2SY  k . SY.zipx . sampler delta k
testbench2DE  k delta = ampl2DE  . fft2DE  k . DE.zipx . sampler delta k
testbench2CT  k delta = ampl2CT  . fft2CT  k . CT.zipx . sampler delta k
testbench2SDF k _     = ampl2SDF . fft2SDF k 
\end{code}

\subsection{Simulation}
\label{sec:simulation}

To test the correctness of out FFT network, we shall consider a signal formed of two overlapping sine waves, as plotted below:

For this, we provide a sine wave generator function which takes a list of parameters of type \texttt{(Amplitude, Frequency, Phase)} and sums up the resulting sines.

\begin{code}
wave :: [(Double, Double, Double)] -> CT.Time -> Complex Double
wave param t = (:+0) $ foldl (\ sum (a,f,phi) -> sum + a * sin (2 * pi * f * (fromRational t) + phi)) 0 param
\end{code}%$

For this experiment, we shall input a two-tone signal having the components $(A_1,f_1,\phi_1)$ into $(A_2,f_2,\phi_2)$ $(A_1,f_1,\phi_1)$ an FFT with $N$ bins. The signal needs to be sampled at $fs$, and to be able to identify the tone components, we have to ensure that $\max (f_1,f_2) \leq fs/2$. Below is the experimental data:

\begin{table}[h!]
  \centering
  \begin{tabular}{|c|c|}
    \hline Parameter       & Value            \\
    \hline $A_1,f_1,\phi_1$ & 1, 50 Hz, 0 rad  \\
    \hline $A_2,f_2,\phi_2$ & 3, 120 Hz, 2 rad  \\
    \hline $fs$            & 900 Hz           \\
    \hline $k$             & 7 stages         \\
    \hline $N$             & $2^k =$ 128 bins \\
    \hline
  \end{tabular}
  \caption{Experimental data}
  \label{tab:in-exp}
\end{table}

\begin{code}
testwave = wave [(1,50,0),(3,120,2)]
sampFreq = 900
kStages  = 7
nBins    = 2^kStages
sampPer  = 1%sampFreq
\end{code}

\begin{figure}[h!]\centering
  \begin{tikzpicture}
    \begin{axis}[ticks=none, width=15cm, height=4cm]
      \pgfplotstabletranspose\datatable{data/sine.dat}
      \addplot[mark=none] table {\datatable};
    \end{axis}
  \end{tikzpicture}    
  \caption{Plotted input signal}
\end{figure}

Now let us generate the signals for the tested MoCs. For the CT MoC, the signal will only have one event starting at time 0, carrying the function $\mathtt{testwave}(t)$. For the other MoCs though, we need to use a \texttt{generate} process.
\begin{code}
testWaveCT  = CT.signal [(0,testwave)]
testWaveSY  = ( SY.comb11          testwave  .  SY.generate1          (+sampPer))  0
testWaveDE  = ( DE.comb11          testwave  .  DE.generate1          (+sampPer))  (1, 0)
testWaveSDF = (SDF.comb11 (1,1,map testwave) . SDF.generate1 (1,1,map (+sampPer))) [0]
\end{code}

Now we need to provide a delay element as an argument for the sampler array. As suggested in Figure~\ref{fig:fft-sampler}, each pair of neighboring signals will contain the same sine wave phase-shifted with exactly one sampling period ($ts=1/fs$). This is clearly seen in the generation of \texttt{testwave(SY|DE|SDF)} and in the definition of \texttt{delayCT}.

\begin{code}
deltaSY  =  SY.delay               (0.0:+0.0)
deltaDE  =  DE.delay 1             (0.0:+0.0)
deltaCT  =  CT.delay (sampPer) (\_->0.0:+0.0)
deltaSDF = SDF.delay               [0.0:+0.0]
\end{code}

We have provided a complete set of testbenches for the test cases presented in Sections~\ref{sec:case-1:-vector}~and~\ref{sec:case-2:-signal}. Since we input infinite signals, we expect to get infinite signals in return, containing the resulting values of the $N$ bins. 
\begin{code}
test1FFTSY  = testbench1SY  kStages deltaSY  testWaveSY
test1FFTDE  = testbench1DE  kStages deltaDE  testWaveDE
test1FFTCT  = testbench1CT  kStages deltaCT  testWaveCT
test1FFTSDF = testbench1SDF kStages deltaSDF testWaveSDF

test2FFTSY  = testbench2SY  kStages deltaSY  testWaveSY
test2FFTDE  = testbench2DE  kStages deltaDE  testWaveDE
test2FFTCT  = testbench2CT  kStages deltaCT  testWaveCT
test2FFTSDF = testbench2SDF kStages deltaSDF testWaveSDF
\end{code}

In Figure~\ref{fig:result} we plot the first 200 samples for all test cases, with a provided script which takes the number of samples as argument:

\begin{lstlisting}
runTestBench 200
\end{lstlisting}


\begin{figure}[ht!]
  \centering
  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out1SY}\caption{\texttt{test1FFTSY}}\label{fig:test1SY}
  \end{subfigure}
  ~
  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out1DE}\caption{\texttt{test1FFTDE}}\label{fig:test1DE}
  \end{subfigure}
  ~
  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out1CT}\caption{\texttt{test1FFTCT}}\label{fig:test1CT}
  \end{subfigure}
  ~  
  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out1SDF}\caption{\texttt{test1FFTSDF}}\label{fig:test1SDF}
  \end{subfigure}

  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out2SY}\caption{\texttt{test2FFTSY}}\label{fig:test2SY}
  \end{subfigure}
  ~
  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out2DE}\caption{\texttt{test2FFTDE}}\label{fig:test2DE}
  \end{subfigure}
  ~
  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out2CT}\caption{\texttt{test2FFTCT}}\label{fig:test2CT}
  \end{subfigure}
  ~
  \begin{subfigure}{.23\linewidth}
    \includegraphics[width=\linewidth]{data/out2SDF}\caption{\texttt{test2FFTSDF}}\label{fig:test2SDF}
  \end{subfigure}
  \caption{Heatmap plots for all FFT bins for all test cases and MoCs. The $N$ bins are represented on the $x$ axis, whereas the output samples (in time) are stacked on the $y$ axis}
  \label{fig:result}
\end{figure}

Some observations:
\begin{itemize}
\item the two tone components can be observed on the heatmaps in form of the mirrored yellow (120Hz) and light blue (50Hz) vertical lines.
\item The first $N$ output bins in \Cref{fig:test1SY,fig:test1DE,fig:test1CT,fig:test1SDF,fig:test2SY,fig:test2DE,fig:test2CT} expose the effect of ``filling in'' the delay network we use as sampler. \Cref{fig:test2SDF} does not show this behavior since \texttt{test2FFTSDF} does not use a input sampler at all, instead it consumes $N$ samples of complete sine wave at each firing of the actor (see Figure~\ref{fig:fft-one-proc}).
\item The ``wavy'' shape if the FFT output after stabilization (after $N$ samples) is due to the fact that the sampling window is time-shifting and does not capture an integer multiple of the input periods. We also do not use a correction window (e.g. Hamming).
\item  Although the FFT is a DFT method and would not make sense in continuous time, the fact that \Cref{fig:test1CT,fig:test2CT} show the exact same output as the rest demonstrates an important property of the inherent lazy evaluation mechanism: a CT process network is practically discretized once we ``observe it'' (i.e. plot it), since we force the runtime to evaluate the value of a signal at certain (discrete) instances in time. In this sense we can make the observation that no matter where the discretization takes place (before the delay network in the case of DE or after the FFT stage in the case of CT), the results are practically the same since the all system instances are \emph{functionally equivalent}.
\end{itemize}




\ignore{
\begin{code}
outpath = "examples/latex/data/"

rmBraces = filter (not . (`elem` "[]{}<\b"))
comma2spc = map (\x -> if x == ',' then ' ' else x )
grt2newln = map (\x -> if x == '>' then '\n' else x )

saveInWave sig = do
  let xaxis    = map (*sampPer) [0 .. 150]
      filepath = outpath ++ "sine.dat"
    in (writeFile filepath . comma2spc . rmBraces . show . map realPart . map sig) xaxis

saveSigVec name nsamp sig = do
  let filepath = outpath ++ name
      outsig   = takeS nsamp sig
    in (writeFile filepath . grt2newln . comma2spc . rmBraces . show) outsig

convertToEps nsamp inFile outFile = do
  let datFile = outpath ++ inFile
      epsFile = outpath ++ outFile
      plotScript =
        "set view map\n"
        ++ "set yrange [0:" ++ nsamp ++ "]\n"
        ++ "set xrange [0:" ++ show nBins ++ "]\n"
        ++ "set terminal postscript eps color\n"
        ++ "set output \"" ++ epsFile ++"\"\n"
        ++ "plot \'" ++ datFile ++ "\' matrix with image \n"
  writeFile "script.gnuplot" plotScript
  system ("gnuplot -persist script.gnuplot")

runTestBench nsamp = do 
  let  inputSineWave  = testwave
       fft1SYResult   = SY.zipx test1FFTSY
       fft1DEResult   = (snd . DE.toSY . DE.zipx) test1FFTDE
       fft1CTResult   = (snd . DE.toSY . CT.toDE sampPer testWaveDE . CT.zipx) test1FFTCT
       fft1SDFResult  = SDF.zipx (V.fanout 1) test1FFTSDF
       fft2SYResult   = test2FFTSY
       fft2DEResult   = (snd . DE.toSY) test2FFTDE
       fft2CTResult   = (snd . DE.toSY . CT.toDE sampPer testWaveDE) test2FFTCT
       fft2SDFResult  = SDF.comb11 (nBins,1,\ x->[V.vector x]) test2FFTSDF
  putStrLn "Plotting input signal"
  saveInWave inputSineWave
  putStrLn "Plotting out1SY.dat"
  saveSigVec "out1SY.dat" nsamp  fft1SYResult
  convertToEps (show nsamp) "out1SY.dat" "out1SY.eps" 
  putStrLn "Plotting out1DE.dat"
  saveSigVec "out1DE.dat" nsamp  fft1DEResult
  convertToEps (show nsamp) "out1DE.dat" "out1DE.eps"
  putStrLn "Plotting out1CT.dat"
  saveSigVec "out1CT.dat" nsamp  fft1CTResult
  convertToEps (show nsamp) "out1CT.dat" "out1CT.eps"
  putStrLn "Plotting outSDF.dat"
  saveSigVec "out1SDF.dat" nsamp fft1SDFResult
  convertToEps (show nsamp) "out1SDF.dat" "out1SDF.eps"
  putStrLn "Plotting out2SY.dat"
  saveSigVec "out2SY.dat" nsamp  fft2SYResult 
  convertToEps (show nsamp) "out2SY.dat" "out2SY.eps"
  putStrLn "Plotting out2DE.dat"
  saveSigVec "out2DE.dat" nsamp  fft2DEResult
  convertToEps (show nsamp) "out2DE.dat" "out2DE.eps"
  putStrLn "Plotting out2CT.dat"
  saveSigVec "out2CT.dat" nsamp  fft2CTResult
  convertToEps (show nsamp) "out2CT.dat" "out2CT.eps"
  putStrLn "Plotting out2SDF.dat"
  saveSigVec "out2SDF.dat" nsamp fft2SDFResult
  convertToEps (show nsamp) "out2SDF.dat" "out2SDF.eps"

\end{code}%"$
}


%%% Local Variables:
%%% TeX-command-default: "Make"
%%% mode: latex
%%% TeX-master: "latex/examples"
%%% End:
