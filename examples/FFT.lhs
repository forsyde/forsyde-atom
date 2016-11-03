\ignore{
\begin{code}
{-# LANGUAGE PostfixOperators #-}  
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
  where bW x = (cis . negate) (-2 * pi * (fromInteger (x - 1)) / (fromInteger bN))
\end{code}
\begin{code}
fft :: (Complex Double -> a -> a -> (a, a)) -> Integer -> V.Vector a -> V.Vector a
fft butterfly k vs = (V.bitrev . (stage `V.pipe1` (V.iterate k (*2) 2))) vs
  where
    stage   w = V.concat . (segment `V.map21` (twiddles n)) . V.group w
    segment t = (<>)V.unduals . (<>)((butterfly t) `V.map22`) . V.duals
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

In conclusion, we have shown a parametrizable constructor belonging to the skeleton layer which creates an FFT structure. In this sense we can consider it a template describing the structure of an FFT butterfly network. For example, Figure \ref{fig:fft-struct} shows an FFT network instance for $k=3$.

\begin{figure}[h]
  \centering
  \includegraphics[]{figs/fft_struct.pdf}
  \caption{An example instance of the FFT process network for $k=3$}
  \label{fig:fft-struct}
\end{figure}

\subsection{Case 1: Vector of Signals}
\label{sec:case-1:-vector}


\begin{equation*}
\mathtt{fft} : (\wrapdf{\mathbb{C}_e^2 \rightarrow \mathbb{C}_e^2}) 
               \rightarrow \mathbb{N} 
               \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)} 
               \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)}
\end{equation*}


The first argument, $\mathtt{bffunc} : (\wrapdf{\mathbb{C}_e^2 \rightarrow \mathbb{C}_e^2})$ is a 2-input and 2-output behavior-wrapped and context-wrapped function performing the butterfly computation, as taken by the $\combM$ MoC constructor. The second argument $\mathtt{k}:\mathbb{N}$ is the number of stages of the FFT network. The third argument is a vector of signals $\mathtt{vs} : \vect{\mathcal{S}(\mathbb{C}_e)}$ carrying the samples which need to be computed.

\textbf{OBS:} The design assumes that $L(\mathtt{vs}) = 2^{\mathtt{k}}$. The testbench takes care that this condition is satisfied. 


Unfortunately, due to Haskell's strict type system and to several library design decisions, apart from a context wrapper, we \emph{do} need to define functions for timed and untimed MoCs differently. While timed MoCs require functions on values directly, untimed MoCs need to specify functions as operating on lists of values, as to mirror the partial order of events. Here is how we define the butterfly functions for timed and untimed MoCs:
\begin{code}
bffuncT w  x0   x1  = let t = w * x1 in ( x0 + t,   x0 - t)
bffuncU w [x0] [x1] = let t = w * x1 in ([x0 + t], [x0 - t])
\end{code}
and based on these definitions, here is how we properly wrap with a context and a behavior the butterfly function, according to the currently implemented MoCs, so that they fit as an argument to the \texttt{butterfly} process constructor:
\begin{code}
bffuncSY  w = SY.wrap22              (B.psi22 (\x0 x1 -> bffuncT w x0 x1))
bffuncDE  w = DE.wrap22              (B.psi22 (\x0 x1 -> bffuncT w x0 x1))
bffuncCT  w = CT.wrap22              (B.psi22 (\x0 x1 -> bffuncT w x0 x1))
bffuncSDF w = SDF.wrap22 (1,1) (1,1) (B.psi22 (\x0 x1 -> bffuncU w x0 x1))
\end{code}


Further on, we need a testbench for our FFT. As input stage, we provide a signal sampler which feeds the FFT network with $2^k$ signals. To 
keep the design simple, the sampler does not buffer the samples, rather it streams them through a systolic array of delays, like in Figure \ref{fig:fft-sampler}.
\begin{code}
sampler delta k s = (V.fanoutn (2 ^ k) delta) `V.systolic0` s
\end{code}
\begin{figure}[h]
  \centering
  \includegraphics[]{figs/fft_sampler.pdf}
  \caption{The input sampler}
  \label{fig:fft-sampler}
\end{figure}

As with the butterfly function, the sampler needs to input the correctly wrapped delay process. In this case we shall use the helper constructors provided by each MoC.
\begin{code}
samplerSY  = sampler (SY.delay 0)
samplerDE  = sampler (DE.delay 5 0)
samplerCT  = sampler (CT.delay (1/300) (\_->0))
samplerSDF = sampler (SDF.delay [0])
\end{code}

Finally, a sink function has been provided, which extracts the magnitude from the FFT results and synchronizes all output signals into one signal carrying vectors. 

\begin{code}
sinkSY  vs = SY.zipx               $ (SY.comb11           magnitude)  `V.map11` vs 
sinkDE  vs = DE.zipx               $ (DE.comb11           magnitude)  `V.map11` vs 
sinkCT  vs = CT.zipx               $ (CT.comb11           magnitude)  `V.map11` vs 
sinkSDF vs = SDF.zipx (V.fanout 1) $ (SDF.comb11 (1,1,map magnitude)) `V.map11` vs
\end{code}

Putting it all together, the testbench and DUT for signals of different MoCs serializes the simple sampler, the FFT network and sink, as in the following listing. Notice that the parameter \texttt{k} is used both for specifying the number of FFT stages and for providing the right amount of sample signals.
\begin{code}
butterfly bffunc w         = ((bffunc w) `MoC.comb22`)

testFFTSY  k = sinkSY  . fft (butterfly bffuncSY)  k . samplerSY  k
testFFTDE  k = sinkDE  . fft (butterfly bffuncDE)  k . samplerDE  k
testFFTCT  k = sinkCT  . fft (butterfly bffuncCT)  k . samplerCT  k
testFFTSDF k = sinkSDF . fft (butterfly bffuncSDF) k . samplerSDF k
\end{code}

\subsection{Case 2: Signal of Vectors}
\label{sec:case-2:-signal}

\subsection{Case 3: SDF network}
\label{sec:case-3:-sdf}


\subsection{Simulation}
\label{sec:simulation-results}




To test the correctness of out FFT network, we shall consider a signal formed of two overlapping sine waves, as plotted below:

\begin{code}
doublesine t = sin (2 * pi * 50 * ((fromRational t) :+ 0)) + 3 * sin (2*pi*90 * (fromRational t) + 2)

inputSignalSY = SY.comb11 ((:+ 0) . (\t->sin (2*pi*freq*t) + 3 * sin (2*pi*90*t + 2)) . (*tau)) samps
  where freq  = 50
        tau   = 1/500
        samps = SY.signal [0..299]

\end{code}

\begin{tikzpicture}\centering
\begin{axis}[ticks=none, width=15cm, height=4cm]
\pgfplotstabletranspose\datatable{data/sine.dat}
\addplot[mark=none] table {\datatable};
\end{axis}
\end{tikzpicture}

First, let us see how the network behaves in continuous time. For this, we create the CT signal \texttt{inputSignalCT} which carries only one event: the function \texttt{doublesine} which starts from time 0.

\begin{code}
inputSignalCT = CT.signal [(0, doublesine)]
\end{code}

Feeding the signal to a 128-bin FFT network:

\begin{code}
outSignalCT = testFFTCT 7 inputSignalCT
outSignalSY = testFFTSY 7 inputSignalSY
\end{code}

% \begin{tikzpicture}\centering
% \begin{axis}[]
%   \addplot3[raw gnuplot,surf]
%   gnuplot[id={surf}]{%
%     set pm3d;
%     splot 'data/outCT.dat' matrix with pm3d;};
% \end{axis}
% \end{tikzpicture}




\ignore{
\begin{code}
outpath = "examples/latex/data/"
listToDat   = map (\x -> if x == ',' then ' ' else x ) . filter (not . (`elem` "[]")) . show

sigVecToDat :: Signal [Value (V.Vector Double)] -> String
sigVecToDat = map replchar . filter (not . (`elem` "{}<[]")) . show
  where replchar '>' = '\n'
        replchar ',' = ' '
        replchar c   = c

saveDoubleSine = do
  let xaxis    = map (*(1/500)) [0..299]
      filepath = outpath ++ "sine.dat"
    in writeFile filepath $ listToDat $ map doublesine xaxis

saveSVOutput name sig = do
  let sigVecToDat = map replchar . filter (not . (`elem` "{}<\b")) . show
        where replchar '>' = '\n'
              replchar ',' = ' '
              replchar c   = c
      filepath = outpath ++ name
    in writeFile filepath $ sigVecToDat $ sig


saveCtOutput = do
  let sampledOut = CT.eval $ CT.splitUntil 2 0.005 $ outSignalCT
      filepath = outpath ++ "outCT.dat"
    in writeFile filepath $ sigVecToDat $ sampledOut
\end{code}%"
}

%-- fft :: Int -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
%-- fft k xs | n == 2 ^ k = (zipxPN . bitrevPN . unzipxPN . pipe1PN stage (iterateV k (*2) 2)) xs
%--   where
%-- 	stage :: Int -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex Float))
%-- 	stage k = zipxPN . concatPN . (mapV unzipxPN) . farm1PN segment (takeV m twiddles) 
%--               . (mapV zipxPN) . groupPN k . unzipxPN
%-- 		where m = n `div` k
%
%-- 	segment :: Complex Float -> Signal (Vector (Complex Float)) -> Signal (Vector (Complex %Float))
%-- 	segment twid = zipxPN . undualsSYPN . farmPN (butterfly twid) . dualsSYPN . unzipxPN
%
%-- 	butterfly :: RealFloat a => Complex a -> Signal (Complex  a, Complex a) -> Signal (Complex %a, Complex a)
%-- 	butterfly w = mapSY (\(x0, x1) -> let t = w * x1 in (x0 + t, x0 - t))
%
%-- 	twiddles :: Vector (Complex Float)
%-- 	twiddles = vector $ (bitrev . map (cis . negate) . halfcycle) (toInteger n)
%
%-- 	halfcycle :: Integer -> [Float]
%-- 	halfcycle n = halfcycle1 0 (fromInteger n / 2) n
%-- 	  	where halfcycle1 l m n 
%-- 			           | l == m = []
%-- 					   | l /= m = -2 * pi * l / (fromInteger n) : halfcycle1 %(l+1) m n
%
%-- 	n = lengthV $ unzipxPN xs
%
%-- -- helpers
%
%-- evens []  = []
%-- evens [x] = [x]
%- evens (x:_:xs) = x : evens xs
%-- odds []  = []
%-- odds [x] = []
%-- odds (_:x:xs) = x : odds xs
%-- bitrev :: [a] -> [a]
%-- bitrev [x] = [x]
%-- bitrev xs = bitrev (evens xs) ++ bitrev (odds xs)


%%% Local Variables:
%%% TeX-command-default: "Make"
%%% mode: latex
%%% TeX-master: "latex/examples"
%%% End:
