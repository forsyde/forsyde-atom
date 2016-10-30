\chapter{An FFT network}

This example shows how to instantiate an FFT process network using skeleton layer constructors operating on \texttt{Vector} types, and simulates the system's response for signals of different MoCs.  It was inspired from \cite{reekie95}. 

\ignore{
\begin{code}
{-# LANGUAGE PostfixOperators #-}  
\end{code}
}

\begin{code}
module FFT where
\end{code}

Following are the loaded modules. In order to show the generality of this method, the \texttt{fft} network is unaware of the timing semantics it operates with, since timing aspects are captured in a layer below: the MoC layer. To show this, instead of using the short helpers for process constructors provided for each MoC, we deconstruct processes into their respective layers and use those constructors instead. This is why we import \texttt{Behavior} for its \texttt{psi} behavior constructor. The \texttt{MoC.SY}, \texttt{MoC.DE}, \texttt{MoC.CT} and \texttt{MoC.SDF} modules are imported only for their context wrappers (\texttt{wrap}), \texttt{signal} utilities, and their \texttt{delay} helpers for fully-instantiated \texttt{delay} patterns. 

\begin{code}
import           ForSyDe.Atom
import           ForSyDe.Atom.Behavior       (psi22, psi11)
import qualified ForSyDe.Atom.MoC     as MoC (comb22, comb11)
import qualified ForSyDe.Atom.MoC.SY  as SY  
import qualified ForSyDe.Atom.MoC.DE  as DE  
import qualified ForSyDe.Atom.MoC.CT  as CT  
import qualified ForSyDe.Atom.MoC.SDF as SDF
import qualified ForSyDe.Atom.Skeleton.Vector as V
\end{code}

The \texttt{Data.Complex} module is needed for the complex arithmetic computations performed by a ``butterfly'' element. It provides the \texttt{cis} function for conversion towards a complex number, and the \texttt{magnitude} function for extracting the magnitude of a complex number.
\begin{code}
import Data.Complex
\end{code}

The \texttt{fft} network is instantiated with the following function. Its type signature is:
\begin{equation*}
\mathtt{fft} : (\wrapdf{\mathbb{C}_e^2 \rightarrow \mathbb{C}_e^2}) 
               \rightarrow \mathbb{N} 
               \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)} 
               \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)}
\end{equation*}

\begin{code}
fft bffunc k vs = (V.bitrev . (stage `V.pipe1` (V.iterate k (*2) 2))) vs
  where
    n = V.length vs     -- n is the length of the input vector 
    stage     w         = V.concat . (segment `V.map21` (V.take (n `div` w) twiddles)) . V.group w
    segment   twid      = (V.unduals<>) . (<>)((butterfly twid) `V.map22`) . V.duals
    butterfly w         = ((bffunc w) `MoC.comb22`)
    twiddles            = (V.bitrev . (halfcycle `V.map11`) . V.take (n `div` 2)) V.indexes
      where halfcycle l = (cis . negate) (-2 * pi * (fromInteger (l - 1)) / (fromInteger n))
\end{code}

The first argument, $\mathtt{bffunc} : (\wrapdf{\mathbb{C}_e^2 \rightarrow \mathbb{C}_e^2})$ is a 2-input and 2-output behavior-wrapped and context-wrapped function performing the butterfly computation, as taken by the $\combM$ MoC constructor. The second argument $\mathtt{k}:\mathbb{N}$ is the number of stages of the FFT network. The third argument is a vector of signals $\mathtt{vs} : \vect{\mathcal{S}(\mathbb{C}_e)}$ carrying the samples which need to be computed.

\textbf{OBS:} The design assumes that $L(\mathtt{vs}) = 2^{\mathtt{k}}$. The testbench takes care that this condition is satisfied. 

Figure \ref{fig:fft-top} draws the top view of the \texttt{fft} function where the \texttt{iterate} skeleton generates the vector sequence $\vect{2,4,8,16,...}$ where the length is given by \texttt{k}. A pipeline pattern is created by serializing \texttt{stage}, and its result is \texttt{bitrev}ersed.

\begin{lstlisting}
fft bffunc k = V.bitrev . stage `V.pipe1` (V.iterate k (*2) 2)
\end{lstlisting}
\begin{figure}[h]
  \centering
  \includegraphics[]{figs/fft_top.pdf}
  \caption{Top view of the \texttt{fft} process network}
  \label{fig:fft-top}
\end{figure}

If we draw  
$\mathtt{stage} : \mathbb{N} 
      \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)} 
      \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)}$ 
explicitly then the structure in Figure \ref{fig:fft-top} becomes Figure \ref{fig:fft-stage}, where the stage width  \texttt{w} is used to group the vectors that will enter the butterfly segments. We use the function \texttt{twiddles} to generate the vector sequence $\vect{0, \frac{2\pi}{L(vs)}, \frac{4\pi}{L(vs)}, \frac{6\pi}{L(vs)},...}$, mapping \texttt{halfcyle} over a vector of \texttt{indexes} $\vect{1,2,...,\lfloor \frac{L(vs)}{w} \rfloor}$.
\begin{lstlisting}
    stage     w         = V.concat . (segment `V.map21` (V.take (n `div` w) twiddles)) . V.group w
    twiddles            = (V.bitrev . (halfcycle `V.map11`) . V.take (n `div` 2)) V.indexes
      where halfcycle l = (cis . negate) (-2 * pi * (fromInteger (l - 1)) / (fromInteger n))
\end{lstlisting}
\begin{figure}[h]
  \centering
  \includegraphics[]{figs/fft_stage.pdf}
  \caption{The \texttt{fft} process network with explicit \texttt{stage}}
  \label{fig:fft-stage}
\end{figure}

Now let's make 
$\mathtt{segment} : \mathbb{C} 
      \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)} 
      \rightarrow \vect{\mathcal{S}(\mathbb{C}_e)}$ 
and 
$\mathtt{butterfly} : \mathbb{C} 
      \rightarrow \mathcal{S}(\mathbb{C}_e)^2 
      \rightarrow \mathcal{S}(\mathbb{C}_e)^2$ 
explicit. As you can see, \texttt{bffunc} is left out as an argument, so that we can pass it properly context-wrapped for a certain MoC. 
\begin{lstlisting}
    segment   twid = (V.unduals<>) . (<>)((butterfly twid) `V.map22`) . V.duals
    butterfly w    = ((bffunc w) `MoC.comb22`)
\end{lstlisting}%$
\begin{figure}[h]
  \centering
  \includegraphics[]{figs/fft_complete.pdf}
  \caption{The complete \texttt{fft} process network}
  \label{fig:fft-complete}
\end{figure}

Unfortunately, due to Haskell's strict type system and to several library design decisions, apart from a context wrapper, we \emph{do} need to define functions for timed and untimed MoCs differently. While timed MoCs require functions on values directly, untimed MoCs need to specify functions as operating on lists of values, as to mirror the partial order of events. Here is how we define the butterfly functions for timed and untimed MoCs:
\begin{code}
bffuncT w  x0   x1  = let t = w * x1 in ( x0 + t,   x0 - t)
bffuncU w [x0] [x1] = let t = w * x1 in ([x0 + t], [x0 - t])
\end{code}
and based on these definitions, here is how we properly wrap with a context and a behavior the butterfly function, according to the currently implemented MoCs, so that they fit as an argument to the \texttt{butterfly} process constructor:
\begin{code}
bffuncSY  w = SY.wrap22              (psi22 (\x0 x1 -> bffuncT w x0 x1))
bffuncDE  w = DE.wrap22              (psi22 (\x0 x1 -> bffuncT w x0 x1))
bffuncCT  w = CT.wrap22              (psi22 (\x0 x1 -> bffuncT w x0 x1))
bffuncSDF w = SDF.wrap22 (1,1) (1,1) (psi22 (\x0 x1 -> bffuncU w x0 x1))
\end{code}

So let us rewind: we have shown a functional description for the function \texttt{fft} which creates the process network in Figure \ref{fig:fft-complete}. This process network is oblivious to the MoC semantics it executes, but it does require a function properly wrapped with a context for instantiating a process constructor for a specific MoC. In this sense we can consider it a template describing the structure of an FFT butterfly network.

Further on, we need a testbench for our FFT. As input stage, we provide a signal sampler which feeds the FFT network with $2^k$ signals. To keep the design simple, the sampler does not buffer the samples, rather it streams them through a systolic array of delays, like in Figure \ref{fig:fft-sampler}.
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
samplerCT  = sampler (CT.delay 0.5 (\_->0))
samplerSDF = sampler (SDF.delay [0])
\end{code}

Finally, a sink function has been provided, which extracts the magnitude from the FFT results and synchronizes all output signals into one carrying vector events. 

\begin{code}
sinkSY  vs = SY.zipx $ (SY.comb11           magnitude)  `V.map11` vs 
sinkDE  vs = (DE.comb11           magnitude)  `V.map11` vs 
sinkCT  vs = (CT.comb11           magnitude)  `V.map11` vs 
sinkSDF vs = (SDF.comb11 (1,1,map magnitude)) `V.map11` vs
\end{code}

Putting it all together, the testbench and DUT for signals of different MoCs looks as following:
\begin{code}
testFFTSY  k = sinkSY  . fft bffuncSY  k . samplerSY  k
testFFTDE  k = sinkDE  . fft bffuncDE  k . samplerDE  k
testFFTCT  k = sinkCT  . fft bffuncCT  k . samplerCT  k
testFFTSDF k = sinkSDF . fft bffuncSDF k . samplerSDF k
\end{code}




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
