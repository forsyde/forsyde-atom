{-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK prune, show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Core.Utility.Plot
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015-2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module imports plotting and data dumping functions working
-- with "plottable" data types, i.e. instances of the 'Plot' and
-- 'Plottable' type classes.
-----------------------------------------------------------------------------

module ForSyDe.Atom.Utility.Plot (
  -- * User API

  -- | The following commands are frequently used as part of the
  -- normal modeling routine.
  
  -- ** Configuration settings
  Config(..), defaultCfg, silentCfg, noJunkCfg,

  -- ** Data preparation
  prepare, prepareL, prepareV,

  -- ** Dumping and plotting data
  showDat, dumpDat, plotGnu, heatmapGnu,
  showLatex, dumpLatex, plotLatex,
  
  -- * The data types

  -- | Below the data types involved are shown and the plottable
  -- structures are documented.

  Plottable(..), Plot(..), PInfo(..), Samples, PlotData
  
  ) where

import Control.Arrow
import Control.Exception
import Control.Monad (unless, when)
import Data.List (intercalate, intersperse, unwords)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Process

import qualified ForSyDe.Atom.ExB.Absent as AE (
  AbstExt(..))
import qualified ForSyDe.Atom.MoC.SY.Core as SY (
  Signal, SY(..), signal )
import qualified ForSyDe.Atom.MoC.DE.Core as DE (
  SignalBase, DE(..), signal )
import qualified ForSyDe.Atom.MoC.DE.React.Core as RE (
  SignalBase, RE(..), signal )
import qualified ForSyDe.Atom.MoC.CT.Core as CT (
  Signal, CT(..), signal, evalTs, tag)
import qualified ForSyDe.Atom.MoC.SDF.Core as SDF (
  Signal, SDF(..), signal)
import qualified ForSyDe.Atom.MoC.TimeStamp as Ts (
  TimeStamp )
import qualified ForSyDe.Atom.MoC.Time as T (
  Time )
import ForSyDe.Atom.MoC.Stream (
  Stream(..), fromStream, takeS)
import qualified ForSyDe.Atom.Skel.Vector as V (
  Vector(..), fromVector, take, vector)
import qualified ForSyDe.Atom.Prob as P (
  Histogram(..)
  )

-------------------------- TYPES --------------------------

-- | Record structure containing configuration settings for the
-- plotting commands.
data Config =
  Cfg { verbose :: Bool     -- ^ verbose printouts on terminal
      , path    :: String   -- ^ directory where all dumped files will be found
      , title    :: String  -- ^ base name for dumped files
      , rate    :: Float    -- ^ sample rate if relevant. Useful for explicit-tagged signals, ignored otherwise.
      , xmax    :: Float    -- ^ Maximum X coordinate. Mandatory for infinite structures, optional otherwise.
      , labels  :: [String] -- ^ list of labels with the names of the structures plotted
      , fire    :: Bool     -- ^ if relevant, fires a plotting or compiling program.
      , other   :: Bool     -- ^ if relevant, dumps additional scripts and plots.
      } deriving (Show)

-- | Default configuration: verbose, dump everything possible, fire
-- whatever program needed. Check source for settings.
--
-- Example usage:
--
-- >>> defaultCfg {xmax = 15, verbose = False, labels = ["john","doe"]}
-- Cfg {verbose = False, path = "./fig", title = "plot", rate = 1.0e-2, xmax = 15.0, labels = ["john","doe"], fire = True, other = True}
defaultCfg = Cfg { path    = "./fig"
                 , title    = "plot"
                 , rate    = 0.01
                 , xmax    = 200
                 , labels  = replicate 10 ""
                 , verbose = True
                 , fire    = True
                 , other   = True
                 }

-- | Silent configuration: does not fire any program or print our
-- unnecessary info. Check source for settings.
silentCfg = Cfg  { path    = "./fig"
                 , title  = "plot"
                 , rate    = 0.01
                 , xmax    = 200
                 , labels  = replicate 10 ""
                 , verbose = False
                 , fire    = False
                 , other   = True
                 }

-- | Clean configuration: verbose, does not dump more than necessary,
-- fire whatever program needed. Check source for settings.
noJunkCfg = Cfg  { path    = "./fig"
                 , title  = "plot"
                 , rate    = 0.01
                 , xmax    = 200
                 , labels  = repeat ""
                 , verbose = True
                 , fire    = True
                 , other   = False
                 }


-- | Static information of each plottable data type.
data PInfo = Info { typeid  :: String  -- ^ id used usually in implicit tags
                  , command :: String  -- ^ LaTeX identifier
                  , measure :: String  -- ^ unit of measure
                  , style   :: String  -- ^ style tweaking in the GNUplot script
                  , stacking:: Bool    -- ^ if the plot is stacking
                  , sparse  :: Bool    -- ^ if the sampled data is sparse instead of dense
                  } deriving (Show)

-- | Alias for sampled data 
type Samples  = [(String, String)]

-- | Alias for a data set 'prepare'd to be plotted.
type PlotData = (Config, PInfo, [(String,Samples)])

-------------------------- CLASSES --------------------------

-- | This class gathers all ForSyDe-Atom structures that can be
-- plotted.
class Plot a where
  {-# MINIMAL (sample | sample') , takeUntil, getInfo #-}
  -- | Samples the data according to a given step size.
  sample    :: Float -> a -> Samples
  sample _   = sample'
  ------------------------
  -- | Samples the data according to the internal structure.
  sample'   :: a -> Samples
  sample'   = sample 0.00001
  ------------------------
  -- | Takes the first samples until a given tag.
  takeUntil :: Float -> a -> a
  ------------------------
  -- | Returns static information about the data type.
  getInfo   :: a -> PInfo
  ------------------------

-- | This class gathers types which can be sampled and converted to a
-- numerical string which can be read and interpreted by a plotter
-- engine.
class Plottable a where
  -- | Transforms the input type into a coordinate string.
  toCoord :: a -> String

-------------------------- INSTANCES --------------------------

-- | Time stamps
instance {-# OVERLAPPING #-} Plottable Ts.TimeStamp where
  toCoord = init . show

-- | Absent-extended plottable types
instance (Show a, Plottable a) => Plottable (AE.AbstExt a) where
  -- toCoord = show
  toCoord AE.Abst     = "_"
  toCoord (AE.Prst a) = toCoord a

-- | Vectors of plottable types
instance (Plottable a) => Plottable (V.Vector a) where
  toCoord = (++) "<" . unwords . map toCoord . V.fromVector 
  -- toCoord = concat . map (\v -> (show $ realToFrac v) ++ " ") .
  --           V.fromVector

-- | Real numbers that can be converted to a floating point representation
instance {-# OVERLAPPABLE #-} (Show a, Real a) => Plottable a where
  toCoord = show . realToFrac

-- | For plotting 'ForSyDe.Atom.MoC.SDF.SDF' signals.
instance Plottable a => Plot (SDF.Signal a) where
  sample' = zip (map show [0..]) . fromStream . fmap vToSamp
    where vToSamp (SDF.SDF a) = toCoord a
  ------------------------
  takeUntil n = takeS (truncate n)
  ------------------------
  getInfo _ = Info { typeid   = "sig-sdf"
                   , command  = "SY"
                   , measure  = "token"
                   , style    = "impulses lw 3"
                   , stacking = True
                   , sparse   = False
                   }

-- | 'ForSyDe.Atom.MoC.SY.SY' signals.
instance Plottable a => Plot (SY.Signal a) where
  sample' = zip (map show [0..]) . fromStream . fmap vToSamp
    where vToSamp (SY.SY a) = toCoord a
  ------------------------
  takeUntil n = takeS (truncate n)
  ------------------------
  getInfo _ = Info { typeid   = "sig-sy"
                   , command  = "SY"
                   , measure  = "sample"
                   , style    = "impulses lw 3"
                   , stacking = True
                   , sparse   = False
                   }

-- | For plotting 'ForSyDe.Atom.MoC.DE.DE' signals.
instance (Plottable a, Show t, Real t, Fractional t, Num t, Ord t, Eq t) =>
  Plot (DE.SignalBase t a) where
  sample' = map v2s . fromStream
    where v2s (DE.DE t v) = (toCoord t, toCoord v)
  -- sample' sig = concat $ zipWith v2s ((head lst):lst) lst 
  --   where lst = fromStream sig
  --         v2s (DE.DE pt pv) (DE.DE t v)
  --           = [(toCoord t, toCoord pv), (toCoord t, toCoord v)]
  ------------------------
  takeUntil n = until (realToFrac n)
    where until _ NullS = NullS
          until u (DE.DE t v:-NullS)
            | t < u     = DE.DE t v :- DE.DE u v :- NullS
            | otherwise = DE.DE u v :- NullS
          until u (DE.DE t v:-xs)
            | t < u     = DE.DE t v :- until u xs
            | otherwise = DE.DE u v :- NullS
  ------------------------
  getInfo _ = Info { typeid   = "sig-de"
                   , command  = "DE"
                   , measure  = "timestamp"
                   , style    = "lines lw 2"
                   , stacking = False
                   , sparse   = True
                   }

-- | For plotting 'ForSyDe.Atom.MoC.RE.React.RE' signals.
instance (Plottable a, Show t, Real t, Fractional t, Num t, Ord t, Eq t) =>
  Plot (RE.SignalBase t a) where
  sample' = map v2s . fromStream
    where v2s (RE.RE t v) = (toCoord t, toCoord v)
  -- sample' sig = concat $ zipWith v2s ((head lst):lst) lst 
  --   where lst = fromStream sig
  --         v2s (RE.RE pt pv) (RE.RE t v)
  --           = [(toCoord t, toCoord pv), (toCoord t, toCoord v)]
  ------------------------
  takeUntil n = until (realToFrac n)
    where until _ NullS = NullS
          until u (RE.RE t v:-NullS)
            | t < u     = RE.RE t v :- RE.RE u v :- NullS
            | otherwise = RE.RE u v :- NullS
          until u (RE.RE t v:-xs)
            | t < u     = RE.RE t v :- until u xs
            | otherwise = RE.RE u v :- NullS
  ------------------------
  getInfo _ = Info { typeid   = "sig-de"
                   , command  = "RE"
                   , measure  = "timestamp"
                   , style    = "lines lw 2"
                   , stacking = False
                   , sparse   = True
                   }


-- | For plotting 'ForSyDe.Atom.MoC.CT.CT' signals.
instance (Plottable a) => Plot (CT.Signal a) where
  sample stepsize = evalSamples 0
    where evalSamples t s@(x:-y:-xs)
            | CT.tag y <= t  = evalSamples t (y:-xs)
            | otherwise      = (toCoord t,
                                toCoord $ CT.evalTs t x) :
                               evalSamples (t + step) s
          evalSamples _ (_:-NullS) = []
          evalSamples _ NullS      = []
          step = realToFrac stepsize
  ------------------------
  takeUntil n = until (realToFrac n)
    where until _ NullS = NullS
          until u (CT.CT t p f:-NullS)
            | t < u     = CT.CT t p f :- CT.CT u p f :- NullS
            | otherwise = CT.CT u p f :- NullS
          until u (CT.CT t p f:-xs)
            | t < u     = CT.CT t p f :- until u xs
            | otherwise = CT.CT u p f :- NullS
  ------------------------
  getInfo _ = Info { typeid   = "sig-ct"
                   , command  = "CT"
                   , measure  = "time (s)"
                   , style    = "lines"
                   , stacking = False
                   , sparse   = False
                   }

-- | For plotting vectors of coordinates
instance Plottable a => Plot (V.Vector a) where
  sample' = zip (map show [1..]) . V.fromVector . fmap toCoord
  ------------------------
  takeUntil n = V.take (truncate n)
  ------------------------
  getInfo _ = Info { typeid   = "vect"
                   , command  = "NONE"
                   , measure  = "index"
                   , style    = "impulses lw 3"
                   , stacking = True
                   , sparse   = False
                   }

-- | For plotting vectors of coordinates
instance Plot P.Histogram where
  sample' = map (toCoord *** toCoord) . P.getBins
  ------------------------
  takeUntil n = P.Hist . take (truncate n) . P.getBins
  ------------------------
  getInfo _ = Info { typeid   = "hist"
                   , command  = "HIST"
                   , measure  = "bin"
                   , style    = "boxes"
                   , stacking = True
                   , sparse   = False
                   }

-------------------------- PREPARE --------------------------

-- | Prepares a single plottable data structure to be dumped and/or
-- plotted.
prepare :: (Plot a)
        => Config   -- ^ configuration settings
        -> a        -- ^ plottable data type
        -> PlotData -- ^ structure ready for dumping
prepare cfg = prepareL cfg . (:[])

-- | Prepares a vector of plottable data structures to be dumped
-- and/or plotted. See 'prepare'.
prepareV :: (Plot a) => Config -> V.Vector a -> PlotData
prepareV cfg = prepareL cfg . V.fromVector
                 
-- | Prepares a list of plottable data structures to be dumped and/or
-- plotted. See 'prepare'.
prepareL :: (Plot a) => Config -> [a] -> PlotData
prepareL cfg x = (cfg, getInfo (head x), zipWith3 prep [1..] lbls x)
  where prep  i l  s = (mkLbl i l s, sample sr $ takeUntil supx s)
        mkLbl i "" s = typeid (getInfo s) ++ show i
        mkLbl _ l  _ = l  
        -- extract settings
        lbls = labels cfg
        sr   = rate   cfg
        supx = xmax   cfg

-------------------------- SAMPLE DATA --------------------------

-- | Prints out the sampled contents of a 'prepare'd data set.
showDat :: PlotData -> IO ()
showDat (_,_,pdata) = putStrLn $ intercalate "\n\n" $ map showD pdata
  where
    showD (label,samp) = label ++ " = \n"
                         ++ intercalate "\n" (map showS samp)
    showS (tag,value)  = "\t" ++ tag ++ "\t" ++ value
        
-- | Dumps the sampled contents of a 'prepare'd data set into separate
-- @.dat@ files.
dumpDat :: PlotData -> IO [String]
dumpDat (cfg, _, pdata) = do
  createDirectoryIfMissing True dpath
  files <- mapM dump pdata
  when verb $ putStrLn ("Dumped " ++ allLabels ++ " in " ++ dpath)
  return files
  where
    dump (lbl,samp)  = let name = mkFileNm lbl
                       in do writeFile name (dumpSamp samp)
                             return name
    mkFileNm label = dpath ++ "/" ++ replChar "$<>{}" '_' label ++ ".dat"
    dumpSamp = concatMap (\(x,y) -> x ++"    "++ y ++ "\n")
    allLabels= drop 2 $ foldl (\s (l,_)-> s ++ ", " ++ l) "" pdata
    -- extract settings
    dpath    = path    cfg
    verb     = verbose cfg

-------------------------- GNUPLOT --------------------------

-- | Generates a GNUplot script and @.dat@ files for plotting the
-- sampled contents of a 'prepare'd data set. Depending on the
-- configuration settings, it also dumps LaTeX and PDF plots, and
-- fires the script.
--
-- __OBS:__ needless to say that <http://www.gnuplot.info/ GNUplot>
-- needs to be installed in order to use this command. Also, in order
-- to fire GNUplot from a ghci session you might need to install
-- @gnuplot-x11@.
plotGnu :: PlotData -> IO ()
plotGnu pdata@(cfg,info,samps) = do
  datFiles <- dumpDat $ alterForGnuPlot pdata
  -- Write the gnuplot title to a file; Try several times to be able
  -- to open multiple plots in the same session
  script <- tryNTimes 10 basename $ writePlotScript datFiles
  _ <- if fireGnuplot then system ("gnuplot -persist " ++ script)
       else return ExitSuccess
  when isVerbose   $ putStrLn ("Signal(s) " ++ allLabels ++ " plotted.")
  where
    writePlotScript dat f = writeFile f $ mkPlotScript cfg info dat
    allLabels = drop 2 $ foldl (\s (l,_)-> s ++ ", " ++ l) "" samps
    -- extract settings
    fireGnuplot = fire cfg
    isVerbose   = verbose cfg
    basename    = path cfg ++ "/" ++ title cfg

-- | Similar to 'plotGnu' but creates a heatmap plot using the GNUplot
-- engine. For this, the input needs to contain at least two columns
-- of data, otherwise the plot does not show anything, i.e. the
-- samples need to be lists or vectors of two or more elements.
--
-- __OBS:__ same dependencies are needed as for 'plotGnu'.
heatmapGnu :: PlotData -> IO ()
heatmapGnu pdata@(cfg,info,samps) = do
  datFiles <- dumpDat $ alterForGnuHeatmap pdata
  script <- tryNTimes 10 basename $ writeHeatmapScript datFiles
  _ <- if fireGnuplot then system ("gnuplot -persist " ++ script)
       else return ExitSuccess
  when isVerbose   $ putStrLn ("Signal(s) " ++ allLabels ++ " plotted"
                              ++ " as heatmaps.")
  where
    writeHeatmapScript dat f = writeFile f $ mkHeatmapScript cfg info dat
    allLabels = drop 2 $ foldl (\s (l,_)-> s ++ ", " ++ l) "" samps
    -- extract settings
    fireGnuplot = fire cfg
    isVerbose   = verbose cfg
    basename    = path cfg ++ "/" ++ title cfg ++ "-heat"

----------- not exported -----------

alterForGnuPlot :: PlotData -> PlotData
alterForGnuPlot (cfg,info,lsamp) = (cfg, info, map alter lsamp)
  where
    alter (label, samp)
      | sparse info   = (label, map handler $ mkDe samp)
      | otherwise     = (label, map handler samp)
    mkDe samp@(fs:_)  =  concat $ zipWith dup (fs:samp) samp
    dup (pt,pv) (t,v) = [(t,pv), (t,v)]
    handler (t,"_")   = (t,"\n")
    handler (t,v)
      | head v == '<' = (t, tail v)
      | otherwise     = (t, v)

alterForGnuHeatmap :: PlotData -> PlotData
alterForGnuHeatmap (cfg,info,lsamp) = (cfg, info, map alter lsamp)
  where
    alter (label, samp)
      | sparse info   = (label, map noTag $ mkDe $ map handler samp)
      | otherwise     = (label, map (noTag . handler) samp)
    mkDe []           = []
    mkDe ((t,v):[])   = [(t,v)]
    mkDe ((t,v):(ft,fv):xs)
      | itfl >= ftfl  = (t,v) : mkDe ((ft,fv):xs)
      | otherwise     = (t,v) : mkDe ((show itfl,v):(ft,fv):xs)
      where itfl      =(read t::Float) + (rate cfg)
            ftfl      = read ft::Float
    noTag (_,v)       = ("",v)
    handler (t,"_")   = (t,"0")
    handler (t,v)
      | head v == '<' = (t, tail v)
      | otherwise     = (t, v)

mkPlotScript :: Config -> PInfo -> [FilePath] -> String 
mkPlotScript cfg info files =
  (if plotTitle == "plot" then "" else "set title \"" ++ plotTitle ++  "\"\n")
  ++ "set xlabel \"" ++ unitOfMeasure ++ "\" \n"
  ++ (if command info == "HIST" then
       "set style fill solid\nset boxwidth 0.5\n"
     else "")
  ++ "set xzeroaxis\n"
  ++ "plot " ++ plotCmds ++ "\n"
  ++ epsCmd ++ latexCmd ++ pdfCmd
  where
    plotCmds = intercalate ",\\\n    " $ zipWith3 pCmd [0..] files plotLb
    pCmd i f l = "\t\"" ++ f  ++ "\"" ++ stackCmd i ++ " with "
                 ++ plotStyle ++ " title \""++ l ++ "\""
    stackCmd i = if isStacking then
                   " using ($1+0." ++ show i ++ "):2"
                 else ""
    epsCmd     = if plotOther then
                   "set  terminal postscript eps color\n"
                   ++ "set output \"" ++ plotName ++".eps\"\n"
                   ++ "replot \n"
                 else ""
    latexCmd   = if plotOther then
                   "set terminal epslatex color\n"
                   ++ "set output \"" ++ plotName ++"-latex.eps\"\n"
                   ++ "replot\n"
                 else ""
    pdfCmd     = if plotOther then
                   "set terminal pdf\n"
                   ++ "set output \"" ++ plotName ++".pdf\"\n"
                   ++ "replot\n"
                 else ""
    plotName   = plotPath ++ "/" ++ plotId ++ "-" ++ plotTitle
    -- extract styles
    unitOfMeasure = measure  info
    plotStyle     = style    info
    plotId        = typeid   info
    isStacking    = stacking info
    -- extract settings
    plotPath   = path    cfg
    plotTitle  = title   cfg
    plotOther  = other   cfg
    plotLb     = labels  cfg

mkHeatmapScript :: Config -> PInfo -> [FilePath] -> String 
mkHeatmapScript cfg info files =
  (if plotTitle == "plot" then "" else "set title \"" ++ plotTitle ++  "\"\n")
  ++ "set xlabel \"index\" \n"
  ++ "set ylabel \"" ++ unitOfMeasure ++ "\" \n"
  ++ "set yrange [-0.5" ++ scale ++ ":" ++ plotXmax
  ++ "+0.5" ++ scale ++ "]\n"
  ++ "set palette rgbformula -7,2,-7\n"
  ++ "set multiplot layout 1," ++ show (length files) ++ "\n"
  ++ plotCmds ++ "\n"
  ++ "unset multiplot\n"
  ++ epsCmd ++ latexCmd ++ pdfCmd
  where
    plotCmds = intercalate ",\n" $ zipWith3 pCmd [0..] files plotLb
    pCmd i f l = "set title \"" ++ l ++ "\"\n"
                 ++ "plot \"" ++ f  ++ "\" matrix "
                 ++ "using 1:" ++ scaley ++ ":3 "
                 ++ "with image title \"\""
    epsCmd     = if plotOther then
                   "set  terminal postscript eps color\n"
                   ++ "set output \"" ++ plotName ++".eps\"\n"
                   ++ "replot \n"
                 else ""
    latexCmd   = if plotOther then
                   "set terminal epslatex color\n"
                   ++ "set output \"" ++ plotName ++"-latex.eps\"\n"
                   ++ "replot\n"
                 else ""
    pdfCmd     = if plotOther then
                   "set terminal pdf\n"
                   ++ "set output \"" ++ plotName ++".pdf\"\n"
                   ++ "replot\n"
                 else ""
    plotName   = plotPath ++ "/" ++ plotId ++ "-" ++ plotTitle
    -- extract styles
    unitOfMeasure = measure  info
    plotStyle     = style    info
    plotId        = typeid   info
    isStacking    = stacking info
    isSparse      = sparse   info
    scale         = if isSparse then "*" ++ plotRate else ""
    scaley        = if isSparse then "($2*"++ plotRate ++ ")" else "2"
    -- extract settings
    plotPath   = path    cfg
    plotTitle  = title   cfg
    plotOther  = other   cfg
    plotLb     = labels  cfg
    plotXmax   = show $ xmax cfg
    plotRate   = show $ rate cfg


-------------------------- LATEX --------------------------

-- | Prints out a LaTeX environment from a 'prepare'd data set. This
-- environment should be paste inside a @tikzpicture@ in a document
-- title which imports the ForSyDe-LaTeX package.
showLatex :: PlotData -> IO ()
showLatex pdata = putStrLn $ mkLatex pdata

-- | Dumps a set of formatted data files with the extension @.flx@
-- that can be imported by a LaTeX document which uses the
-- ForSyDe-LaTeX package.
dumpLatex :: PlotData -> IO [String]
dumpLatex (cfg, _, pdata) = do
  createDirectoryIfMissing True dpath
  files <- mapM dump pdata
  when verb $ putStrLn ("Dumped " ++ allLabels ++ " in " ++ dpath)
  return files
  where
    dump (lbl,samp)  = let name = mkFileNm lbl
                       in do writeFile name (dumpSamp samp)
                             return name
    mkFileNm label = dpath ++ "/" ++ replChar "$<>{}" '_' label ++ ".flx"
    dumpSamp = intercalate ",\n" . map (\(x,y) -> y ++" : "++ x)
    allLabels= drop 2 $ foldl (\s (l,_)-> s ++ ", " ++ l) "" pdata
    -- extract settings
    dpath    = path    cfg
    verb     = verbose cfg

-- | Creates a standalone LaTeX document which uses the ForSyDe-LaTeX
-- package, plotting a 'prepare'd data set. Depending on the
-- configuration settings, the command @pdflatex@ may also be invoked
-- to compile a pdf image.
--
-- __OBS:__ A LaTeX compiler is required to run the @pdflatex@
-- command. The <https://github.com/forsyde/forsyde-latex ForSyDe-LaTeX>
-- package also needs to be installed according to the instructions on
-- the project web page.
plotLatex :: PlotData -> IO ()
plotLatex pdata@(cfg,_,_) = do
  createDirectoryIfMissing True filepath
  writeFile filename $ mkLatexFile $ mkLatex pdata
  when isVerbose $ putStrLn ("Dumped LaTeX title " ++ filename)
  _ <- if fireLatex then
         system ("pdflatex -output-directory=" ++ filepath
                 ++ " " ++ filename)
       else return ExitSuccess
  when (isVerbose && fireLatex) $ putStrLn ("Compiled PDF in " ++ filepath)
  where
    -- extract settings
    isVerbose = verbose cfg
    filename  = path cfg ++ "/" ++ title cfg ++ ".tex"
    filepath  = path cfg
    fireLatex = fire cfg
  
----------- not exported -----------

alterForLatex :: PlotData -> PlotData
alterForLatex (cfg,info,lsamp) = (cfg, info, map alter lsamp)
  where
    alter (label, samp) = (label, map handler samp)
    handler (t,"_")     = (t,"$\\bot$")
    handler (t,v)
      | head v == '<' = (t, "$\\langle$ "
                            ++ tail v
                            ++ " $\\rangle$")
      | otherwise     = (t, v)
                            
mkLatex :: PlotData -> String
mkLatex = latexCmd . alterForLatex 
  where
    latexCmd (cfg, info, lsamp)
      -- SIGNAL plots
      | command info `elem` ["SY","DE","RE","CT"] =
        "  \\begin{signals" ++ mocStr ++ "}[]{"
        ++ lastX ++ "}\n"
        ++ concatMap toSignal lsamp
        ++ "  \\end{signals" ++ mocStr ++ "}\n"
      -- HISTOGRAM plots
      | command info `elem` ["HIST"] =
        "  \\begin{axis}[ybar,ytick=\\empty,axis x line*=bottom,axis y line*=left]\n"
        ++ concatMap toPlot lsamp
        ++ "\n  \\end{axis}\n"
      | otherwise = error "mkLatex: plot for this type not implemented."
      where
        ------ SIGNAL helpers ------  
        toSignal (label,samp) =
          "    \\signal" ++ mocStr ++ "[name= " ++ label ++ "]{"
          ++ intercalate "," (map showEvent samp) ++ "}\n"
        showEvent (t,v) = v ++ ":" ++ t
        mocStr = command info
        lastX  = show $ xmax cfg
        ------ HISTOGRAM helpers ------  
        toPlot (_,sp) = "  \\addplot coordinates {" ++ concatMap showBin sp ++ "};"
        showBin (t,v) = "(" ++ v ++ "," ++ t ++ ")"
       
mkLatexFile :: String -> String
mkLatexFile cmd =
  "\\documentclass{standalone}\n"
  ++ "\\usepackage[plot]{forsyde}\n"
  ++ "\\begin{document}\n"
  ++ "\\begin{tikzpicture}[]\n"
  ++ cmd
  ++ "\\end{tikzpicture}\n"
  ++ "\\end{document}\n"

-------------------------- UTILITIES --------------------------

replChar :: String -- all characters in this set are replaced by '_'
         -> Char   -- Char to replace with
         -> String -- the string where characters are replaced
         -> String -- the result string with all characters replaced
replChar [] _ s  = s
replChar _  _ [] = []
replChar rSet rCh (c:s) | c `elem` rSet = rCh : replChar rSet rCh s
                        | otherwise     = c   : replChar rSet rCh s


tryNTimes :: Int -> String -> (String -> IO ()) -> IO String
tryNTimes n base a
  | n <= 0 = error "tryNTimes: not succedded"
  | n > 0  = catch (action fname a) (handler a)
  where handler :: (String -> IO()) -> IOError -> IO String
        handler a _ = tryNTimes (n-1) base a
        fname = base ++ show n ++ ".gnuplot"
        action :: String -> (String -> IO ()) -> IO String
        action fname a = do a fname
                            return fname
tryNTimes _ _ _ = error "tryNTimes: Unexpected pattern."

