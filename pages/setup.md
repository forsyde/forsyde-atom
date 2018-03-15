---
layout: default
title: Installation and usage
permalink: setup.html
---

# Dependencies

Based on how you are planning to use the library and for what purpose, there is a number of dependencies to take care of before installing:

### General dependencies

These are required to acquire, install and use the base libraries:

 * The [Haskell Platform](https://www.haskell.org/platform/). The libraries usually support the latest `ghc` versions, but you can check the tested versions in the   [`forsyde-atom.cabal`](https://github.com/forsyde/forsyde-atom/blob/master/forsyde-atom.cabal) file in case the installation does not succeed.
 
 * [Git](https://git-scm.com/downloads) if you want to clone the whole repository, and not just download the sources. 
 
Library dependencies are taken care of by the [Cabal](https://www.haskell.org/cabal/) package manager shipped with [Haskell Platform](https://www.haskell.org/platform/).

### Plotting the signals

If you intend to use the [ForSyDe.Atom.Utility.Plot](api/ForSyDe-Atom-Utility-Plot.html) library features to plot signals instead of printing them to the terminal output, then you need to install the respectve plotting engines and their dependencies.

##### Using Gnuplot

[ForSyDe.Atom.Utility.Plot](api/ForSyDe-Atom-Utility-Plot.html) provides helper functions for plotting signals using the [gnuplot](http://gnuplot.info/) engine. Naturally, you need to have it installed for your OS.

For an OS using the [X Window System](https://en.wikipedia.org/wiki/X_Window_System) e.g. Ubuntu Linux, you might need to install the [`gnuplot-x11`](http://gnuplot.sourceforge.net/docs_4.2/node442.html) library, to be able to launch plots from an interpreter session. For example, in Ubuntu, you need to type in:

    sudo apt instal gnuplot-x11
	
##### Using ForSyDe-LaTeX

[ForSyDe.Atom.Utility.Plot](api/ForSyDe-Atom-Utility-Plot.html) also provides helper functions to generate LaTeX files using the [ForSyDe-LaTeX](https://forsyde.github.io/forsyde-latex/) style package. To be able to compile them you need:

 * a LaTeX compiler suite. We have tested and recommend [TexLive](https://www.tug.org/texlive/). 
 
 * the [ForSyDe-LaTeX](https://forsyde.github.io/forsyde-latex/) package. To acquire it, you need to follow the [installation instructions](https://forsyde.github.io/forsyde-latex/#installation).

# Installation

Before trying to install, check the list of [dependencies](#dependencies) above, to see that you meet the requirements based on how you are planning to use the library.

### The ForSyDe-Atom libraries

This package is *cabal*ized, thus one shoud use `cabal` to install
it. It is recommended to install and test inside a sandbox:

    cabal update                      # downloads the most recent list of packages from the Hackage repository
    cabal sandbox init                # initializes the sandbox
    cabal install --dependencies-only # (optional) installs only dependencies inside the sandbox
    cabal install                     # installs the library inside the sandbox
    cabal configure                   # configures the package
	
Now the `forsyde-atom` package should be installed in a sandbox in the current path. To open an interpreter session with the default ForSyDe-Atom libraries loaded, run the command:
	
    cabal repl                        # starts GHCi with the sandbox loaded	

For a quick test that everything works fine, you can try the following example inside the interpreter session. The example implements a Moore finite state machine that calculates the running sum and multiplies the output with 2.

    *ForSyDe.Atom> import ForSyDe.Atom.MoC.SY as SY
    *ForSyDe.Atom SY> let s = SY.signal [1..4]
    *ForSyDe.Atom SY> SY.moore11 (+) (*2) 0 s
	{0,2,6,12,20}

For more examples, please check the [user manual](assets/manual.pdf) generated from the [`forsyde-atom-examples`](https://github.com/forsyde/forsyde-atom-examples) project.

To uninstall the library and everything that was generated, you can type in:

    cabal sandbox delete              # deletes the sandbox and everything in it
	
### Testing the libraries

The test suite and its dependencies can be installed and run by
explicitly adding the flag `--enable-tests` to the previous
installation commands, namely:

    cabal install --enable-tests --dependencies-only # (optional)
    cabal install --enable-tests                     # installs library + test suite
    cabal configure --enable-tests                   # configures the package to run the test suite
    cabal test                                       # runs the test suite 
   
   
   
# Generating the API documentation locally

The API documentation for latest release is publicly available [here](api/) but if for some reason you need to generate it locally on your machine you need perform the following steps:

 1. install the [`hscolour`](https://hackage.haskell.org/package/hscolour) Haskell package
     
	    cabal install hscolour
	 
 1. download the archive containing the figures from the documentation from [here](assets/docfig.zip) and unzip it in the same folder as the `forsyde-atom.cabal` file.
 
 1. open the `forsyde-atom.cabal` using your favourite text editor and uncomment the following line:
 
        -- extra-doc-files:     fig/*.png

 1. `cd` into the root of the project (the folder containing the `forsyde-atom.cabal` file) and type in:
     
	    cabal haddock --hyperlink-source


