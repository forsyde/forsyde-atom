forsyde-atom
============

[![Build Status](https://travis-ci.org/forsyde/forsyde-atom.svg?branch=master)](https://travis-ci.org/forsyde/forsyde-atom)

Reimplementation of the ForSyDe-Haskell modelling library in an
applicative manner using the concept of atoms, and a layered process
model. For a technical report, check the extended API
documentation at [forsyde.github.io/forsyde-atom](https://forsyde.github.io/forsyde-atom),
which was generated with Haddock.

---- 

Ideas based on the projects `forsyde-patterns` (George
Ungureanu), `forsyde-appl` (Mikkel Jakobsen) and the `ForSyDe. library
(KTH-SAM group)


Installation and usage
----------------------

This package is *cabal*ized, thus one shoud use `cabal` to install
it. It is recommended to install and test inside a sandbox:

    cabal sandbox init    # initializes the sandbox
    cabal install --dependencies-only # installs dependencies inside the sandbox
    cabal configure       # condfigures the package
    cabal install         # installs the library inside the sandbox
    cabal repl            # starts GHCi with the sandbox loaded
    cabal sandbox delete  # deletes the sandbox and everything in it
    
Generating the documentation
----------------------------

A haddock extended API + technical report documentation can be generated from the 
version found in the `docs` branch. To do this, you should should use the provided 
`Makefile`:

    make doc              # compiles the documentation and the LaTeX figures
    make clean            # removes all the generated documentation files
    
General dependencies
--------------------
    
There are a number of dependencies to be taken care of:

 * Haskell dependencies: check `forsyde-atom.cabal` for the libraries
   and compilers necessary

 * `git`: for setting up the dependent Haskell libraries in the sandbox.

 * `haddock` for generating the documentation

 * a LaTeX compiler (`pdflatex`) for generating the pictures inside
   the documentation.

 * `imagemagick` to convert the above PDF pictures to PNG to be
   recognized by Haddock.
   
On a Ubuntu 16.04 OS, the following commands might help for installing these dependencies:

    # To be able to clone this repository
    sudo apt-get install -y git
    
    # For the latest GHC compiler 
    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt-get update
    sudo apt-get install cabal-install-1.24 ghc-8.0.1
    export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH
    cabal update
    
    # To be able to generate the documentation, in case a LaTeX installation is not available:
    sudo apt-get install -y texlive texlive-latex-extra texlive-fonts-extra texlive-math-extra
    sudo apt-get install -y imagemagick
    
