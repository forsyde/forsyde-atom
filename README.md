forsyde-atom
============

[![Build Status](https://travis-ci.org/forsyde/forsyde-atom.svg?branch=master)](https://travis-ci.org/forsyde/forsyde-atom)

Reimplementation of the ForSyDe-Haskell modelling library in an
applicative manner using the concepts of atoms and the layered process
model. For a technical report, check the extended API documentation at
[forsyde.github.io/forsyde-atom](https://forsyde.github.io/forsyde-atom),
which was generated with Haddock.

----

**OBS:** the examples and case studies are being migrated to the 
[forsye-atom-examples](https://github.com/forsyde/forsyde-atom-examples) 
repository. Please refer to that repository for further reports and 
instructions. 

----

Ideas based on the projects `forsyde-patterns` (George Ungureanu),
`forsyde-appl` (Mikkel Jakobsen) and the `ForSyDe. library (KTH-SAM
group)


Installation and usage
----------------------

### The `forsyde-atom` libraries

This package is *cabal*ized, thus one shoud use `cabal` to install
it. It is recommended to install and test inside a sandbox:

    cabal sandbox init                # initializes the sandbox
    cabal configure                   # configures the package
    cabal install --dependencies-only # (optional) installs dependencies inside the sandbox
    cabal install                     # installs the library inside the sandbox
    cabal repl                        # starts GHCi with the sandbox loaded
    cabal sandbox delete              # deletes the sandbox and everything in it
	
### The test suite

The test suite and its dependencies can be installed and run by
explicitly adding the flag `--enable-tests` to the previous
installation commands, namely:

    cabal configure --enable-tests 
    # cabal install --enable-tests --dependencies-only
    cabal install --enable-tests
    cabal test                        # runs the test suite 
   
### Generating the documentation

A haddock extended API + technical report documentation can be
generated from the version found in the `docs` branch. It contains an
additional folder called `docfiles` in the project root, which holds
`LaTeX` sources for the figures and equations in the API
documentation.

To generate the documentation, you should should use the provided
`Makefile`:

    cd docfiles           # here the Makefile is found
    make haddock          # compiles the LaTeX figures and the documentation
    make clean            # removes all generated LaTeX files
	make superclean       # removes generated files including the figures
    
Pay attention as the `docs` branch might lag behind the current
development and it is recommended to check the project history.
	
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
   
 * the package
   [`forsyde-latex`](https://github.com/forsyde/forsyde-latex), as
   most figures are dependent on it. It needs to be manually installed
   using the instructions on the project web page.
   
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
    
