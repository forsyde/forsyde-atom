forsyde-atom
============

[![Build Status](https://travis-ci.org/forsyde/forsyde-atom.svg?branch=master)](https://travis-ci.org/forsyde/forsyde-atom)

Reimplementation of the ForSyDe-Haskell modelling library in an
applicative manner using the concept of atoms, and a layered process
model. For an extended technical report, generate and check the API
documentation.

---- 

Ideas based on the projects `forsyde-patterns` (George
Ungureanu), `forsyde-appl` (Mikkel Jakobsen) and the `ForSyDe. library
(KTH-SAM group)


Installation and usage
----------------------

This package is _Cabal_ized, thus one shoud use `cabal` to install
it. It is recommended to install and test inside a sandbox:

    cabal sandbox init    # initializes the sandbox
    cabal install --dependencies-only # installs dependencies inside the sandbox
    cabal configure       # condfigures the package
    cabal install         # installs the library inside the sandbox
    cabal repl            # starts GHCi with the sandbox loaded
    cabal sandbox delete  # deletes the sandbox and everything in it

To generate the haddock extended API + technical report documentation
one should use the provided `Makefile` instead:

    make doc              # compiles the documentation and the LaTeX figures
    make clean            # removes all the generated documentation files
    
There are a number of dependencies to be taken care of:

 * Haskell dependencies: check `forsyde-atom.cabal` for the libraries
   and compilers necessary

 * `git`: for setting up the dependent Haskell libraries in the sandbox.

 * `haddock` for generating the documentation

 * a LaTeX compiler (`pdflatex`) for generating the pictures inside
   the documentation.

 * `imagemagick` to convert the above PDF pictures to PNG to be
   recognized by Haddock.
