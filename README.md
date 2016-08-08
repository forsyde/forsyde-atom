forsyde-atom
============

[![Build Status](https://travis-ci.org/forsyde/forsyde-atom.svg?branch=dev-untracked)](https://travis-ci.org/forsyde/forsyde-atom)

Reimplementation of the ForSyDe-Haskell modelling library in an
applicative manner using the concept of atoms, and a layered process
model.

---- 

Ideas based on the projects `forsyde-patterns` (George
Ungureanu), `forsyde-appl` (Mikkel Jakobsen) and the `ForSyDe. library
(KTH-SAM group)


Installation and usage
----------------------

The project is equipped with a makefile. To install the Haskell
library inside a sandbox type:

    make sandbox

To generate the haddock extended API + technical report documentation
type:

    make doc

To clean the documentation and the intermediate files generated for it
type:

    make clean
    
To delete the sandbox, use `cabal` instead:

    cabal sandbox delete

There are a number of dependencies to be taken care of:

 * Haskell dependencies: check `forsyde-atom.cabal` for the libraries
   and compilers necessary

 * `git`: for setting up the dependent Haskell libraries in the sandbox.

 * `haddock` for generating the documentation

 * a LaTeX compiler (`pdflatex`) for generating the pictures inside
   the documentation.

 * `imagemagick` to convert the above PDF pictures to PNG to be
   recognized by Haddock.
