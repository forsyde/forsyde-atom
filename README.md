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

Associated publications
-----------------------

George Ungureanu and Ingo Sander. 2017. A layered formal framework for modeling of cyber-physical systems. In _Proceedings of the Conference on Design, Automation & Test in Europe_ (DATE '17). Lausanne, Switzerland, March 2017, pp. 1715–1720. [ [slides](https://www.researchgate.net/publication/320004563_Slides_handout_from_DATE%2717_talk) | [doi](https://doi.org/10.23919/DATE.2017.7927270) | [bib](https://people.kth.se/~ugeorge/cite/publications.html#Ungureanu17:DATE) ]

 * case study report: from the [user manual](https://github.com/forsyde/forsyde-atom-examples/blob/master/manual.pdf), chapter _"Getting Started with ForSyDe-Atom"_, Section _"Toy example: a focus on MoCs"_
 * executable source: [forsyde-atom-examples/getting-started](https://github.com/forsyde/forsyde-atom-examples/tree/master/getting-started)

George Ungureanu, José E. G. de Medeiros and Ingo Sander. 2018. Bridging discrete and continuous time models with Atoms. In _Proceedings of the Conference on Design, Automation & Test in Europe_ (DATE '18). Dresden, Germany, March 2018 [ pre-print ]

 * case study report: from the [user manual](https://github.com/forsyde/forsyde-atom-examples/blob/master/manual.pdf), chapter _"Hybrid CT/DT Models in ForSyDe-Atom"_, Section _"RC Oscillator"_
 * executable source: [forsyde-atom-examples/hybrid](https://github.com/forsyde/forsyde-atom-examples/tree/master/hybrid)

Installation and usage
----------------------

### The `forsyde-atom` libraries

The package can be installed under an isolated sandbox using the [Stack](https://docs.haskellstack.org/en/stable/README/) program, or as a local *Nix project using the new [Cabal](https://www.haskell.org/cabal/users-guide/) build system.

#### Using Stack

For a completely hassle-free installation, we recommend using Stack, which acquires the right compliler version and every other dependency. For this, please install Stack [as recommended on their site](https://docs.haskellstack.org/en/stable/README/) (i.e. using `wget` or `curl`). Clone this repository and

	cd path/to/forsyde-atom
	stack install

#### Using Cabal

Make sure you have the newest [Haskell Platform](https://www.haskell.org/platform/) along with the newest [Cabal](https://www.haskell.org/cabal/) (version >= 3). For instructions on installing using legacy tools, check the [ForSyDe-Atom web page](forsyde.github.io/forsyde-atom).
	
	cd path/to/forsyde-atom
    cabal build

### Quick Start
	
Now the `forsyde-atom` package should be installed in a sandbox in the current path. Depending on how you installed the package you need to open an interpreter session with the default ForSyDe-Atom libraries loaded, by running the appropriate command:
    
    stack repl                        # starts GHCi with the sandbox loaded 

or

	cabal repl                        # starts GHCi with the library loaded 

respectively.
	
Test that the libraries load correctly: this example implements a Moore finite state machine that calculates the running sum and multiplies the output with 2.

    *ForSyDe.Atom> import ForSyDe.Atom.MoC.SY as SY
    *ForSyDe.Atom SY> let s = SY.signal [1..4]
    *ForSyDe.Atom SY> SY.moore11 (+) (*2) 0 s
    {0,2,6,12,20}
	
For more examples, please check the [user manual](https://github.com/forsyde/forsyde-atom-examples/blob/master/manual.pdf) generated from the [`forsyde-atom-examples`](https://github.com/forsyde/forsyde-atom-examples) project.

### The test suite

The test suite and its dependencies can be installed and run by typing in:

	stack test
	
or 

	cabal test
	
respectively.
	
### Getting the documentation

A Haddock extended API + technical report documentation is
periodically generated and published at the following
[web page](https://forsyde.github.io/forsyde-atom). 

To generate the documentation locally you need to checkout the version
found in the `docs` branch. It contains an additional folder called
`docfiles` in the project root, which holds `LaTeX` sources for the
figures and equations in the API documentation. Be aware that it
requires
[additional dependencies](https://github.com/forsyde/forsyde-atom#general-dependencies)
to be installed!

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

 * `imagemagick` to convert the above PDF pictures to HTML-friendly
   PNG format.
   
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
    export PATH=/opt/ghc/bin:/opt/cabal/bin:$PATH
    cabal update
    
    # To be able to generate the documentation, in case a LaTeX installation is not available:
    sudo apt-get install -y texlive texlive-latex-extra texlive-fonts-extra texlive-math-extra
    sudo apt-get install -y imagemagick
    
