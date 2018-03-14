---
layout: default
title: ForSyDe-Atom
description: A Layered Framework for Modeling Cyber-Physical Systems 
isHome: true
---


# Overview

[![Build Status](https://travis-ci.org/forsyde/forsyde-atom.svg?branch=master)](https://travis-ci.org/forsyde/forsyde-atom)
![Development Status](assets/images/active.svg)

The [ForSyDe-Atom]() project is a spin-off from the [ForSyDe-Shallow]({{site.parent-url}}/forsyde-shallow) library, which further explores the power of functional programming as a system design paradigm. It is a pure functional framework for describing, analyzing and simulating cyber-physical systems (CPS) as *networks of processes* communicating through *signals*, and makes heavy use of concepts like [applicative functors](https://wiki.haskell.org/Applicative_functor) and [type classes](https://en.wikibooks.org/wiki/Haskell/Classes_and_types). 

Excerpt from [Ungureanu and Sander, (2017)](https://forsyde.github.io/publications_bib.html#UngMed2018a):

> The purity inherent from describing behavior as *side-effect free* functions ensures that each and every entity is independent and contains self-sufficient semantics, enabling both the modeling and the simulation of *true concurrency* without the need of a global state or controller. Binding together concepts from a wide pool of formalisms this *unified* framework exhibits three essential properties: 
> 1. it slices processes in *structured enclosing **layers*** abstracting different behavioural aspects, as an embodiment of separation of concerns; 
> 1. it provides *primitive, indivisible building blocks* for each layer, named ***atoms***, as the outcome of deconstructing the abstract semantics to their core; 
> 1. it describes composition in terms of ***patterns*** of building blocks, as a means of both modeling complex behavior within a layer, as well as abstracting it in a hierarchical manner. We demonstrate the potential for modeling, analysis and synthesis through a comprehensible example.

<p align="center"><img src="https://forsyde.github.io/forsyde-latex/assets/svg/example-pictures-layered.svg"></p>
<p align="center">The layered process model.</p>

# Quick-start for the impatient

If you cannot wait to get your hands dirty with ForSyDe-Atom, follow the instructions below. Otherwise we do recommend you to take it easy and [discover the full features](#documentation) of this EDSL.

Before you try anything, make sure you have [Haskell Platform](https://www.haskell.org/platform/) installed on your machine. Once this is done, get the `forsyde-atom` package through any means suit you best, e.g. by cloning the [repository](https://github.com/forsyde/forsyde-atom) or by downloading and extracting one of the archives above.

Installing the package is as simple as typing in the terminal:

    cd path/to/forsyde-atom
	cabal install
	
You need to be patient until the installation finishes. Afterwards you can start a new interpreter session

    ghci

and import the newly installed libraries and try them out

    Prelude> import ForSyDe.Atom
	Prelude ForSyDe.Atom> import ForSyDe.Atom.MoC.SY as SY
    Prelude ForSyDe.Atom SY> let s = SY.signal [1..4]
    Prelude ForSyDe.Atom SY> SY.moore11 (+) (*2) 0 s
	{0,2,6,12,20}

The example above implements a Moore finite state machine that calculates the running sum and multiplies the output with 2. It instantiates the [`moore`](api/ForSyDe-Atom-MoC.html#v:moore22) pattern with one input and one output (as seen below) with the [synchronous (SY) MoC semantics](api/ForSyDe-Atom-MoC-SY.html) using the [`SY.moore11`](http://localhost:4000/api/ForSyDe-Atom-MoC.html#v:moore22) helper. 

<p align="center"><img src="assets/svg/moore.svg"></p>
<p align="center">The atoms pattern of a Moore state machine.</p>


<!--                                                                                                                                                                                                                  For a technical report, check the extended API documentation at -->
<!-- [forsyde.github.io/forsyde-atom](https://forsyde.github.io/forsyde-atom), -->
<!-- which was generated with Haddock. -->

<!-- ---- -->

<!-- **OBS:** the examples and case studies are being migrated to the  -->
<!-- [forsye-atom-examples](https://github.com/forsyde/forsyde-atom-examples)  -->
<!-- repository. Please refer to that repository for further reports and  -->
<!-- instructions.  -->

<!-- ---- -->

<!-- Ideas based on the projects `forsyde-patterns` (George Ungureanu), -->
<!-- `forsyde-appl` (Mikkel Jakobsen) and the `ForSyDe. library (KTH-SAM -->
<!-- group) -->

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

For more examples, please check the [user manual](https://github.com/forsyde/forsyde-atom-examples/blob/master/manual.pdf) generated from the [`forsyde-atom-examples`](https://github.com/forsyde/forsyde-atom-examples) project.

To uninstall the library and everything that was generated, you can type in:

    cabal sandbox delete              # deletes the sandbox and everything in it
	
### The test suite

The test suite and its dependencies can be installed and run by
explicitly adding the flag `--enable-tests` to the previous
installation commands, namely:

    cabal install --enable-tests --dependencies-only # (optional)
    cabal install --enable-tests                     # installs library + test suite
    cabal configure --enable-tests                   # configures the package to run the test suite
    cabal test                                       # runs the test suite 
   
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
    
