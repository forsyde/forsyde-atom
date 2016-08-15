doc: 
	@if [ ! -d "includes/forsyde-tikz" ]; then \
		git clone https://github.com/forsyde/forsyde-tikz.git \
	     	--branch dev --single-branch includes/forsyde-tikz ; \
	fi

	$(MAKE) -C includes

	cabal configure
	cabal haddock --hyperlink-source --css=includes/styles/ocean.css

sandbox:
	cabal sandbox init
	cabal install --dependencies-only
	cabal configure
	cabal install

clean:
	$(MAKE) superclean -C includes
	rm -rf dist/doc

