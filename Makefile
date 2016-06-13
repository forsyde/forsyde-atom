doc: 
	$(MAKE) -C includes
	cabal configure
	cabal haddock --hyperlink-source --css=includes/styles/ocean.css

sandbox:
	cabal sandbox init
	@if [ ! -d "includes/type-level" ]; then \
		git clone https://github.com/forsyde/type-level.git includes/type-level; \
	fi
	@if [ ! -d "includes/parameterized-data" ]; then \
		git clone https://github.com/forsyde/parameterized-data.git includes/parameterized-data; \
	fi
	cabal sandbox add-source includes/type-level
	cabal sandbox add-source includes/parameterized-data
	cabal install --dependencies-only
	cabal configure
	cabal install

clean:
	$(MAKE) superclean -C includes
	rm -rf dist/doc

