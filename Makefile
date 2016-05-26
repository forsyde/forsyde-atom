doc: 
	$(MAKE) -C includes
	cabal configure
	cabal haddock --hyperlink-source

clean:
	$(MAKE) superclean -C includes
	rm -rf dist/doc

