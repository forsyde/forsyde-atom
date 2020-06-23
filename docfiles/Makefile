FILES = eqs-exb \
	misc
#	eqs-moc moc moc-sy moc-de moc-ct moc-sdf \
	eqs-skel skel eqs-skel-vector skel-vector-func skel-vector-comm \

target-format=figs/$(1)-$(2).png

##################### DO NOT CHANGE FROM HERE #####################

MKDIR = mkdir -p
STYLE = $(wildcard ./*.sty)
FIGS  = $(sort $(foreach file,$(FILES),$(call make-fig-targets,$(file))))

get-fig-names=$(shell grep -oP "(?<=begin{docimage}{).*(?=})" $(1))
make-fig-targets=$(patsubst %,$(call target-format,$(1),%),$(call get-fig-names, $(1).tex))


define compile-latex

	@pdflatex --shell-escape "\def\currimage{$(subst $(1)-,,$(basename $(notdir $(2))))} \input{$(1).tex}"
	pdftoppm $(1).pdf $(2) -png
#	@convert -density 150 $(1).pdf -quality 90 $(2)
endef

define fig-template
  $(2) : ./$(1).tex
	$(foreach image,$(2),$(call compile-latex,$(1),$(image)))
endef

haddock: prepare $(FIGS) Makefile $(STYLE)
	@cd .. && cabal haddock --hyperlink-source --css=docfiles/styles/ocean.css

prepare:
	$(MKDIR) figs

$(foreach file,$(FILES),\
	$(eval $(call fig-template,$(file),$(call make-fig-targets,$(file)))))

clean:
	rm -rf auto
	find . -name "*.aux" -type f -delete
	find . -name "*~" -type f -delete
	find . -name "*.log" -type f -delete
	find . -name "*.ps" -type f -delete
	find . -name "*.gz" -type f -delete
	find . -name "*.dvi" -type f -delete
	find . -name "*.blg" -type f -delete
	find . -name "*.xml" -type f -delete
	find . -name "*.vrb" -type f -delete
	find . -name "*.snm" -type f -delete
	find . -name "*.bbl" -type f -delete
	find . -name "*.nav" -type f -delete
	find . -name "*-blx*" -type f -delete
	find . -name "*.tmp" -type f -delete
	find . -name "*.thm" -type f -delete
	find . -name "*.toc" -type f -delete
	find . -name "*.lo*" -type f -delete
	find . -name "*#" -type f -delete
	find . -name "*.pdf" -type f -delete
	find . -name "*.out" -type f -delete
	find . -name "comment.cut" -type f -delete

superclean: clean
	rm -rf ./figs/*
	rm -f *.pdf
