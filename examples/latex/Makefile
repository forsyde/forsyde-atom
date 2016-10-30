
MAIN		= ./examples

STYDIR          = ../../includes/forsyde-tikz/forsyde-tikz
FIGS_ORIG_DIR   = ./figs_orig
FIGS_DIR        = ./figs
STYLEFILES      = $(wildcard $(STYDIR)/*.sty)
LHSFILES        = $(wildcard ../*.lhs)

PDF_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.pdf,$(FIGS_DIR)/%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.pdf))
EPS_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.eps,$(FIGS_DIR)/%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.eps))
TIKZ_FIGS	= $(patsubst $(FIGS_ORIG_DIR)%.tex,$(FIGS_DIR)%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.tex))
SVG_FIGS	= $(patsubst $(FIGS_ORIG_DIR)%.svg,$(FIGS_DIR)%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.svg))
JPG_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.jpg,$(FIGS_DIR)/%.jpg,$(wildcard $(FIGS_ORIG_DIR)/*.jpg))
PNG_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.png,$(FIGS_DIR)/%.png,$(wildcard $(FIGS_ORIG_DIR)/*.png))
XFIG_FIGS       = $(patsubst $(FIGS_ORIG_DIR)/%.fig,$(FIGS_DIR)/%.pdf_t,$(wildcard $(FIGS_ORIG_DIR)/*.fig))


all: 
	mkdir -p $(FIGS_DIR)
	$(MAKE) $(MAIN).pdf

view: $(MAIN).pdf

$(MAIN).pdf : $(MAIN).tex $(EPS_FIGS) $(PDF_FIGS) $(XFIG_FIGS) $(SVG_FIGS) \
              $(JPG_FIGS) $(PNG_FIGS) $(LATEXFILES) $(TIKZ_FIGS) $(TEXFILES) $(LHSFILES)\
              $(MAIN).bbl Makefile $(STYLEFILES) $(BIBFILES)
	TEXINPUTS=:$(STYDIR)/ pdflatex --shell-escape $(MAIN)
	bibtex $(MAIN)
	TEXINPUTS=:$(STYDIR)/ pdflatex $(MAIN)
	@while ( grep "Rerun to get cross-references" 	\
	$(MAIN).log > /dev/null ); do		\
	        echo '** Re-running LaTeX **';		\
	       TEXINPUTS=:$(STYDIR)/ pdflatex $(MAIN);				\
	done

	evince $(MAIN).pdf &

clean:
	find . -name "*.aux" -type f -delete
	find . -name "*~" -type f -delete
	find . -name "*.log" -type f -delete
	find . -name "*.ps" -type f -delete
	find . -name "*.gz" -type f -delete
	find . -name "*.dvi" -type f -delete
	find . -name "*.blg" -type f -delete
	find . -name "*.bbl" -type f -delete
	find . -name "*.tmp" -type f -delete
	find . -name "*.thm" -type f -delete
	find . -name "*.toc" -type f -delete
	find . -name "*.lo*" -type f -delete
	find . -name "*#" -type f -delete
	find . -name "*.out" -type f -delete
	find . -name "*.pdf" -type f -delete
	find . -name "comment.cut" -type f -delete
	rm -f ./figs_orig/*.pdf

superclean: clean
	rm -f ./figs/*

# Rules for original PDF figures
$(PDF_FIGS) : figs/%.pdf: figs_orig/%.pdf
	cp $< $@ 

# Rules for original JPG figures
$(JPG_FIGS) : figs/%.jpg: figs_orig/%.jpg
	cp $< $@ 

# Rules for original PNG figures
$(PNG_FIGS) : figs/%.png: figs_orig/%.png
	cp $< $@ 

# Rules for original EPS figures
GS_OPTS:= -dPDFX
$(EPS_FIGS) : figs/%.pdf : figs_orig/%.eps
        #Creates .pdf files from .esp files
	a2ping --gsextra='$(GS_OPTS)' --outfile=$@  $(<)

# Rules for Tikz and LaTeX figures
$(TIKZ_FIGS): figs/%.pdf: figs_orig/%.tex
	@echo $(@F)
	TEXINPUTS=:$(FIGS_ORIG_DIR)/:$(STYDIR)/ pdflatex $(*F)
	mv $(@F) $@ 
	rm *.log
	rm *.aux

# Rules for SVG files
$(SVG_FIGS): figs/%.pdf : figs_orig/%.svg
	echo $<
	inkscape -z -D --export-pdf=$@ $(<)

# Rules for FIG files (xfig)
# Create combined pdf/latex figures from .fig file
$(XFIG_FIGS): figs/%.pdf_t: figs_orig/%.fig
	echo $*
	fig2dev -L pdftex -p dummy figs_orig/$*.fig > figs/$*.pdf
	fig2dev -L pdftex_t -p figs/$* figs_orig/$*.fig > figs/$*.pdf_t 

$(MAIN).bbl : $(MAIN).tex $(BIBFILES)
	TEXINPUTS=:$(STYDIR)/ pdflatex $(MAIN).tex
	bibtex $(MAIN)
