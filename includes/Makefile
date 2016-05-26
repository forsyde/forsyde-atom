# LaTeX-Makefile
#
# Author: Ingo Sander
# 
# Date: 20130420
#
# Description:
#
# The Makefile assumes that original figures are placed in the
# directory ./figs_orig. During execution of the makefile,
# LaTeX-compatible figures are generated in the directory ./figs. The
# name of the main-file has to be explicitly been given in the
# Makefile. LaTeX-files that shall be included shall be placed in the
# ./include directory. Currently the following figure-formats are
# supported: pdf, eps, tex, svg, jpg, png, fig.
#        
# The following commands are supported:
#
# make 
# make view 
# make clean

STYDIR          = ./packages
FIGS_ORIG_DIR   = ./figs_orig
FIGS_DIR        = ./figs

PDF_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.pdf,$(FIGS_DIR)/%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.pdf))
EPS_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.eps,$(FIGS_DIR)/%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.eps))
TIKZ_FIGS	= $(patsubst $(FIGS_ORIG_DIR)%.tex,$(FIGS_DIR)%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.tex))
SVG_FIGS	= $(patsubst $(FIGS_ORIG_DIR)%.svg,$(FIGS_DIR)%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.svg))
JPG_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.jpg,$(FIGS_DIR)/%.jpg,$(wildcard $(FIGS_ORIG_DIR)/*.jpg))
PNG_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.png,$(FIGS_DIR)/%.png,$(wildcard $(FIGS_ORIG_DIR)/*.png))
XFIG_FIGS       = $(patsubst $(FIGS_ORIG_DIR)/%.fig,$(FIGS_DIR)/%.pdf_t,$(wildcard $(FIGS_ORIG_DIR)/*.fig))

STYLEFILES   = $(wildcard $(STYDIR)/*.sty)

all: pre figs

pre:
	mkdir -p includes/figs

figs : $(EPS_FIGS) $(PDF_FIGS) $(XFIG_FIGS) $(SVG_FIGS) \
	$(JPG_FIGS) $(PNG_FIGS) $(LATEXFILES) $(TIKZ_FIGS) \
	Makefile $(STYLEFILES)

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
	convert -density 150 $@ -quality 90 $@.png
	rm $@	
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


