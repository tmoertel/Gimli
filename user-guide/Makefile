# $Id: Makefile,v 1.1 2005/02/09 02:04:25 thor Exp $

texenv     = TEXINPUTS=/home/thor/texmf//:
run_mps    = $(texenv) ghci -i$(HOME)/texmf/FuncMP-1.2
figdir     = figs

ltxfiles  := $(wildcard *.ltx)
pdffiles  := $(ltxfiles:.ltx=.pdf)
figfiles  := $(wildcard $(figdir)/*.hs)
figstamps := $(figfiles:%.hs=%.stamp)

fast      = no



all : $(pdffiles)

fast :
	$(MAKE) fast=yes

show : $(pdffiles)
	xpdf $^

%.pdf : %.ltx figs
	$(texenv) pdflatex $<
ifeq ($(fast),no)
	$(texenv) pdflatex $<
endif

.PHONY : clean
clean :
	rm -f $(figstamps) $(figdir)/*.log $(pdffiles)

# Build Functional MetaPost figures

.PHONY : figs
figs : $(figstamps)

$(figstamps) : %.stamp : %.hs $(figdir)/.FuncMP
	cd $(dir $@) && echo 'mapM_ snd figures' | $(run_mps) $(notdir $<) >& $(notdir $<).log
	touch $@
