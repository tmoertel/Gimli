# GIMLI Makefile
# Tom Moertel <tom@moertel.com>

package_name = gimli

app_dir  = app
src_dir  = src
dist_dir = dist

hsfiles := $(wildcard $(src_dir)/*.hs $(src_dir)/*/*.hs)
vfile    = $(src_dir)/Version.hs
version := $(shell [ -e $(vfile) ] && \
             perl -lne'print $$1 if /([0-9.]+)/' $(vfile))

platform := $(shell (uname -m; uname -s) | tr A-Z a-z | tr -cs a-z0-9 - | \
	      sed -e 's/^-//' -e 's/-$$//')

dist_pkg = $(dist_dir)/$(package_name)-$(version)
bin_pkg  = $(dist_pkg)-bin-$(platform)

gimli_packages = -package readline

ghc      = ghc
ghc_prof =
ghc_opt  =
ghc_opts += $(ghc_prof) $(ghc_opt)

# targets

all:
	@echo This Makefile is a holdback to the pre-Cabal days.
	@echo The project has since been Cabalized.
	@echo Invoking the Cabal build system...
	@echo
	runhaskell Setup.lhs configure --enable-tests
	runhaskell Setup.lhs build
.PHONY: all

$(app_dir)/gimli: $(hsfiles)
	@mkdir -p $(app_dir)
	$(ghc) $(ghc_opts) -i$(src_dir) --make -o $@ $(gimli_packages) $(src_dir)/Shell.hs

.PHONY: prof
prof:
	$(MAKE) ghc_prof"=-auto-all -prof" all

.PHONY: opt
opt:
	$(MAKE) ghc_opt=-O2 all

.PHONY: test sanity recent
test: all
	runhaskell Setup.lhs test

sanity: all
	prove test/01-sanity.t

recent: all
	prove `find test/ -name '*.t' -mmin -60`

.PHONY: smokeloop
smokeloop:
	perl script/smoke bifur:work/darcs/gimli-root gimli-root/gimli

.PHONY: wc
wc:
	find . -type f | \
	egrep -v '/$(app_dir)/|/$(dist_dir)/|~|.o$$|.hi$$|.prof$$|/eg/' | \
	xargs wc -l

.PHONY: clean
clean:
	find $(src_dir) \( -name '*.o' -o -name '*.hi' \) -print | xargs rm -f
	rm -f $(app_dir)/gimli
	rm -rf $(dist_dir)

.PHONY: dist
dist:
	rm -rf $(dist_pkg)
	mkdir -p $(dist_pkg)
	rsync -avRC `cat MANIFEST` $(dist_pkg)
	tar zcf $(dist_pkg).tar.gz -C $(dist_dir) $(notdir $(dist_pkg))
	rm -rf $(dist_pkg)

.PHONY: bindist
bindist: all
	rm -rf $(bin_pkg)
	mkdir -p $(bin_pkg)
	rsync -avRC `cat MANIFEST | grep -v '^$(src_dir)/'` \
	    app $(vfile) $(bin_pkg)
	tar zcf $(bin_pkg).tar.gz -C $(dist_dir) $(notdir $(bin_pkg))
	rm -rf $(bin_pkg)
