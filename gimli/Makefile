# GIMLI Makefile

package_name = gimli

app_dir  = app
src_dir  = src
dist_dir = dist

hsfiles := $(wildcard $(src_dir)/*.hs)
version := $(shell perl -lne'print $$1 if /([0-9.]+)/' $(src_dir)/Version.hs)

dist_pkg = $(dist_dir)/$(package_name)-$(version)

gimli_packages = -package readline


# targets

all: $(app_dir)/gimli
.PHONY: all

$(app_dir)/gimli: $(hsfiles)
	@mkdir -p $(app_dir)
	ghc -i$(src_dir) --make -o $@ $(gimli_packages) $(src_dir)/Main.hs

.PHONY: test sanity recent
test: all
	prove test/*.t

sanity: all
	prove test/01-sanity.t

recent: all
	prove `find test/ -name '*.t' -mmin -60`

.PHONY: smokeloop
smokeloop:
	perl script/smoke bifur:work/darcs/gimli-root gimli-root/gimli

.PHONY: wc
wc:
	find . -type f | egrep -v '/app/|~|.o$$|.hi$$' | xargs wc -l

.PHONY: clean
clean:
	rm -f $(src_dir)/*.o $(src_dir)/*.hi $(app_dir)/gimli
	rm -rf $(dist_dir)

.PHONY: dist
dist:
	rm -rf $(dist_pkg)
	mkdir -p $(dist_pkg)
	rsync -aRC `cat MANIFEST` $(dist_pkg)
	tar zcf $(dist_pkg).tar.gz -C $(dist_dir) $(package_name)-$(version)

