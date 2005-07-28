# GIMLI Makefile

app_dir = app
src_dir = src

hsfiles := $(wildcard $(src_dir)/*.hs)

gimli_packages = -package readline


# targets

all: $(app_dir)/gimli
.PHONY: all

$(app_dir)/gimli: $(hsfiles)
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
