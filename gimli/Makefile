# Makefile

app_dir = app
src_dir = src

hsfiles := $(wildcard $(src_dir)/*.hs)

gimli_packages = -package readline


# targets

all : $(app_dir)/gimli
.PHONY: all

$(app_dir)/gimli : $(hsfiles)
	ghc -i$(src_dir) --make -o $@ $(gimli_packages) $(src_dir)/Main.hs

.PHONY: test sanity smokeloop
test: all
	prove test/*.t

sanity:
	prove test/sanity.t

smokeloop:
	perl script/smoke bifur:work/darcs/gimli-root gimli-root/gimli
