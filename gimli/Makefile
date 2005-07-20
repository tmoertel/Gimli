# Makefile

app_dir = app
src_dir = src

hsfiles := $(wildcard $(src_dir)/*.hs)

gimli_packages = -package readline


# targets

all : $(app_dir)/gimli

$(app_dir)/gimli : $(hsfiles)
	ghc -i$(src_dir) --make -o $@ $(gimli_packages) $(src_dir)/Main.hs

.PHONY: test sanity
test :
	prove test/*.t

sanity:
	prove test/sanity.t


