gimli_packages = -package readline

hsfiles := $(wildcard src/*.hs)

all : app/gimli

app/gimli : $(hsfiles)
	ghc -isrc --make -o $@ $(gimli_packages) src/Main.hs
