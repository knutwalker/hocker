install: ${HOME}/bin/hocker

${HOME}/bin/hocker: dist/build/Hocker/hocker
	cp dist/build/Hocker/hocker ${HOME}/bin/hocker

dist/build/Hocker/hocker: dist/setup-config src/main/*.hs src/lib/*.hs src/lib/Hocker/*.hs
	cabal build exe:hocker

dist/setup-config: hocker.cabal
	cabal configure

clean:
	cabal clean

.PHONY: clean install
