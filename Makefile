PREFIX = ${HOME}

install: ${PREFIX}/bin/hocker

${PREFIX}/bin/hocker: dist/build/Hocker/hocker
	cp dist/build/Hocker/hocker ${PREFIX}/bin/hocker

dist/build/Hocker/hocker: dist/setup-config src/main/*.hs src/lib/*.hs src/lib/Hocker/*.hs
	cabal build exe:hocker

dist/setup-config: .cabal-sandbox/lib
	cabal configure

.cabal-sandbox/lib: cabal.sandbox.config hocker.cabal
	cabal install --only-dependencies

cabal.sandbox.config:
	cabal sandbox init

clean:
	cabal clean

.PHONY: clean install
