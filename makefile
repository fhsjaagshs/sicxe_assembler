all:
	cabal install --dependencies-only
	cabal build
	cp dist/build/assem/assem .

clean:
	rm -rf dist
	rm assem
