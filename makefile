all:
	cabal install --dependencies-only
	cabal build
	rm assem
	cp dist/build/assem/assem .

clean:
	rm assem
