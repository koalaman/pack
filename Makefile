pack: pack.hs
	ghc --make -O3 pack.hs

test: pack dummy
	./test

dummy:

