pack: pack.hs
	ghc --make -O3 pack.hs

test: pack dummy
	./test

clean:
	rm -f pack.hi pack.o pack

dummy:

