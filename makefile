all:
	ghc Main.hs -o stk

clean:
	rm stk
	rm *.hi
	rm *.o