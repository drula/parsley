HC = ghc

all: parsley

parsley: main.hs
	$(HC) $^ -o $@

clean:
	rm -f parsley
	rm -f *.hi
	rm -f *.o
