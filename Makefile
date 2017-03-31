BIN = bin

all: p1

p1:
	ghc -O2 --make P1.hs -o $(BIN)/p1

clean:
	rm *.o *.hi *.html