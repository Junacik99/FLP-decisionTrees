# Purpose: Makefile for FLP-FUN project
# Author: Martin Takacs (xtakac07)

make:
	ghc -Wall Main.hs -o flp-fun

clean: 
	rm -f *.o *.hi *.o *.hi