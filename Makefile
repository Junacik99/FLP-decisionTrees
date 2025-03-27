##############################
#  Decision Tree Classifier  #
#            FLP             #
#       Martin Takacs        #
#          xtakac07          #
#            2025            #
##############################
make:
	ghc -Wall Main.hs -o flp-fun

clean: 
	rm -f *.o *.hi *.o *.hi