import Test.Hspec 
funcionMisteriosa2 g = g . length 

{-runtests = hspec $ do  
	describe "Parte 1: Diagnosticos 6" $ do
		it "6-a) De antigüedad: analiza si el índice de antigüedad es mayor a 1. Debería dar positivo para Mickey" $ do 
			mickeyMouse `shouldSatisfy` analisisDeExceso 1 estudioAntiguedad
-}