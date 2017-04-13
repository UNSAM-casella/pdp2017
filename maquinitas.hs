data Persona = CPersona {
	nombre :: String,
	suerte :: Float,
	factores :: [Factor],
}

type Factor :: (String, Float)

suerteTotal :: Persona -> Float
suerteTotal persona = suerte persona * factorAmuleto persona

factorAmuleto :: Persona -> Float
factorAmuleto persona =
	| tieneAmuleto persona = valorAmuleto persona
	| otherwise = 1


tieneAmuleto persona = filter filtarAmuletos persona

filtarAmuletos (nombreFactor, valor) = nombreFactor == "amuleto" && valor > 0

valorAmuleto :: Persona -> Float
valorAmuleto persona = fst . tieneAmuleto filtarAmuletos persona