--TP1.hs

-- Raton
data Raton = CRaton {
	edad :: Float,
	peso :: Float,
	altura :: Float
} deriving (Show, Eq)

mikeyMouse = CRaton {
	edad = 88,
	peso = 20,
	altura = 0.8
}


jerry = CRaton {
	edad = 0,
	peso = 0,
	altura = 0
}

pinky = CRaton {
	edad = 0,
	peso = 0,
	altura = 0
}

-- Utils
-- Devuelve true si el valor pasado está entre el valorMaximo y el valorMinimo
estaEntreValores valor valorMinimo valorMaximo = valor < valorMaximo || valor > valorMinimo

mitad = (/) 2
doble = (*) 2

-- Obtiene el porcentaje de un valor pasado, pej: 10%  ==> porcentaje 10 10 = 10 * 10 / 100 ==> 1
porcentaje num denominador = num * denominador / 100

-- Estudios
masaCorporal raton = peso raton / ( ( altura raton ) ^2 )

antiguedad raton = (( edad raton + 5 ) / 85)

-- Analisis
estudioAntiguedad raton = (antiguedad raton) > 1

estudioMasaCorporal raton = (estaEntreValores) (masaCorporal raton) 18.5 25

-- Harcodeado no define un valor critico para el estudio
valorCritico = 10
esMayor indice = indice > valorCritico
analisisDeExceso valorEstudio = esMayor valorEstudio

-- Valores máximo = 25 y minimo 18.5
rangoMedio valor = not ( estaEntreValores valor 25 18.5)
analisisRangoMedio valorEstudio = rangoMedio valorEstudio

analisisBerretas valorEstudio = False

-- Curando ratones
-- Hierbas
hierbaBuena (CRaton edad peso altura) = (CRaton (mitad edad) peso altura)

hierbaMala (CRaton edad peso altura) = (CRaton (doble edad) peso altura)

alcachofa valor (CRaton edad peso altura) = (CRaton (edad - porcentaje edad valor) peso altura)

hierbaZort (CRaton _ _ _) = pinky

-- Medicinas

