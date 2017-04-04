--TP1.hs
instance Show (a -> b) where
         show a= "funcion"

-- Raton
data Raton = CRaton {
	edad :: Float,
	peso :: Float,
	altura :: Float
} deriving (Show, Eq)

type Estudio = Raton -> Raton
type Analisis = Raton -> Float
type Hierba = Raton -> Raton
type Medicina = [Hierba] -> Raton -> Raton


-- Utils
-- Devuelve true si el valor pasado está entre el valorMaximo y el valorMinimo
estaEntreValores valor valorMinimo valorMaximo = valor < valorMaximo && valor > valorMinimo

mitad num1 = num1 / 2

doble = (*) 2

esMayor :: Ord(a) => a -> a -> Bool
esMayor num1 num2 = num1 > num2

-- Obtiene el porcentaje de un valor pasado, pej: 10%  ==> porcentaje 10 10 = 10 * 10 / 100 ==> 1
porcentaje num denominador = num * denominador / 100

rangoMedio valor min max =  estaEntreValores valor min max 

-- Estudios
masaCorporal :: Raton -> Float
masaCorporal raton = peso raton / ( ( altura raton ) ^2 )

antiguedad raton = (( edad raton + 5 ) / 85)
antiguedad :: Raton -> Float

-- analisis
diagnostico analisis valor = analisis valor

-- Dignosticos - Analisis
-- Harcodeado no define un valor critico para el estudio
analisisDeExceso valorCritico valorEstudio = esMayor valorEstudio valorCritico

-- Valores máximo = 25 y minimo 18.5
analisisRangoMedio min max valorEstudio = not (rangoMedio valorEstudio min max)

analisisBerretas _ = False

-- Curando ratones
-- Hierbas
hierbaBuena :: Hierba
hierbaBuena (CRaton edad peso altura) = (CRaton (mitad edad) peso altura)

hierbaMala :: Hierba
hierbaMala (CRaton edad peso altura) = (CRaton (doble edad) peso altura)

alcachofa :: Float -> Hierba
alcachofa valor (CRaton edad peso altura) = (CRaton edad (peso - porcentaje peso valor) altura)

hierbaZort :: Hierba
hierbaZort (CRaton _ _ _) = pinky

-- Medicinas (hacer medicinas)
-- TODO: Falta hacer una función que reciba 2 hierbas y devuelva 1 hierba con el efecto que daría
-- mezclar :: Hierba -> Hierba -> Hierba
-- mezclar hierba1 hierba2 = ACA HAY QUE SUMAR TODOS LOS VALORES DE hierba1 CON LOS VALORES DE hierba2

mezclar :: Hierba -> Hierba -> Hierba
mezclar = hierba1 . hierba2 



hacerMedicamento :: Medicina
hacerMedicamento [] raton = raton
hacerMedicamento (hierba:cola) raton = hacerMedicamento cola (hierba raton)

aplicarMedicamento = hacerMedicamento


-- ratisalil raton = hacerMedicamento [hierbaZort, hierbaMala] raton
-- pondsAntiAge raton = hacerMedicamento [alcachofa 10, hierbaBuena, hierbaBuena, hierbaBuena] raton

-- Tratamientos (realizar un tratamiento a un ratón contra un diagnostico)
realizarTratamiento diagnostico raton medicinas = foldl (condicionDiagnostico diagnostico) raton medicinas

condicionDiagnostico diagnostico raton medicina
        | diagnostico raton = medicina raton
        | otherwise = raton


-- --TESTS
mickeyMouse = CRaton {
	edad = 88,
	peso = 20,
	altura = 0.8
}


jerry = CRaton {
	edad = 76,
	peso = 2,
	altura = 0.3
}

pinky = CRaton {
	edad = 0,
	peso = 0,
	altura = 0
}

-- --Test 6 a y b
-- diagnostico analisisDeExceso 1 (antiguedad mickeyMouse) ShouldBy True
-- diagnostico analisisDeExceso 1 (antiguedad jerry) ShouldBy False
-- diagnostico analisisRangoMedio 18.5 25 (masaCorporal mickeyMouse) ShouldBy True
-- diagnostico analisisRangoMedio 18.5 25 (masaCorporal jerry) ShouldBy False

-- masaCorporal mickeyMouse -- >0 para mickeyMouse
-- masaCorporal jerry 		-- <0 para Jerry

-- --Test 7 a, b, c y d
-- mezclar jerry hierbaBuena hierbaMala --Tiene que devolver un raton igual a Jerry

-- mezclar Jerry hierbaZort hierbaMala --Tiene que devolver a Pinky

-- medicina Jerry [Alcachofa(10, Jerry) hierbaBuena(Jerry edad peso altura) hierbaBuena(Jerry edad peso altura) hierbaBuena(Jerry edad peso altura)] -- Devuelve a un Raton con edad 9.5 peso 1.8 altura 0.3









