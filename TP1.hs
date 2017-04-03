--TP1.hs

-- Raton
data Raton = CRaton {
	edad :: Float,
	peso :: Float,
	altura :: Float
} deriving (Show, Eq)

type Estudio = Raton -> Float
type Analisis = Raton -> Float
type Estudio = Analisis -> Bool
type Hierba = CRaton edad peso altura -> Raton
type Medicina = [] raton -> raton


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

-- analisis = 
diagnosticoAntiguedad raton = analisisDeExceso 1 (antiguedad raton)

-- analisis = 
-- masaCorporal = 22.222222
diagnosticoMasaCorporal raton = analisisRangoMedio 18.5 25 (masaCorporal raton)

-- Dignosticos - Analisis
-- Harcodeado no define un valor critico para el estudio
analisisDeExceso valorCritico valorEstudio = esMayor valorEstudio valorCritico

-- Valores máximo = 25 y minimo 18.5
analisisRangoMedio min max valorEstudio = not (rangoMedio valorEstudio min max)

analisisBerretas _ = False

-- Curando ratones
-- Hierbas
hierbaBuena (CRaton edad peso altura) = (CRaton (mitad edad) peso altura)

hierbaMala (CRaton edad peso altura) = (CRaton (doble edad) peso altura)

alcachofa valor (CRaton edad peso altura) = (CRaton edad (peso - porcentaje peso valor) altura)

hierbaZort (CRaton _ _ _) = pinky

-- Medicinas (hacer medicinas)
hacerMedicamento [] raton = raton
hacerMedicamento (hierba:cola) raton = hacerMedicamento cola (hierba raton)

aplicarMedicamento = hacerMedicamento


ratisalil raton = hacerMedicamento [hierbaZort, hierbaMala] raton
pondsAntiAge raton = hacerMedicamento [alcachofa 10, hierbaBuena, hierbaBuena, hierbaBuena] raton

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
-- estudioAntiguedad mickeyMouse --Tiene que devolver True
-- estudioAntiguedad jerry --Tiene que devolver False

-- masaCorporal mickeyMouse -- >0 para mickeyMouse
-- masaCorporal jerry 		-- <0 para Jerry

-- --Test 7 a, b, c y d
-- mezclar jerry hierbaBuena hierbaMala --Tiene que devolver un raton igual a Jerry

-- mezclar Jerry hierbaZort hierbaMala --Tiene que devolver a Pinky

-- medicina Jerry [Alcachofa(10, Jerry) hierbaBuena(Jerry edad peso altura) hierbaBuena(Jerry edad peso altura) hierbaBuena(Jerry edad peso altura)] -- Devuelve a un Raton con edad 9.5 peso 1.8 altura 0.3

-- ---tratamiento (CRaton) --Aca en el punto D me mato...








