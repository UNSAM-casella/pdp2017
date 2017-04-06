--TP1.hs
import Data.List

instance Show (a -> b) where
         show a= "funcion"

-- Raton
data Raton = CRaton {
	edad :: Float,
	peso :: Float,
	altura :: Float,
	enfermedades :: [Enfermedad]
} deriving (Show, Eq)

type Estudio = Raton -> Float
type Analisis = Estudio -> Diagnostico
type Diagnostico = Raton -> Bool
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

rangoMedio min max valor =  estaEntreValores valor min max 

-- Estudios (Raton -> Float)
estudioMasaCorporal :: Estudio
estudioMasaCorporal raton = calcularMasaCorporal (peso raton) (altura raton)

calcularMasaCorporal :: Float -> Float -> Float
calcularMasaCorporal peso altura =   peso / ( altura ^2 )


estudioAntiguedad :: Estudio
estudioAntiguedad  = calcularAntiguedad . edad 

calcularAntiguedad :: Float -> Float
calcularAntiguedad edad = (edad + 5) / 85




-- analisis
-- diagnostico analisis valor = analisis valor

-- Dignosticos - Analisis
-- analisisDeExceso :: Float -> Estudio -> Analisis
analisisDeExceso :: Float -> Analisis
analisisDeExceso valorCritico estudio  =  (valorCritico <) . estudio
{-De exceso, que dan positivo cuando el índice producido por el estudio se encuentra por arriba de cierto valor crítico.
-}



{- De rango medio, que dan positivo cuando el índice producido por el estudio no se encuentra entre dos valores.-}
analisisRangoMedio :: Float -> Float -> Analisis
analisisRangoMedio valorMinimo valorMaximo estudio = not . (rangoMedio valorMinimo valorMaximo) . estudio


{-Berretas, que nunca dan positivo.-}
analisisBerretas :: Analisis
analisisBerretas estudio = darSiemprePositivo

darSiemprePositivo _ = True;

-- Diagnlaticoesviejo = deExceso 10 . Antigüedad.    :: Diagnóstico
-- deExceso valor crítico estudio  =( valor crítico < ).estudio 


-- Curando ratones
-- Hierbas
hierbaBuena :: Hierba
hierbaBuena = cambiarEdad (/2)

hierbaMala :: Hierba
hierbaMala = cambiarEdad (*2)

alcachofa :: Float -> Hierba
alcachofa valorPorcentaje = restarPeso (porcentaje valorPorcentaje)


hierbaZort :: Hierba
hierbaZort = alcachofa 0


-- cambiarEdad efecto raton = raton { edad = (efecto . edad) raton }
cambiarEdad efecto raton = raton { edad = efecto (edad raton) }


restarPeso efecto raton = raton { peso = peso raton - efecto (peso raton) }

cambiarPeso efecto raton = raton { peso = efecto (peso raton) }

-- Medicinas (hacer medicinas)

mezclar :: Hierba -> Hierba -> Hierba
-- mezclar = hierba1 . hierba2 
mezclar = (.)



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
	altura = 0.8,
	enfermedades = ["brucelosis","sarampión","tuberculosis"]
}


jerry = CRaton {
	edad = 76,
	peso = 2,
	altura = 0.3,
	enfermedades = []
}

pinky = CRaton {
	edad = 0,
	peso = 0,
	altura = 0,
	enfermedades = []
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



---------------------------------------------------------------------------------------
------------------------------------ SEGUNDA PARTE ------------------------------------
---------------------------------------------------------------------------------------
type Enfermedad = String

estudioCantidadEnfermedades :: Estudio
estudioCantidadEnfermedades = genericLength.enfermedades


diagnosticoTieneEnfermedad :: Enfermedad -> Diagnostico
diagnosticoTieneEnfermedad enfermedad = (elem enfermedad).enfermedades


-- hierbaVerde :: Hierba
hierbaVerde = cambiarEnfermedades (eliminarEnfermedadesCon "sis")

-- cambiarEdad efecto raton = raton { edad = (efecto . edad) raton }
cambiarEnfermedades efecto raton = raton { enfermedades = (efecto.enfermedades) raton }

eliminarEnfermedadesCon terminacion enfermedades = filter (not.enfermedadTerminaCon terminacion) enfermedades 

enfermedadTerminaCon terminacion enfermedad = elem terminacion (tails enfermedad)