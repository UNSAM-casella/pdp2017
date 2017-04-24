--TP1.hs
import Test.Hspec 
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

type Indice = Float
type Estudio = Raton -> Indice
type Analisis = Estudio -> Diagnostico
type Diagnostico = Raton -> Bool
type Hierba = Raton -> Raton
type Colonia = [Raton]

-----------------------------------------
----------------- UTILS -----------------
-----------------------------------------
-- Devuelve true si el valor pasado está entre el valorMaximo y el valorMinimo
estaEntreValores valor valorMinimo valorMaximo = valor < valorMaximo && valor > valorMinimo

mitad num1 = num1 / 2

doble = (*) 2

esMayor :: Ord(a) => a -> a -> Bool
esMayor num1 num2 = num1 > num2

-- Obtiene el porcentaje de un valor pasado, pej: 10%  ==> porcentaje 10 10 = 10 * 10 / 100 ==> 1
porcentaje num denominador = num * denominador / 100

rangoMedio min max valor =  estaEntreValores valor min max 

promedio listadoNums = (sum listadoNums) / (genericLength listadoNums)





----------------------------------------
--------------- ESTUDIOS ---------------
----------------------------------------
estudioMasaCorporal :: Estudio
estudioMasaCorporal raton = calcularMasaCorporal (peso raton) (altura raton)

calcularMasaCorporal :: Float -> Float -> Float
calcularMasaCorporal peso altura =   peso / altura ^2


estudioAntiguedad :: Estudio
estudioAntiguedad  = calcularAntiguedad . edad 

calcularAntiguedad :: Float -> Float
calcularAntiguedad edad = (edad + 5) / 85


-- Dignosticos - Analisis
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




-- Curando ratones
-- Hierbas
hierbaBuena :: Hierba
hierbaBuena = cambiarEdad (/2)

hierbaMala :: Hierba
hierbaMala = cambiarEdad (*2)

alcachofa :: Float -> Hierba
alcachofa valorPorcentaje = restarPeso (porcentaje valorPorcentaje)


hierbaZort :: Hierba
hierbaZort raton = raton { edad = 0, peso = 0, altura = 0, enfermedades = []}


cambiarEdad efecto raton = raton { edad = efecto (edad raton) }

restarPeso efecto raton = raton { peso = peso raton - efecto (peso raton) }

cambiarPeso efecto raton = raton { peso = efecto (peso raton) }

-- Medicinas (hacer medicinas)
mezclar :: Hierba -> Hierba -> Hierba
-- mezclar = hierba1 . hierba2 
mezclar = (.)


type Medicina = [Hierba]

aplicarMedicamentos :: Medicina -> Raton -> Raton
aplicarMedicamentos hierbas raton = foldl aplicarMedicamento raton hierbas

-- Tratamientos (realizar un tratamiento a un ratón contra un diagnostico)
realizarTratamiento diagnostico raton medicinas = foldl (condicionDiagnostico diagnostico) raton medicinas

condicionDiagnostico diagnostico raton medicina
        | diagnostico raton = medicina raton
        | otherwise = raton


---------------------------------------------------------------------------------------
------------------------------------ SEGUNDA PARTE ------------------------------------
---------------------------------------------------------------------------------------
type Enfermedad = String

estudioCantidadEnfermedades :: Estudio
estudioCantidadEnfermedades = genericLength.enfermedades


diagnosticoTieneEnfermedad :: Enfermedad -> Diagnostico
diagnosticoTieneEnfermedad enfermedad = (elem enfermedad).enfermedades


hierbaVerde :: String -> Hierba
hierbaVerde = cambiarEnfermedades.eliminarEnfermedadesCon

cambiarEnfermedades efecto raton = raton { enfermedades = (efecto.enfermedades) raton }

eliminarEnfermedadesCon terminacion enfermedades = filter (not.enfermedadTerminaCon terminacion) enfermedades 

enfermedadTerminaCon terminacion enfermedad = elem terminacion (tails enfermedad)


aplicarMedicamento raton medicina = medicina raton


pdpCilina :: Medicina
pdpCilina = map hierbaVerde terminacionesEnfermedadesInfecciosas

terminacionesEnfermedadesInfecciosas = ["sis", "itis", "emia", "cocos"]





---------------------------------------
------------ OBSERVACIONES ------------
---------------------------------------
type Observacion = Colonia -> Indice

obsPromedioEstudio :: Estudio -> Observacion
obsPromedioEstudio estudio = promedio.(map estudio)


ratonesPeligroDiagnostico :: Diagnostico -> Colonia -> Colonia
ratonesPeligroDiagnostico diagnostico colonia = filter diagnostico colonia

obsCantEnfermos :: Diagnostico -> Observacion
obsCantEnfermos diagnostico = genericLength . ratonesPeligroDiagnostico diagnostico


obsDeLimite :: Estudio -> Diagnostico-> Observacion
obsDeLimite estudio diagnostico colonia = maximum  . map estudio $ (ratonesPeligroDiagnostico diagnostico colonia) 

obsPromedioEnfermedades colonia = (/) (cantidadEnfermedades colonia) (genericLength colonia)
cantidadEnfermedades colonia = genericLength . concat $ (map enfermedades colonia)

cantidadEnfermosPor enfermedad colonia = genericLength (filter (unaEnfermedadEsta enfermedad) colonia)


unaEnfermedadEsta enfermedad = (elem enfermedad).enfermedades

unaEnfermedadEstaEnTodos colonia enfermedad = all (unaEnfermedadEsta enfermedad) colonia

todasLasEnfermedades = nub . concat . map  enfermedades


indiceEnfermedadesPeligrosas = elemIndices True (map (unaEnfermedadEstaEnTodos unaColonia) (todasLasEnfermedades unaColonia))

enfermedadesPeligrosas colonia = map ((!!)(todasLasEnfermedades unaColonia)) indiceEnfermedadesPeligrosas



------------------------------------------------
----------------- EXPERIMENTOS -----------------
------------------------------------------------
medicinaFunciona :: Medicina -> Diagnostico -> Colonia -> Bool
medicinaFunciona medicina diagnostico colonia = not . any diagnostico . aplicarMedicinaDiagnosticoColonia medicina diagnostico $ colonia


aplicarMedicinaDiagnosticoColonia :: Medicina -> Diagnostico -> Colonia -> Colonia
aplicarMedicinaDiagnosticoColonia medicina diagnostico colonia = map (aplicarMedicamentos medicina) (ratonesPeligroDiagnostico diagnostico colonia)


-- Sacar de aca!
muchasMedicinas = [hierbaBuena, hierbaZort, hierbaMala]

-- aplicarMedicamentoColonia :: Medicina -> Colonia -> Colonia
aplicarMedicamentoColonia colonia medicina = map medicina colonia


getElementByIndex list index = (!!) list index
getIndex elem list = elemIndices elem list

unaObservacion = obsPromedioEstudio estudioAntiguedad


observarMedicinasAplicadasColonia _ [] _ = []
observarMedicinasAplicadasColonia observacion (medicina:colaMedicina) colonia = observacion (aplicarMedicamentoColonia medicina colonia)  : observarMedicinasAplicadasColonia observacion colaMedicina colonia


obsLaMejorMedicina observacion medicinas = minimum . observarMedicinasAplicadasColonia observacion medicinas


indexLaMejorMedicina observacion medicinas colonia = head . getIndex (obsLaMejorMedicina observacion medicinas colonia) $ (observarMedicinasAplicadasColonia observacion medicinas colonia)


laMejorMedicina observacion medicinas colonia = getElementByIndex medicinas (indexLaMejorMedicina observacion medicinas colonia)




------------------------------------------------------------------------------------
-------------------------------------- TESTS  --------------------------------------
------------------------------------------------------------------------------------
-- --TESTS
mickeyMouse = CRaton {
	edad = 88,
	peso = 20,
	altura = 0.8,
	enfermedades = ["disneymania", "hipotermia"]
}

jerry = CRaton {
	edad = 76,
	peso = 2,
	altura = 0.3,
	enfermedades = ["tuberculosis", "varicela", "endemia"]
}

pinky = CRaton {
	edad = 0,
	peso = 0,
	altura = 0,
	enfermedades = []
}

-- ratisalil raton = hacerMedicamento [hierbaZort, hierbaMala] raton
pondsAntiAge raton = aplicarMedicamentos [alcachofa 10, hierbaBuena, hierbaBuena, hierbaBuena] raton

-- analisis = 
diagnosticoAntiguedad = analisisDeExceso 1 estudioAntiguedad

diagnosticoTieneDisneymania :: Diagnostico
diagnosticoTieneDisneymania = diagnosticoTieneEnfermedad "disneymania"

unaColonia :: Colonia
unaColonia = [mickeyMouse, jerry]


runtests = hspec $ do  
	describe "Parte 1: Diagnosticos 6" $ do
		it "6-a) De antigüedad: analiza si el índice de antigüedad es mayor a 1. Debería dar positivo para Mickey" $ do 
			mickeyMouse `shouldSatisfy` analisisDeExceso 1 estudioAntiguedad

		it "6-a) De antigüedad: analiza si el índice de antigüedad es mayor a 1. Debería dar negativo para Jerry" $ do 
			jerry `shouldNotSatisfy` analisisDeExceso 1 estudioAntiguedad

		it "6-b) De masa corporal: analiza si el índice de masa corporal no se encuentra entre 18.5 y 25. Debería dar positivo para Mickey" $ do 
			mickeyMouse `shouldSatisfy` analisisRangoMedio 18.5 25 estudioMasaCorporal

		it "6-b) De masa corporal: analiza si el índice de masa corporal no se encuentra entre 18.5 y 25. Debería dar negativo para Jerry" $ do 
			jerry `shouldNotSatisfy` analisisRangoMedio 18.5 25 estudioMasaCorporal
	
	describe "Parte 1: Medicinas 7" $ do
		it "7-a) Verificar que mezclar una Hierba Buena con una Hierba Mala crea una hierba que no produce efecto." $ do 
			(mezclar hierbaBuena hierbaMala) mickeyMouse `shouldBe` mickeyMouse

		it "7-b) Hacer el Ratisalil, un medicamento basado en una Hierba Zort y una Hierba Mala. Verificar que produce los mismos resultados que una Hierba Zort." $ do 
			(mezclar hierbaZort hierbaMala) mickeyMouse `shouldBe` hierbaZort mickeyMouse
			
		it "7-c) Hacer la Ponds Anti Age, que es un medicamento que está hecho con una Alcachofa de  10 y 3 hierbas buenas. Verificar que al administrárselo a Jerry queda con edad = 9.5, peso = 1.8 y altura = 0.3" $ do 
			pondsAntiAge jerry `shouldBe` CRaton 9.5 1.8 0.3 (enfermedades jerry)

		it "7-d-i) Crear un tratamiento contra la antigüedad que conste de una Hierba Buena y una Ponds Anti Age. A Mickey tiene el mismo efecto que una hierba buena." $ do 
			realizarTratamiento diagnosticoAntiguedad mickeyMouse [hierbaBuena, pondsAntiAge] `shouldBe` realizarTratamiento diagnosticoAntiguedad mickeyMouse [hierbaBuena] 

		it "7-d-ii) Crear un tratamiento contra la antigüedad que conste de una Hierba Buena y una Ponds Anti Age. A Jerry tiene el mismo efecto que una alcachofa de 0." $ do 
			realizarTratamiento diagnosticoAntiguedad jerry [hierbaBuena, pondsAntiAge] `shouldBe` realizarTratamiento diagnosticoAntiguedad jerry [alcachofa 0] 

	describe "Parte 2: 8" $ do
		it "8-a) Crear un diagnóstico para saber si un ratón posee disneymania. Deberia dar True en Mickey" $ do 
			mickeyMouse `shouldSatisfy` diagnosticoTieneDisneymania

		it "8-b) Crear un diagnóstico para saber si un ratón posee disneymania. Deberia dar True en Mickey y False en Jerry" $ do 
			mickeyMouse `shouldSatisfy` diagnosticoTieneDisneymania
			jerry `shouldNotSatisfy` diagnosticoTieneDisneymania

		it "8-c) Darle pdpCilina a Jerry sólo le queda varicela como enfermedad" $ do 
			aplicarMedicamentos pdpCilina jerry `shouldBe` CRaton 76.0 2.0 0.3 ["varicela"]
			
	describe "Parte 2: 9" $ do
		it "9-a) Armar una observación que indique el promedio de enfermedades de una colonia." $ do 
			obsPromedioEstudio estudioAntiguedad unaColonia `shouldBe` 1.0235294

		it "9-b) Armar una observación que indique el promedio de enfermedades de una colonia." $ do 
			obsPromedioEnfermedades unaColonia `shouldBe` 2.5

		it "9-c) La cantidad de enfermos que poseen la enfermedad disneymania para la colonia de PDP es 1." $ do 
			cantidadEnfermosPor "disneymania" unaColonia `shouldBe` 1

	describe "Parte 2: 10" $ do
		it "10-a) La pdpCilina no funciona para tratar la enfermedad disneymania." $ do 
			unaColonia `shouldNotSatisfy` medicinaFunciona pdpCilina diagnosticoTieneDisneymania

		it "10-b) La pdpCilina no funciona para tratar la enfermedad disneymania." $ do 
			unaColonia `shouldSatisfy` medicinaFunciona [(hierbaVerde "ania")] diagnosticoTieneDisneymania
