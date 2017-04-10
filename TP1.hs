--TP1.hs
import Data.List

instance Show (a -> b) where
         show a= "funcion"

-- Raton
data Raton = CRaton {
	edad :: Float,
	peso :: Float,
	altura :: Float,
	enfermedades :: [Enfermedad],
	nombre :: String
} deriving (Show, Eq)


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



type Indice = Float
type Estudio = Raton -> Indice
type Analisis = Estudio -> Diagnostico
type Diagnostico = Raton -> Bool
type Hierba = Raton -> Raton


-- analisis
-- analizar :: Estudio -> Analisis
-- analizar :: Estudio -> (Estudio -> Diagnostico)
analizar :: Analisis
analizar estudio = analisisDeExceso 1 estudio
	


-- Dignosticos - Analisis
-- analisisDeExceso :: Float -> Estudio -> Analisis
analisisDeExceso :: Float -> Analisis
analisisDeExceso valorCritico estudio  =  (valorCritico <) . estudio
-- analisisDeExceso 1 estudioAntiguedad(mickeyMouse)
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


type Medicina = [Hierba]

aplicarMedicamentos :: Raton -> Medicina ->Raton
aplicarMedicamentos raton [] = raton
aplicarMedicamentos raton (hierba:cola)= aplicarMedicamentos (hierba raton) cola


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
	enfermedades = ["brucelosis","sarampion","tuberculosis"],
	nombre = "Mickey"
}

minnie = CRaton {
	edad = 90,
	peso = 30,
	altura = 0.6,
	enfermedades = ["tuberculosis","brucelosis"],
	nombre = "Minnie"
}

jerry = CRaton {
	edad = 76,
	peso = 2,
	altura = 0.3,
	enfermedades = ["otitis","tuberculosis","brucelosis"],
	nombre = "Jerry"
}

pinky = CRaton {
	edad = 0,
	peso = 0,
	altura = 0,
	enfermedades = [],
	nombre = "Pinky"
}

-- --Test 6 a y b
-- hacerDiagnostico analisisDeExceso 1 (calcularAntiguedad (edad mickeyMouse)) ShouldBy True
-- hacerDiagnostico analisisDeExceso 1 (antiguedad jerry) ShouldBy False
-- hacerDiagnostico analisisRangoMedio 18.5 25 (masaCorporal mickeyMouse) ShouldBy True
-- hacerDiagnostico analisisRangoMedio 18.5 25 (masaCorporal jerry) ShouldBy False

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


hierbaVerde :: String -> Hierba
hierbaVerde terminacion = cambiarEnfermedades (eliminarEnfermedadesCon terminacion)

cambiarEnfermedades efecto raton = raton { enfermedades = (efecto.enfermedades) raton }

-- cambiarEnfermedades efecto raton = raton { enfermedades = filter (not.enfermedadTerminaCon "sis") (enfermedades raton) }

-- cambiarEnfermedades (eliminarEnfermedadesCon terminacion (enfermedades raton))

-- eliminarEnfermedadesCon terminacion (enfermedades raton) ====>>>> filter (not.enfermedadTerminaCon terminacion) (enfermedades raton)


eliminarEnfermedadesCon terminacion enfermedades = filter (not.enfermedadTerminaCon terminacion) enfermedades 

enfermedadTerminaCon terminacion enfermedad = elem terminacion (tails enfermedad)

-- pdpCilina :: Medicina
-- type Medicina = [Hierba] -> Raton -> Raton
-- pdpCilina raton = foldl aplicarMedicamento raton hierbasVerdesContraInfecciosas


-- aplicarMedicamentos :: Medicina -> Raton -> Raton
-- aplicarMedicamentos hierbas raton = foldl aplicarMedicamento raton hierbas

-- aplicarMedicamentos = hacerMedicamento
-- aplicarMedicamento :: Medicina -> Raton -> Raton
-- aplicarMedicamento raton medicina = medicina raton
aplicarMedicamento medicina raton = medicina raton


pdpCilina :: Medicina
pdpCilina = hierbasVerdesContraInfecciosas

hierbasVerdesContraInfecciosas = map hierbaVerde terminacionesEnfermedadesInfecciosas

terminacionesEnfermedadesInfecciosas = ["sis", "itis", "emia", "cocos"]






-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
----------------------------------------- COLONIA -----------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------


type Colonia = [Raton]
unaColonia = [mickeyMouse, jerry, minnie]
-- unaColonia = [minnie]











---------------------------------------
------------ OBSERVACIONES ------------
---------------------------------------
type Observacion = Colonia -> Indice

-- obsPromedioEstudio :: Observacion
-- obsPromedioEstudio = map promedio (estudio raton)

obsPromedioEstudio :: Estudio -> Colonia -> Indice
obsPromedioEstudio estudio colonia = promedio (map estudio colonia)



ratonesPeligroDiagnostico :: Diagnostico -> Colonia -> Colonia
ratonesPeligroDiagnostico diagnostico colonia = filter diagnostico colonia

obsCantEnfermos :: Diagnostico -> Observacion
obsCantEnfermos diagnostico = genericLength . ratonesPeligroDiagnostico diagnostico
-- obsCantEnfermos (analisisDeExceso 1 estudioAntiguedad) unaColonia


obsDeLimite :: Estudio -> Diagnostico-> Observacion
obsDeLimite estudio diagnostico colonia = maximum  . map estudio $ (ratonesPeligroDiagnostico diagnostico colonia) 
-- obsDeLimite estudioAntiguedad (analisisDeExceso 1 estudioAntiguedad) unaColonia
-- ratonesPeligroDiagnostico (analisisDeExceso 1 estudioAntiguedad) unaColonia

-- enfermedadesPeligrosas :: Colonia -> [Enfermedad]
-- enfermedadesPeligrosas colonia = all elem (enfermedades raton)

{-Obtengo todas las enfermedades de todos los ratones. 
map enfermedades unaColonia

si una enfermedad esta en todos los ratones, es peligrosa.
-}

unaEnfermedadEsta enfermedad raton = elem enfermedad (enfermedades raton)

unaEnfermedadEstaEnTodos colonia enfermedad = all (unaEnfermedadEsta enfermedad) colonia
-- unaEnfermedadEstaEnTodos enfermedad colonia = map (unaEnfermedadEsta enfermedad (enfermedades colonia)) colonia


-- todasLasEnfermedades colonia = nub (concat (map enfermedades colonia))
-- todasLasEnfermedades colonia = nub . concat $ (map enfermedades colonia)
-- todasLasEnfermedades colonia = nub . concat . map  enfermedades $ colonia
todasLasEnfermedades = nub . concat . map  enfermedades


-- indiceEnfermedadesPeligrosas = elemIndices True (map (unaEnfermedadEstaEnTodos unaColonia) (todasLasEnfermedades unaColonia))
indiceEnfermedadesPeligrosas = elemIndices True (map (unaEnfermedadEstaEnTodos unaColonia) (todasLasEnfermedades unaColonia))

-- map ((!!)(todasLasEnfermedades unaColonia))(todasLasEnfermedades unaColonia) indiceEnfermedadesPeligrosas
enfermedadesPeligrosas colonia = map ((!!)(todasLasEnfermedades unaColonia)) indiceEnfermedadesPeligrosas



------------------------------------------------
----------------- EXPERIMENTOS -----------------
------------------------------------------------

medicinaFunciona medicina diagnostico colonia = map (aplicarMedicamento medicina) (ratonesPeligroDiagnostico diagnostico colonia)
{- FALTA CONTAR RESULTADO Y DAR TRUE O FALSE SI ES MAYOR A 0 -}
-- medicinaFunciona hierbaBuena (analisisDeExceso 1 estudioAntiguedad) unaColonia
-- ratonesPeligroDiagnostico (analisisDeExceso 1 estudioAntiguedad) unaColonia
-- aplicarMedicamento hierbaBuena

-- map (aplicarMedicamento hierbaBuena ) (ratonesPeligroDiagnostico (analisisDeExceso 1 estudioAntiguedad) unaColonia)


-- experimentarMuchasMedicinas :: [Medicina] -> Colonia



muchasMedicinas = [hierbaBuena, hierbaZort, hierbaMala]




aplicarMedicamentoColonia medicina colonia = map medicina colonia



experimentarMuchasMedicinas [] raton = raton
experimentarMuchasMedicinas (medicina:colaMedicina) raton = experimentarMuchasMedicinas colaMedicina (medicina raton)

{- No sirve, recorre uno por uno en ambas listas al mismo tiempo 
addList [] _ = []
addList _ [] = []
addList (raton:colaRaton) (medicina:colaMedicina) = medicina raton : addList colaRaton colaMedicina
-}


-- addList [] _ = []
-- addList (raton:colaRaton) medicinas = experimentarMuchasMedicinas raton medicinas : addList colaRaton medicinas



aplicarMuchasMedicinasColonia :: Colonia -> Medicina -> Colonia
aplicarMuchasMedicinasColonia colonia medicinas = map (experimentarMuchasMedicinas medicinas) colonia




-- observarColoniaMedicinas = map (obsCantEnfermos (analisisDeExceso 1 estudioAntiguedad) ) (aplicarMuchasMedicinasColonia unaColonia muchasMedicinas)



-- obsCantEnfermos (analisisDeExceso 1 estudioAntiguedad) (aplicarMedicamentoColonia hierbaBuena unaColonia)

-- laMejorMedicina [] = []
-- laMejorMedicina (medicina:colaMedicina) = obsCantEnfermos (analisisDeExceso 1 estudioAntiguedad) (aplicarMedicamentoColonia medicina unaColonia) : colaMedicina


getElementByIndex list index = (!!) list index
getIndex elem list = elemIndices elem list


{- Quitar observacion y estudio harcodeados -}
observarMedicinasAplicadasColonia [] _ = []
observarMedicinasAplicadasColonia (medicina:colaMedicina) colonia = obsCantEnfermos (analisisDeExceso 1 estudioAntiguedad) (aplicarMedicamentoColonia medicina colonia)  : observarMedicinasAplicadasColonia colaMedicina colonia


obsLaMejorMedicina medicinas colonia = minimum (observarMedicinasAplicadasColonia medicinas colonia)


indexLaMejorMedicina medicinas colonia = head ( getIndex (obsLaMejorMedicina medicinas colonia) (observarMedicinasAplicadasColonia medicinas colonia) )


laMejorMedicina medicinas colonia = getElementByIndex medicinas (indexLaMejorMedicina medicinas colonia)