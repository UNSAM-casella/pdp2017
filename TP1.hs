--TP1.hs

-- Raton
data Raton = CRaton {
	edad :: Float,
	peso :: Float,
	altura :: Float
} deriving (Show, Eq)



-- Utils
-- Devuelve true si el valor pasado está entre el valorMaximo y el valorMinimo
estaEntreValores valor valorMinimo valorMaximo = valor < valorMaximo || valor > valorMinimo

-- Revisar como simplificar esto
mitad num1 = num1 / 2
doble = (*) 2

-- Obtiene el porcentaje de un valor pasado, pej: 10%  ==> porcentaje 10 10 = 10 * 10 / 100 ==> 1
porcentaje num denominador = num * denominador / 100

-- Estudios

masaCorporal :: Raton -> Float
masaCorporal raton = peso raton / ( ( altura raton ) ^2 )

antiguedad :: Raton -> Float
antiguedad raton = (( edad raton + 5 ) / 85)

diagnosticoAntiguedad :: Raton -> Bool
diagnosticoAntiguedad raton = (antiguedad raton) > 1

diagnosticoMasaCorporal :: Raton -> Bool
diagnosticoMasaCorporal raton = (estaEntreValores) (masaCorporal raton) 18.5 25

-- Dignosticos - Analisis
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

alcachofa valor (CRaton edad peso altura) = (CRaton edad (peso - porcentaje peso valor) altura)

hierbaZort (CRaton _ _ _) = pinky



-- Usar tipo en hierbas
-- usar patter matching con una funcion que reciba un medicamento . tmb ysar cabeza y cola
-- agregar a las hierbas efecto
fun [] raton = raton
fun (cabeza:cola) raton = fun cola (cabeza raton)

-- Medicinas (hacer medicinas)
hacerMedicamento = fun 
-- Test de mezclarHierbas hierbaBuena hierbaMala jerry OK!
-- Test de mezclarHierbas hierbaZort hierbaMala jerry OK!
-- Test de mezclarHierbas hierbaMala hierbaBuena jerry OK!
-- Test de mezclarHierbas hierbaMala hierbaZort jerry OK!
-- Test de alcachofa 10 con 3 hierbasBuenas

-- Queda obsoleta
-- mezclarHierbas hierba1 hierba2 raton = hierba2 (CRaton (edad (hierba1 raton)) (peso (hierba1 raton)) (altura (hierba1 raton)) )
-- Test de mezclarHierbas hierbaBuena hierbaMala jerry OK!
-- Test de mezclarHierbas hierbaZort hierbaMala jerry OK!
-- Test de mezclarHierbas hierbaMala hierbaBuena jerry OK!
-- Test de mezclarHierbas hierbaMala hierbaZort jerry OK!

ratisalil raton = hacerMedicamento [hierbaZort, hierbaMala] raton
pondsAntiAge raton = hacerMedicamento [alcachofa 10, hierbaBuena, hierbaBuena, hierbaBuena] raton

-- Tratamientos (realizar un tratamiento a un ratón contra un diagnostico)


condition :: Int -> Bool
condition x = x > 0 && x < 100

-- realizarTratamiento :: (Raton -> Bool) -> [a] -> Raton -> Raton
-- realizarTratamiento diagnostico [] raton = raton
-- realizarTratamiento diagnostico (medicina:xs) raton  = (realizarTratamiento (diagnostico raton) xs (medicina raton))

divideList :: [[a]] -> ([[a]], [[a]])

realizarTratamiento :: t1 -> [a] -> Raton -> Raton
realizarTratamiento diagnostico [] raton = raton
realizarTratamiento diagnostico (medicina:xs) raton
	| diagnostico raton > 0 || diagnostico raton == False = realizarTratamiento'
	| otherwise = realizarTratamiento'
	where realizarTratamiento' = realizarTratamiento diagnostico xs (medicina raton)


maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

-- Medicinas
-- cualquierHierba (CRaton hierba ) = hierba(CRaton edad, peso, altura)

-- --recibe 2 hierbas y suma el efecto de ambas
-- mezclar (CRaton hierba1 hierba2  )= hierba1(hierba2(CRaton edad peso altura)

-- -- recibe una lista de hierbas y un raton
-- medicina (CRaton [hierba])  = forEach hierba(CRaton edad, peso, altura)

-- -- recibe una lista de medicinas, entonces en el for each va  a llamar a la funcion medicina
-- tratamiento (CRaton [medicina]) = (forEach medicina(CRaton edad, peso, altura))


-- --TESTS
mikeyMouse = CRaton {
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
-- estudioAntiguedad mikeyMouse --Tiene que devolver True
-- estudioAntiguedad jerry --Tiene que devolver False

-- masaCorporal mikeyMouse -- >0 para mikeyMouse
-- masaCorporal jerry 		-- <0 para Jerry

-- --Test 7 a, b, c y d
-- mezclar jerry hierbaBuena hierbaMala --Tiene que devolver un raton igual a Jerry

-- mezclar Jerry hierbaZort hierbaMala --Tiene que devolver a Pinky

-- medicina Jerry [Alcachofa(10, Jerry) hierbaBuena(Jerry edad peso altura) hierbaBuena(Jerry edad peso altura) hierbaBuena(Jerry edad peso altura)] -- Devuelve a un Raton con edad 9.5 peso 1.8 altura 0.3

-- ---tratamiento (CRaton) --Aca en el punto D me mato...








