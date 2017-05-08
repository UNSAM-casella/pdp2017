data Persona = CPersona {
	nombre :: String,
	suerte :: Float,
	factores :: [Factor],
	dinero :: Dinero
} deriving (Show, Eq)

type Factor =  (String, Float)

suerteTotal :: Persona -> Float
suerteTotal persona = suerte persona * factorAmuleto persona

factorAmuleto :: Persona -> Float
factorAmuleto persona 
	| tieneAmuleto persona = valorAmuleto persona
	| otherwise = 1


tieneAmuleto = tieneFactor "amuleto"

tienePaciencia = tieneFactor "paciencia"

tieneFactor :: String -> Persona -> Bool
tieneFactor nombreBuscar persona = any (filtrarPor nombreBuscar) (factores persona)

filtrarPor nombreBuscar (nombreFactor, valor) = nombreFactor == nombreBuscar && valor > 0

-- valorAmuleto :: Persona -> Float
valorAmuleto persona = getValor . head . filter (filtrarPor "amuleto") $ factores persona

getNombre (nombre, valor) = nombre
getValor (nombre, valor) = valor


data Juego = CJuego {
	nombreJ :: String,
	montoAGanar :: Dinero	-> Dinero,
	criterios :: [Criterio]
}

type Dinero = Float

type Criterio = Persona -> Bool

--2
ruleta = CJuego {
	nombreJ = "Ruleta",
	montoAGanar = (*37),
	criterios = [(>80).suerteTotal]
}

maquinita jackpot = CJuego {
	nombreJ = "Maquina",
	montoAGanar = (+jackpot),
	criterios = [(>95).suerteTotal, tienePaciencia]
}

-- 3
puedeGanar persona juego = all ($ persona) (criterios juego)



-- 4
apostar dinerGanado [] = dinerGanado
apostar apuestaInicial (juego:colaJuegos) = apostar ( ( $ apuestaInicial) (montoAGanar juego) ) colaJuegos
	

-- 5
noPuedenGanarNinguno [] _ = []
-- noPuedenGanarNinguno jugadores (juego:colaJuegos) = all (puedeGanar juego) jugadores : noPuedenGanarNinguno jugadores colaJuegos
-- noPuedenGanarNinguno (jugador:colaJugadores) juegos = not (all (puedeGanar jugador) juegos) : noPuedenGanarNinguno colaJugadores juegos
noPuedenGanarNinguno (jugador:colaJugadores) juegos = nombreNoPuedeganar jugador juegos : noPuedenGanarNinguno colaJugadores juegos

nombreNoPuedeganar jugador juegos 
	| not (all (puedeGanar jugador) juegos)  = nombre jugador
	| otherwise = ""


jugar montoApostar persona juego
	| puedeGanar persona juego = cambiarDinero montoApostar (montoAGanar juego) persona
	| otherwise = persona

cambiarDinero monto accionCambiar persona = persona { dinero = (flip (-) monto).accionCambiar.dinero $ persona }

















--------------------------------------------------------------
---------------------------- TEST ----------------------------
--------------------------------------------------------------
agus = CPersona {
	nombre = "maru",
	suerte = 100,
	factores = [("amuleto", 10)],
	dinero = 150
}

maru = CPersona {
	nombre = "maru",
	suerte = 5,
	factores = [("paciencia", 10),("amuleto", 0)],
	dinero = 3000
}


