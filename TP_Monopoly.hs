import Text.Show.Functions

data Participante = Participante{
    nombre       :: String,
    dinero       :: Int, 
    tactica      :: String, 
    propiedades  :: [Propiedad], 
    acciones     :: [Accion]
    }deriving (Show)

data Propiedad = Propiedad{
    nombrePropiedad :: String,
    precio :: Int
}deriving (Eq,Show)

type Accion = Participante -> Participante

--Funciones Auxiliares
cambiarDinero :: (Int -> Int) -> Participante -> Participante
cambiarDinero funcion jugador = jugador{dinero = funcion (dinero jugador)}

agregarPropiedad :: Propiedad -> Accion
agregarPropiedad propiedad jugador = jugador{propiedades = propiedades jugador ++ [propiedad] }

adquirirPropiedad :: Propiedad -> Accion
adquirirPropiedad propiedad jugador = cambiarDinero (subtract (precio propiedad)).agregarPropiedad propiedad $ jugador

esTacticaGanadora :: Participante -> Bool
esTacticaGanadora jugador = tactica jugador == "Oferente singular" || tactica jugador == "Accionista"

puedeComprar :: Propiedad -> Participante -> Bool
puedeComprar propiedad jugador = dinero jugador >= precio propiedad

cambiarTactica :: String -> Participante -> Participante
cambiarTactica unaTactica jugador = jugador{tactica = unaTactica}

valorAlquilerPropiedades :: Propiedad -> Int
valorAlquilerPropiedades propiedad | ((<150).precio) propiedad = 10
                                   | otherwise = 20

--Acciones
pasarPorElBanco :: Accion
pasarPorElBanco jugador = cambiarDinero (+40).cambiarTactica "Comprador Compulsivo" $ jugador

enojarse :: Accion
enojarse jugador = cambiarDinero (+50) jugador{acciones = acciones jugador ++ [gritar]}

gritar :: Accion
gritar jugador = jugador{nombre = "AHHHH" ++ nombre jugador}

subastar :: Propiedad -> Accion 
subastar propiedad jugador | esTacticaGanadora jugador && puedeComprar propiedad jugador = adquirirPropiedad propiedad jugador
                           | otherwise = jugador

cobrarAlquileres :: Accion
cobrarAlquileres jugador = cambiarDinero (+(sum.map valorAlquilerPropiedades $ (propiedades jugador))) jugador

pagarAAccionistas :: Accion
pagarAAccionistas jugador | tactica jugador == "Accionista" = cambiarDinero (+200) jugador
                          | otherwise = cambiarDinero (subtract 100) jugador

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad jugador | puedeComprar propiedad jugador = adquirirPropiedad propiedad jugador
                                    | otherwise = (hacerBerrinchePor propiedad).gritar.(cambiarDinero (+10)) $ jugador

ultimaRonda :: Accion
ultimaRonda jugador = foldl1 (.) (acciones jugador) $ jugador

juegoFinal :: Participante -> Accion
juegoFinal jugador1 jugador2 | (dinero.ultimaRonda) jugador1 > (dinero.ultimaRonda) jugador2 = ultimaRonda jugador1
                             | otherwise = ultimaRonda jugador2

--Participantes
carolina :: Participante
carolina = Participante{nombre = "Carolina", dinero = 500, tactica = "Accionista", propiedades = [lasHeras, yrigoyen], acciones = [pasarPorElBanco, pagarAAccionistas]}

manuel :: Participante
manuel = Participante{nombre = "Manuel", dinero = 500, tactica = "Oferente singular", propiedades = [], acciones = [pasarPorElBanco, enojarse]}

lasHeras :: Propiedad
lasHeras = Propiedad{nombrePropiedad = "Las Heras", precio = 150}

yrigoyen :: Propiedad
yrigoyen = Propiedad{nombrePropiedad = "Hipolito Yrigoyen", precio = 100}
