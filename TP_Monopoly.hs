import Text.Show.Functions

data Participante = Participante{
    nombre       :: String,
    dinero       :: Int, 
    tactica      :: String, 
    propiedades  :: [Propiedad], 
    acciones     :: [Accion]
    }deriving (Show)

type Propiedad = (String,Int)

type Accion = Participante -> Participante

--Funciones Auxiliares
cambiarDinero :: (Int -> Int) -> Participante -> Participante
cambiarDinero funcion jugador = jugador{dinero = funcion (dinero jugador)}

cantidadPropiedadesSegun :: (Int -> Bool) -> [Propiedad] -> Int
cantidadPropiedadesSegun condicion propiedades = (length.filter condicion) (map snd propiedades)

--Acciones
pasarPorElBanco :: Accion
pasarPorElBanco jugador = cambiarDinero (+40) jugador{tactica = "Comprador Compulsivo"}

enojarse :: Accion
enojarse jugador = cambiarDinero (+50) jugador{acciones = acciones jugador ++ [gritar]}

gritar :: Accion
gritar jugador = jugador{nombre = "AHHHH" ++ nombre jugador}

subastar :: Propiedad -> Accion 
subastar propiedad jugador | (tactica jugador == "Oferente singular" || tactica jugador == "Accionista") && dinero jugador >= snd propiedad = cambiarDinero (+(-snd propiedad)) jugador{propiedades = propiedades jugador ++ [propiedad] }
                           | otherwise = jugador

cobrarAlquileres :: Accion
cobrarAlquileres jugador = cambiarDinero (+((cantidadPropiedadesSegun (<150) $ propiedades jugador)*10 + (cantidadPropiedadesSegun (>=150) $ propiedades jugador)*20)) jugador

pagarAAccionistas :: Accion
pagarAAccionistas jugador | tactica jugador == "Accionista" = cambiarDinero (+200) jugador
                          | otherwise = cambiarDinero (+(-100)) jugador

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad jugador | dinero jugador >= snd propiedad = subastar propiedad jugador
                                    | otherwise = (hacerBerrinchePor propiedad).gritar.(cambiarDinero (+10)) $ jugador

ultimaRonda :: Accion
ultimaRonda jugador = foldl1 (.) (acciones jugador) $ jugador

juegoFinal :: Participante -> Accion
juegoFinal jugador1 jugador2 | (dinero.ultimaRonda) jugador1 > (dinero.ultimaRonda) jugador2 = ultimaRonda jugador1
                             | otherwise = ultimaRonda jugador2

--Participantes
carolina :: Participante
carolina = Participante{nombre = "Carolina", dinero = 500, tactica = "Accionista", propiedades = [("Avenida Las Heras",160)], acciones = [pasarPorElBanco, pagarAAccionistas]}

manuel :: Participante
manuel = Participante{nombre = "Manuel", dinero = 500, tactica = "Oferente singular", propiedades = [("Avenida Hipolito Yrigoyen",205)], acciones = [pasarPorElBanco, enojarse]}
