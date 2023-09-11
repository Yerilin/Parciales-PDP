module Parcial where
import Text.Show.Functions()

--Parte A
type Equipamento = Personaje -> Personaje 
type Año = Int 
--1
type Victoria = (String , Int)
data  Personaje = Personaje {

    nombre :: String,
    poder :: Int,
    victorias:: [Victoria],
    equipamentos:: [Equipamento] -- Punto B -1 
}deriving Show




-- 2
entrenamiento::[Personaje]->[Personaje]
entrenamiento personajes = cambiarPoderes (length personajes) personajes

cambiarPoderes:: Int->[Personaje]->[Personaje]
cambiarPoderes  unNumero unosPersonajes = map (cambiarPoder unNumero (*) ) unosPersonajes

cambiarPoder :: Int ->(Int->Int->Int) -> Personaje -> Personaje 
cambiarPoder unNumero function unPersonaje = unPersonaje {poder = function unNumero.poder $ unPersonaje}

poderMayorA :: Int -> Personaje -> Bool
poderMayorA unNumero unPersonaje = poder unPersonaje > unNumero 

--3
rivalesDignos:: [Personaje]-> [Personaje]
rivalesDignos unosPersonajes = (tienenVictorias "Hijo de Thanos "). dignos.entrenamiento $ unosPersonajes

dignos:: [Personaje]->[Personaje]
dignos unosPersonajes = filter (poderMayorA 500) unosPersonajes  -- ser más declarativa con las funciones

tienenVictorias:: String -> [Personaje]->[Personaje]
tienenVictorias unaPalabra unosPersonajes = filter (tienenVictoria unaPalabra) unosPersonajes

tienenVictoria:: String -> Personaje->Bool
tienenVictoria unaPalabra unPersonaje = elem unaPalabra . map fst .victorias $ unPersonaje 

--4
guerraCivil :: Int->[Personaje]->[Personaje]->[Personaje]
guerraCivil unAño unosPersonajes otrosPersonajes = zipWith (ganador unAño) unosPersonajes otrosPersonajes



ganador:: Año->Personaje -> Personaje -> Personaje 
ganador unAño unPersonaje otroPersonaje 
 | poder unPersonaje >= poder otroPersonaje = agregarAlistadeVictoria (nombre otroPersonaje) unAño unPersonaje 
 | otherwise                                  = agregarAlistadeVictoria (nombre  unPersonaje) unAño otroPersonaje


agregarAlistadeVictoria :: String ->Año-> Personaje -> Personaje 
agregarAlistadeVictoria unNombre unAño unPersonaje = unPersonaje {victorias = (unNombre,unAño): victorias unPersonaje }


jose::Personaje
jose  = Personaje {nombre = "Jose ", poder= 23 , victorias=[("Juan",2020)], equipamentos=[]}


--Punto B-----------------------------------------------------------------------------------------------------------------------------------
---1 hecho arriba
---2

escudo::Equipamento 
escudo unPersonaje 
  | cantidadDeVictorias unPersonaje < 5  = cambiarPoder 50 (+) unPersonaje
  | otherwise                             = cambiarPoder 100 subtract unPersonaje 



cantidadDeVictorias:: Personaje -> Int 
cantidadDeVictorias unPersonaje = length.victorias $ unPersonaje 

 

trajeMecanizado::Int->Equipamento
trajeMecanizado unaVersion unPersonaje = agregarUnaVersion unaVersion. agregarAdelanteDelNombre "Iron" $ unPersonaje

agregarAdelanteDelNombre:: String -> Personaje ->Personaje 
agregarAdelanteDelNombre palabra unPersonaje = unPersonaje {nombre= palabra ++ nombre unPersonaje}

agregarUnaVersion:: Int-> Personaje -> Personaje 
agregarUnaVersion unNumero unPersonaje = unPersonaje {nombre = nombre unPersonaje ++ ("V" ++ show unNumero) }


--3 

equipamientoExclusivo:: String -> (Personaje->Personaje) -> Personaje ->Personaje
equipamientoExclusivo unNombreExclusivo unaAccion unPersonaje 
 | (nombre unPersonaje) == unNombreExclusivo = unaAccion unPersonaje 
 | otherwise                                 = unPersonaje 


limpiarVictorias::Personaje -> Personaje 
limpiarVictorias unPersonaje = unPersonaje { victorias = []}

hacerDiosDelTrueno :: Personaje -> Personaje 
hacerDiosDelTrueno unPersonaje = unPersonaje { nombre = nombre unPersonaje ++ "dios del trueno"}

--a 

stormBreaker :: Equipamento
stormBreaker unPersonaje = equipamientoExclusivo "Thor" (hacerDiosDelTrueno.limpiarVictorias $ ) unPersonaje

--b
gemaDelAlma:: Equipamento
gemaDelAlma unPersonaje = equipamientoExclusivo "Thanos" añadirExtras unPersonaje 


todosLosExtras::[String]
todosLosExtras = map (\unNumero -> "extra numero " ++show unNumero ) [1..]

todosLosAños::[Año]
todosLosAños = [2018 ..]

todaLaListaDeExtras::[Victoria]
todaLaListaDeExtras = zip todosLosExtras todosLosAños

añadirExtras:: Personaje ->Personaje 
añadirExtras unPersonaje = unPersonaje{ victorias= victorias unPersonaje ++ todaLaListaDeExtras}


--c 

guanteleteInfinito:: Equipamento
guanteleteInfinito unPersonaje = equipamientoExclusivo "Thanos" (aplicarGemasDelInfinito ) unPersonaje

aplicarGemasDelInfinito :: Personaje -> Personaje 
aplicarGemasDelInfinito unPersonaje = foldr (($)) unPersonaje (hallarLasGemas unPersonaje) 

                                              -- unPersonaje.hallarLasgemas $ unPersonaje    

-- otra manera de hacerlo asociativa hacia derecha :

--aplicarGemasDelInfinito :: Personaje -> Personaje 
--aplicarGemasDelInfinito unPersonaje = foldr (flip ($)) unPersonaje (hallarLasGemas unPersonaje) 

hallarLasGemas :: Personaje -> [Equipamento]
hallarLasGemas unPersonaje = filter esGemaDelInfinito (equipamentos unPersonaje )

esGemaDelInfinito::Equipamento -> Bool
esGemaDelInfinito = undefined 



{- Parte C 
  
  Black Widow tiene infinitas victorias 

 a -¿Que pasaria si le decimos que utilice el Escudo? .Justificá
    al tener infinitas victorias nunca la funcion escudo podra saber cuantas victorias tendra 
    por lo que la funcion nunca terminara de evaluar.

 b -¿Que pasará si al usar rivalesDignos , blackWidow formase parte de la lista que pasamos por parametro ? .Justificá
    Si el caso de que en la lista infinita de victorias formase "Hijos de Thanos " la función talves tardaria
    en encontrarlo  pero igual compilaria , en el caso de que no estuviera en victorias "Hijos de Thanos " la
    funcion nunca terminaria de evaluar por lo cual nunca encontrara la lista de rivalesDignos.

 c -¿Podemos conseguir las primeras 100 victorias de Thanos luego de usar la gema del alma ? Justificá   
    Se, se puede por que solo evaluamos las 100 primeras victorias y no todo la lista en si 


 -}