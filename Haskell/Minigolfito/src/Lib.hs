import Text.Show.Functions ()


data Jugador = UnJugador {

    nombre:: String,
    padre:: String,
    habilidad::Habilidad

}deriving (Eq,Show)
 

data Habilidad = Habilidad {

    fuerzaJugador:: Int,
    precisionJugador:: Int

}deriving (Eq,Show)


data Tiro = UnTiro {
    velocidad :: Int,
    precision:: Int,
    altura :: Int
}deriving (Eq,Show)

type Puntos = Int
type Palo = Habilidad->Tiro


--bart :: Jugador
--bart = UnJugador {nombre="Bart",padre="Homero", habilidad= Habilidad {fuerzaJugador=25,precisionJugador=60} }

bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60 )



between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]



maximoSegun :: Ord b => (a->b)->[a]->a
maximoSegun f = foldl1 ( mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> (t -> t -> t)
mayorSegun f a b 
    | f a > f b = a 
    | otherwise = b 
    
    
--1a
putter::Habilidad->Tiro
putter unaHabilidad = UnTiro {
    velocidad= 10 , 
    precision= (*2).precisionJugador $ unaHabilidad, 
    altura=0}

madera::Habilidad->Tiro
madera unaHabilidad = UnTiro{
    velocidad = 100 ,
    precision= flip div 2 . precisionJugador $ unaHabilidad , 
    altura = 5}

hierro::Int->Habilidad->Tiro
hierro n unaHabilidad = UnTiro {
    velocidad= (*2).fuerzaJugador $ unaHabilidad,
    precision= flip div n . precisionJugador $ unaHabilidad ,
    altura= min 0 .subtract 3 $ n}

--1b
palos::[Palo]
palos = [putter,madera] ++ map (\unNumero-> hierro unNumero)[1..10]


--2

golpe::Jugador-> Palo->Tiro 
golpe unJugador unPalo = unPalo .habilidad $ unJugador 

--3 reconstruirTodo como un DATA
data Obstaculo = UnObstaculo{
    puedeSuperar :: Tiro->Bool,
    efectoDeSuperar :: Tiro->Tiro
}

tiroDetenido:: Tiro->Tiro
tiroDetenido tiro = tiro{velocidad=0,precision=0,altura=0}

tunelConRampita::Obstaculo
tunelConRampita  =  UnObstaculo superaTunelConRampita efectoTunelRampita 

superaTunelConRampita::Tiro->Bool
superaTunelConRampita tiro = velocidad tiro > 90   && vaRasDelSuelo tiro

vaRasDelSuelo ::Tiro->Bool
vaRasDelSuelo tiro = altura tiro == 0 

efectoTunelRampita::Tiro->Tiro
efectoTunelRampita tiro = tiro{velocidad=(*2).velocidad $ tiro, precision = 100,altura=0}

laguna::Int->Obstaculo 
laguna largoDeLaguna = UnObstaculo superaLaguna (efectoLaguna largoDeLaguna) 

superaLaguna::Tiro->Bool
superaLaguna tiro = velocidad tiro >80 && (between 1 5 .altura) tiro

efectoLaguna::Int->Tiro->Tiro
efectoLaguna largo tiroO = tiroO{altura=altura tiroO `div`largo}


hoyo::Obstaculo
hoyo= UnObstaculo superaHoyo efectoHoyo 

superaHoyo :: Tiro->Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && (vaRasDelSuelo tiro)    

efectoHoyo::Tiro->Tiro
efectoHoyo tiro = tiroDetenido tiro

esSuperable :: Obstaculo -> Tiro->Tiro
esSuperable obstaculo tiro 
 | puedeSuperar obstaculo tiro = efectoDeSuperar obstaculo tiro
 | otherwise                   = tiroDetenido tiro 

---4



palosUtiles::Jugador->Obstaculo->[Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar::Jugador->Obstaculo->Palo->Bool
leSirveParaSuperar jugador obstaculo unPalo = puedeSuperar obstaculo (golpe jugador unPalo)


cuantosPuedeSuperar::[Obstaculo]->Tiro-> Int 
cuantosPuedeSuperar  obstaculos tiro  = (length.takeWhile (\(obstaculo , tiroQueLlegua) -> puedeSuperar obstaculo tiroQueLlegua).zip obstaculos. tail. tirosSucesivos tiro )  obstaculos


tirosSucesivos::  Tiro->[Obstaculo] -> [Tiro]
tirosSucesivos tiroOriginal obstaculos   = 
    foldl (\ tiroGenerado obstaculo -> tiroGenerado ++ [ efectoDeSuperar obstaculo (last tiroGenerado)]) [tiroOriginal] obstaculos


paloMasUtil::Jugador->[Obstaculo]->Palo
paloMasUtil jugador obstaculos =  maximoSegun (cuantosPuedeSuperar obstaculos.golpe jugador ) palos

--5 


puntosGanados :: (a, b) -> b
puntosGanados = snd
jugadorDelTorneo :: (a, b) -> a
jugadorDelTorneo = fst
type PuntosTorneo = (Jugador,Puntos)

padresQuePierdenLaApuesta::[PuntosTorneo]->[String]
padresQuePierdenLaApuesta puntosDeTorneo = map (padre.fst).filter (not . ganoTorneo puntosDeTorneo ) $ puntosDeTorneo

ganoTorneo :: [PuntosTorneo] -> PuntosTorneo -> Bool
ganoTorneo puntosDelTorneo jugador = all ((<puntosGanados jugador).puntosGanados). filter (/= jugador) $ puntosDelTorneo