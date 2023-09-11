import Text.Show.Functions ()

type Nombre = String
type Habilidad = String
type Deseo = Chico->Chico 
type Condicion = Chico ->Bool

data Chico = Chico {

    nombre::Nombre,
    edad:: Int  ,
    habilidades:: [Habilidad],
    deseos:: [Deseo]
    
}deriving Show 

data Chica = Chica {

    nombreChica:: Nombre,
    condicion:: Condicion

}deriving Show 


modificarHabilidades::([Habilidad]->[Habilidad])->Chico->Chico
modificarHabilidades funcion unChico = unChico { habilidades = funcion . habilidades $ unChico }

aprenderHabilidades:: [Habilidad]-> Chico -> Chico
aprenderHabilidades habilidadess unChico = modificarHabilidades (++ habilidadess) unChico

serGroseroEnNeedForSpeed::Chico->Chico
serGroseroEnNeedForSpeed unChico = modificarHabilidades (++ agregarJuegos) unChico

agregarJuegos::[Habilidad]
agregarJuegos = map (\unNumero -> "jugar need for speed " ++ show unNumero) [1..]

serMayor :: Chico-> Chico
serMayor unChico = unChico {edad = 18}

wanda:: Chico->Chico
wanda unChico = cambiarEdad (+1).cumplirDeseo $ unChico

cambiarEdad::(Int ->Int)->Chico->Chico
cambiarEdad funtion unChico = unChico{edad =funtion.edad $ unChico}

cumplirDeseo:: Chico->Chico
cumplirDeseo unChico = head (deseos unChico) unChico 


cosmo :: Chico->Chico
cosmo unChico = cambiarEdad  (flip div 2) unChico

muffinMagico:: Chico -> Chico
muffinMagico unChico = foldr ($) unChico  (deseos unChico)  
 

tieneHabilidad :: Habilidad -> Chico ->Bool
tieneHabilidad unaHabilidad unChico = elem unaHabilidad (habilidades unChico)

esSuperMaduro:: Chico ->Bool
esSuperMaduro unChico = esMayordeEdad unChico && tieneHabilidad "saber manejar" unChico


esMayordeEdad :: Chico->Bool
esMayordeEdad unChico = (>18).edad $ unChico




timmy::Chico
timmy= Chico { nombre = "Timmy",edad= 15 , habilidades = ["saber nadar",  "saber multiplicar","matar"],deseos= [serMayor  ]}

mario::Chico
mario= Chico { nombre = "Mario",edad= 15 , habilidades = ["ser violinista", "ser un super modelo","dominar El Mundo", "enamorar"],deseos= [serMayor]}

trixie::Chica
trixie = Chica { nombreChica= "Trixie Tang",condicion = noesTimmy}

vicky::Chica
vicky = Chica {nombreChica = "Vicky",condicion = tieneHabilidad "ser un super modelo"}

dana::Chica 
dana = Chica { nombreChica= "Dana",condicion = tieneHabilidad "saber cocinar"}

noesTimmy::Condicion
noesTimmy unChico = nombre unChico /= "Timmy"


quienConquistarA:: Chica ->[Chico]->Chico
quienConquistarA unaChica (x:xs) = cumpleCondicion unaChica x xs 


cumpleCondicion:: Chica -> Chico ->[Chico]->Chico
cumpleCondicion unaChica unChico lista
  | (condicion unaChica) unChico = unChico 
  |  null lista                  = unChico
  | otherwise                    = quienConquistarA unaChica lista



--C Da Rules 

infractoresDeDaRules::[Chico]->[Nombre]
infractoresDeDaRules unosChicos = nombreDeInfractores.filter tieneDeseoProhibido $ unosChicos

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido unChico = any (esDeseoProhibidoPara unChico) . deseos $ unChico

esDeseoProhibidoPara :: Chico -> Deseo -> Bool
esDeseoProhibidoPara unChico unDeseo = tieneHabilidadesProhibidas $ unDeseo unChico


tieneHabilidadesProhibidas::Chico->Bool
tieneHabilidadesProhibidas unChico = any (esHabilidadProhibida ).take 5 .habilidades $ unChico

esHabilidadProhibida::Habilidad->Bool
esHabilidadProhibida  unahabilidad = elem unahabilidad habilidadesProhibidas

habilidadesProhibidas:: [Habilidad]
habilidadesProhibidas = ["enamorar","matar","dominar El Mundo"]

nombreDeInfractores::[Chico]->[Nombre]
nombreDeInfractores unosChicos = map nombre unosChicos



--







{-
----------------------------------------------------------------------------------------------------------------
data Ninja = Ninja {
  nombre       :: String,
  herramientas :: [Herramienta],
  jutsus       :: [Jutsu],
  rango        :: Int
} deriving Show

type Herramienta = (String, Int)

mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango unaFuncion uneNinja = uneNinja { rango = max 0 . unaFuncion . rango $ uneNinja }

mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta
mapCantidad unaFuncion (unNombre, unaCantidad) = (unNombre, unaFuncion unaCantidad)

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas unaFuncion uneNinja = uneNinja { herramientas = unaFuncion . herramientas $ uneNinja }

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

-- A.

-- A.a.

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta unaHerramienta uneNinja = agregarHerramienta (limitarCantidadA (cuantasHerramientasPuedeObtener uneNinja) unaHerramienta) uneNinja

limitarCantidadA :: Int -> Herramienta -> Herramienta
limitarCantidadA unaCantidad  = mapCantidad (min unaCantidad) 

agregarHerramienta :: Herramienta -> Ninja -> Ninja
agregarHerramienta unaHerramienta = mapHerramientas (unaHerramienta :)

cuantasHerramientasPuedeObtener :: Ninja -> Int
cuantasHerramientasPuedeObtener = (100 -) . cantidadDeHerramientas

cantidadDeHerramientas :: Ninja -> Int
cantidadDeHerramientas = sum . map snd . herramientas

-- A.b.

usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta unNombreDeHerramienta uneNinja = mapHerramientas (filter ((/= unNombreDeHerramienta) . nombreHerramienta)) uneNinja


--Pruebita
unNinja::Ninja
unNinja =Ninja{nombre="Ninja",herramientas=[("maso",3),("suriken",5)],jutsus=[clonesDeSombra 3,fuerzaDeUnCentenar],rango =23 }

otroNinja::Ninja
otroNinja = Ninja{nombre="Ninja2",herramientas=[("maso",8),("suriken",9)],jutsus=[clonesDeSombra 32],rango =42}
-- B

data Mision = Mision {
  cantidadDeNinjas :: Int,
  rangoRecomendado :: Int,
  ninjasEnemigos   :: [Ninja],
  recompensa       :: Herramienta
} deriving Show

type Equipo = [Ninja]

mision1::Mision
mision1 = Mision {cantidadDeNinjas=2,rangoRecomendado=20,ninjasEnemigos=[],recompensa=("bola de fuego",3)}

-- B.a.

esDesafiante :: Equipo -> Mision -> Bool
esDesafiante unEquipo unaMision = tieneMiembroNoCalificadoPara unEquipo unaMision && ((>= 2) . length . ninjasEnemigos) unaMision

tieneMiembroNoCalificadoPara :: Equipo -> Mision -> Bool
tieneMiembroNoCalificadoPara unEquipo unaMision = any (not . estaCalificadoPara unaMision) unEquipo

estaCalificadoPara :: Mision -> Ninja -> Bool
estaCalificadoPara unaMision uneNinja = rango uneNinja >= rangoRecomendado unaMision

-- B.b.

esCopada :: Mision -> Bool
esCopada = esRecompensaCopada . recompensa

esRecompensaCopada :: Herramienta -> Bool
esRecompensaCopada unaHerramienta = elem unaHerramienta recompensasCopadas

recompensasCopadas :: [Herramienta]
recompensasCopadas = [("Bomba de Humo", 3), ("Shuriken", 5), ("Kunai", 14)]

-- B.1.c.

esFactible :: Equipo -> Mision -> Bool
esFactible unEquipo unaMision = (not . esDesafiante unEquipo) unaMision && estaBienPreparadoPara unEquipo unaMision

estaBienPreparadoPara :: Equipo -> Mision -> Bool
estaBienPreparadoPara unEquipo unaMision = tieneSuficientesNinjasPara unEquipo unaMision || estanBienArmades unEquipo

tieneSuficientesNinjasPara :: Equipo -> Mision -> Bool
tieneSuficientesNinjasPara unEquipo unaMision = length unEquipo >= cantidadDeNinjas unaMision

estanBienArmades :: Equipo -> Bool
estanBienArmades = (> 500) . sum . map cantidadDeHerramientas


-- B.2.a.

fallarMision :: Mision -> Equipo -> Equipo
fallarMision unaMison unEquipo = map reducirRango . filter (estaCalificadoPara unaMison) $ unEquipo


reducirRango::Ninja ->Ninja
reducirRango ninja = mapRango (subtract 2 ) ninja
-- B.2.b.

cumplirMision :: Mision -> Equipo -> Equipo
cumplirMision unaMision = map (obtenerHerramienta (recompensa unaMision) . promover)  

promover :: Ninja -> Ninja
promover = mapRango succ


-- B.3.a.

type Jutsu = Mision -> Mision

clonesDeSombra :: Int -> Jutsu
clonesDeSombra = mapCantidadDeNinjas . subtract

mapCantidadDeNinjas :: (Int -> Int) -> Mision -> Mision
mapCantidadDeNinjas unaFuncion unaMision = unaMision { cantidadDeNinjas = max 1 . unaFuncion . cantidadDeNinjas $ unaMision }

-- B.3.b.

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar = mapNinjasEnemigos (filter ((>= 500) . rango))

mapNinjasEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapNinjasEnemigos unaFuncion unaMision = unaMision { ninjasEnemigos = unaFuncion . ninjasEnemigos $ unaMision }

ejecutarMision :: Equipo -> Mision -> Equipo
ejecutarMision unEquipo = completarMision unEquipo . usarTodosSusJutsus unEquipo

usarTodosSusJutsus :: Equipo -> Mision -> Mision
usarTodosSusJutsus unEquipo unaMision = foldr ($) unaMision . concatMap jutsus $ unEquipo

completarMision :: Equipo -> Mision -> Equipo
completarMision unEquipo unaMision
  | esCopada unaMision || esFactible unEquipo unaMision = cumplirMision unaMision unEquipo
  | otherwise                                           = fallarMision unaMision unEquipo

-- C.

granGuerraNinja :: Mision
granGuerraNinja = Mision {
  cantidadDeNinjas = 100000,
  rangoRecomendado = 100,
  ninjasEnemigos   = infinitosZetsus,
  recompensa       = ("Abanico de Uchiha Madara", 1)
}

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int -> Ninja
zetsu unNumero = Ninja {
  nombre       = "Zetsu " ++ show unNumero,
  rango        = 600,
  jutsus       = [],
  herramientas = []
}

-- C.a.

-- Si el equipo no tiene ningún miembro no calificado para la misión entonces termina de evaluar y devuelve False.
-- En caso contrario no termina de evaluar porque no se puede obtener la longitud de una lista infinita.
-- Se podría reescribir la segunda condición con drop de modo que pueda terminar de evaluar.

-- C.b.

-- Termina de evaluar y devuelve False ya que el abanico de uchiha madara no es una recompensa copada.

-- C.c.

-- Devuelve una misión con una lista infinita de ninjas enemigos.

{-
  En todos los casos la justificación es que dado que Haskell trabaja con evaluación perezosa no necesita evaluar
  la lista infinita de enemigos antes de empezar a evaluar la función. Esto permite que en los casos en los que no se
  necesita recorrer la lista entera de enemigos para llegar a un resultado se pueda dar una respuesta de todos modos.
-}



-}
