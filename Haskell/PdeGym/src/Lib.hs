import Text.Show.Functions ()


type Nombre = String
type Equipamiento = String

data Persona = Persona {

    nombre:: Nombre,
    calorias:: Int,
    hidratacion:: Int,
    tiempo:: Int,
    equipamientos::[Equipamiento]
} deriving (Show)

type Ejercicio = Persona->Persona 

abdominales:: Int->Ejercicio
abdominales repeticiones unaPersona = perderCalorias (repeticiones*8 ) unaPersona

perderCalorias ::Int->Ejercicio
perderCalorias unasCalorias unaPersona = cambiarCalorias (subtract unasCalorias) unaPersona  

cambiarCalorias:: (Int->Int)->Ejercicio
cambiarCalorias function unaPersona = unaPersona{calorias=function.calorias $ unaPersona}

flexiones::Int-> Persona ->Persona 
flexiones repeticiones unaPersona = pierdeHidratacion(repeticiones `div` 10  * 2).perderCalorias (repeticiones*16) $ unaPersona
                                 -- seria igual poner : (repeticones `div` 10) * 2

pierdeHidratacion::Int->Ejercicio
pierdeHidratacion unaCantidad unaPersona = cambiarHidratacion (subtract unaCantidad) unaPersona

cambiarHidratacion:: (Int->Int)->Ejercicio 
cambiarHidratacion function unaPersona = unaPersona{hidratacion= max 0 . min 100 .function.hidratacion $ unaPersona}


levantarPesas :: Int -> Int ->Ejercicio
levantarPesas repeticiones unaPesa unaPersona 
 | existeElemento "pesa" unaPersona = perderCalorias(repeticiones*32).pierdeHidratacion(repeticiones `div` 10  * unaPesa ) $ unaPersona
 | otherwise                        = unaPersona 

existeElemento :: String ->Persona ->Bool
existeElemento unEquipamiento unaPersona = elem unEquipamiento . equipamientos $ unaPersona 


laGranHomeroSimpson:: Ejercicio
laGranHomeroSimpson = id 


renovarEquipo::Persona->Persona
renovarEquipo unaPersona = cambiarEquipamiento (map ("Nuevo " ++)) unaPersona

cambiarEquipamiento::([Equipamiento]->[Equipamiento])->Persona->Persona
cambiarEquipamiento f unaPersona = unaPersona{equipamientos=f.equipamientos $ unaPersona}


volverseYoguista::Persona->Persona
volverseYoguista unaPersona = quedarseSoloConColchoneta.cambiarCalorias (flip div 2).cambiarHidratacion (*2) $ unaPersona


quedarseSoloConColchoneta::Persona->Persona
quedarseSoloConColchoneta unaPersona = cambiarEquipamiento (const ["colchoneta"]) unaPersona


volverseBodyBuilder::Persona->Persona
volverseBodyBuilder unaPersona 
 | tieneSoloPesas unaPersona = agregarAlFinalDelNombre ("BB") . cambiarCalorias (*3) $ unaPersona
 | otherwise                 = unaPersona


tieneSoloPesas :: Persona-> Bool
tieneSoloPesas unaPersona = all(=="pesa").equipamientos $ unaPersona

agregarAlFinalDelNombre:: String->Persona->Persona
agregarAlFinalDelNombre  algo unaPersona = unaPersona{nombre= nombre unaPersona ++ algo }

comerUnSandwich::Persona->Persona
comerUnSandwich unaPersona = cambiarCalorias (+500).cambiarHidratacion (+100) $ unaPersona



--Parte B
data Rutina = UnaRutina {
    tiempoAproximado :: Int,
    ejercicios::[Ejercicio]
}deriving (Show)

hacerRutina::Rutina->Persona->Persona
hacerRutina  unaRutina unaPersona 
 | puedeHacerRutina unaPersona unaRutina = foldr ($) unaPersona (ejercicios unaRutina)
 | otherwise                            = unaPersona

puedeHacerRutina ::  Persona->Rutina ->Bool
puedeHacerRutina unaPersona unaRutina  = tiempo unaPersona > tiempoAproximado unaRutina

esPeligrosa::Rutina->Persona->Bool
esPeligrosa unaRutina unaPersona= personaAgotada . hacerRutina unaRutina $ unaPersona

personaAgotada::  Persona  ->Bool
personaAgotada  unaPersona  = calorias unaPersona < 50  && hidratacion unaPersona < 10 

esBalanceada :: Rutina->Persona->Bool
esBalanceada unaRutina unaPersonaAntes  = seraBalanceada unaPersonaAntes.hacerRutina unaRutina $ unaPersonaAntes 

seraBalanceada::Persona->Persona -> Bool
seraBalanceada personaAntes personaEjercitada = hidratacion personaEjercitada > 80 && calorias personaEjercitada< calorias personaAntes `div` 2



elAbominableAbdominal:: Rutina 
elAbominableAbdominal = UnaRutina{tiempoAproximado= 1 ,ejercicios = map abdominales [1..]}



--Parte C

seleccionarGrupoDeEjercicio:: Persona -> [Persona]->[Persona]
seleccionarGrupoDeEjercicio unaPersona grupoDePersonas = filter (tieneMismoTiempoDisponible .tiempo $ unaPersona) grupoDePersonas

tieneMismoTiempoDisponible:: Int -> Persona -> Bool
tieneMismoTiempoDisponible unTiempo unaPersona = tiempo unaPersona == unTiempo 


promedioDeRutina:: Rutina->[Persona]->(Int,Int)
promedioDeRutina unaRutina grupo = estadisticasDelgrupo.map (hacerRutina unaRutina) $ grupo

estadisticasDelgrupo:: [Persona]->(Int,Int)
estadisticasDelgrupo grupo = (promedioDe calorias grupo , promedioDe hidratacion grupo)


promedioDe::(b->Int)->[b]->Int
promedioDe f  grupo = promedio.map f $ grupo

promedio::[Int]-> Int
promedio valores = sum valores `div` length valores
--Ejemplos

yerilin :: Persona
yerilin = Persona {nombre="yerilin", calorias=100, hidratacion=50, tiempo=60, equipamientos = ["pesa", "colchoneta"]}

