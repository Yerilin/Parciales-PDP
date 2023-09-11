import Text.Show.Functions ()
 
-- 1 -----------------------------------------------------------------
type Artefacto = (String,Int)
type Tarea = Heroe ->Heroe
type Cuadras= Int

data Heroe = Heroe {
    nombr:: String,
    epiteto :: String,
    reconocimiento :: Int,
    artefactos::[Artefacto],
    tareas :: [Tarea]
}deriving Show

data Bestia = Bestia {
    nombre :: String,
    debilidad :: Heroe ->Bool
}deriving Show


--{-
paseALaHistoria :: Heroe -> Heroe
paseALaHistoria unHeroe
 | (>1000). reconocimiento $ unHeroe                = cambiarArtefactos (const [] ). cambiarEpiteto "El mitico " $ unHeroe
 | (>499). reconocimiento $ unHeroe                 = guardarArtefactos ("lanza del olimpo",100) . cambiarEpiteto "El magnifico " $ unHeroe
 | esMayoryMenor 100 500 . reconocimiento $ unHeroe = guardarArtefactos ("Xiphos",50) . cambiarEpiteto "Hoplita" $ unHeroe
 | otherwise                                        = id unHeroe         

--}

esMayoryMenor:: Int->Int->Int ->Bool
esMayoryMenor unNumero otroNumero numero = numero > unNumero && numero < otroNumero


cambiarEpiteto::String->Heroe ->Heroe
cambiarEpiteto unEpiteto unHeroe = unHeroe{epiteto = unEpiteto}

cambiarArtefactos :: ([Artefacto]->[Artefacto])->Heroe->Heroe
cambiarArtefactos f unHeroe = unHeroe {artefactos= f.artefactos $ unHeroe}

encontrarUnArtefacto:: (String,Int)->Tarea
encontrarUnArtefacto unArtefacto unHeroe = cambiarReconocimiento (+ snd unArtefacto) .guardarArtefactos unArtefacto $ unHeroe

cambiarReconocimiento:: (Int->Int) -> Heroe ->Heroe
cambiarReconocimiento function unHeroe = unHeroe { reconocimiento= function.reconocimiento $ unHeroe}

guardarArtefactos ::( String,Int)->Heroe -> Heroe 
guardarArtefactos unArtefacto unHeroe = cambiarArtefactos (unArtefacto :) unHeroe


escalarElOlimpo :: Tarea
escalarElOlimpo unHeroe = guardarArtefactos ("Un relanpago de Zeus",500) .cambiarArtefactos (filter desecha ).cambiarReconocimientoyRareza $ unHeroe 

cambiarReconocimientoyRareza::Heroe->Heroe
cambiarReconocimientoyRareza unHeroe = cambiarReconocimiento (+500).cambiarArtefactos (map triplicaRareza ) $ unHeroe

triplicaRareza:: Artefacto->Artefacto
triplicaRareza (artefacto,rareza) =(artefacto,rareza*3) 

desecha:: Artefacto ->Bool
desecha unArtefacto = snd unArtefacto < 1000 

ayudarACruzarLaCalle:: Cuadras->Tarea
ayudarACruzarLaCalle cuadras unHeroe =agregarAlEpiteto (replicate cuadras "o").cambiarEpiteto "Gros" $ unHeroe

agregarAlEpiteto::[String]->Heroe ->Heroe
agregarAlEpiteto unaLista unHeroe = unHeroe{epiteto= epiteto unHeroe ++ concat unaLista}

matarUnaBestia :: Bestia ->Tarea
matarUnaBestia unaBestia unHeroe 
 | debilidad unaBestia unHeroe   =  cambiarEpiteto ("El asesino de " ++ nombre unaBestia)  unHeroe
 | otherwise                     = cambiarArtefactos tail.cambiarEpiteto "El Cobarde" $ unHeroe



heracles::Heroe  
heracles = Heroe {nombr= "Heracles",epiteto = "Guradian del Olimpo", reconocimiento = 700, artefactos= [("pistola", 1000),("relampago de Zeus",500)], tareas= [ayudarACruzarLaCalle 3 ,encontrarUnArtefacto ("soga  saltarina",500)]}


jime::Heroe  
jime = Heroe {nombr="Jimena",epiteto = "", reconocimiento = 700, artefactos= [("armadura", 1000),("alfobra Voladora",500)], tareas= [matarUnaBestia monstruo ,escalarElOlimpo ]}
--matarAlLeonDeNemea :: Bestia Tarea
--matarAlLeonDeNemea unaBestia unHeroe = matarUnaBestia 

monstruo :: Bestia
monstruo = Bestia {nombre= "monstruo", debilidad = reconocimientoEs (> 500) }

reconocimientoEs :: (Int -> Bool) -> Heroe -> Bool
reconocimientoEs   function unHeroe = function. reconocimiento $ unHeroe

haga :: Tarea -> Heroe -> Heroe
haga unaTarea unHeroe = agregarATareas [unaTarea] . unaTarea $ unHeroe

agregarATareas :: [Tarea] -> Heroe -> Heroe
agregarATareas tarea unHeroe = unHeroe {tareas = tareas unHeroe ++ tarea }

presuman:: Heroe->Heroe ->(String,String)
presuman unHeroe otroHeroe = ganador unHeroe otroHeroe 

ganador:: Heroe->Heroe->(String,String)
ganador unHeroe otroHeroe
 | (> reconocimiento otroHeroe).reconocimiento $ unHeroe = (epiteto unHeroe , epiteto otroHeroe)
 | ((== reconocimiento otroHeroe).reconocimiento $ unHeroe ) && mayorRareza unHeroe otroHeroe  = (epiteto unHeroe , epiteto otroHeroe)
 |  (< reconocimiento otroHeroe).reconocimiento $ unHeroe = (epiteto otroHeroe , epiteto unHeroe)
 | otherwise               = ganador (aplicarTareas unHeroe.tareas $ otroHeroe) (aplicarTareas otroHeroe.tareas $ unHeroe)



aplicarTareas :: Heroe->[Tarea] -> Heroe
aplicarTareas  unHeroe lista= foldr (haga) unHeroe lista 

mayorRareza:: Heroe ->Heroe -> Bool
mayorRareza unHeroe otroHeroe = sumatoriaRareza unHeroe > sumatoriaRareza otroHeroe 

sumatoriaRareza:: Heroe -> Int 
sumatoriaRareza unHeroe = sum .map snd .artefactos $ unHeroe

