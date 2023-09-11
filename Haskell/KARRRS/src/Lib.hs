import Text.Show.Functions()
type Publico = String 
type Pista = Int 
data Carrera = Carrera {

    numeroDeVuelta :: Int,
    longitud :: Pista ,
    participantes:: [Auto],
    publico :: [Publico]
} deriving Show 

type Truco = Carrera->Auto -> Auto 

data Auto = Auto {

    nombre::String,
    nafta:: Int,
    velocidad:: Int,
    enamorad:: String,
    truco::Truco 
}deriving Show 


deReversaRocha:: Truco 
deReversaRocha carrera unAuto = cambiarNafta  (+ (longitud carrera  *5)) unAuto

cambiarNafta:: (Int->Int )->Int ->Auto -> Auto 
cambiarNafta funtion unNumero unAuto = unAuto {nafta = funtion .nafta $ unAuto }

impresionar ::Truco
impresionar carrera unAuto  
 | estaSuEnamorada publico carrera unAuto = modificarVelocidad (*2) unAuto
 | otherwise                        = unAuto 

estaSuEnamorada:: Carrera -> Auto -> Bool
estaSuEnamorada carrera unAuto = elem (enamorad unAuto) . publico carrera

modificarVelocidad :: (Int->Int)-> Auto -> Auto 
modificarVelocidad function unAuto = unAuto { velocidad= function.velocidad $ unAuto}


nitro:: Truco 
nitro carrrera unAuto = modificarVelocidad (+15) unAuto 

comboLoco::  Truco 
comboLoco carrera unAuto =  nitro.deReversaRocha $ carrera unAuto 



rochaMcQueen:: Auto
rochaMcQueen = Auto {nombre = "RochaMcQueen ", nafta = 282 ,velocidad = 20 , enamorad= "Ronco",truco = deReversaRocha }

blankerr:: Auto
blankerr = Auto {nombre = "Blankerr ", nafta = 378 ,velocidad = 0 , enamorad= "Tincho",truco = impresionar }

rodra:: Auto
rodra = Auto {nombre = "Rodra ", nafta = 153 ,velocidad = 0 , enamorad= "Ronco",truco = comboLoco }

gushtav:: Auto
gushtav = Auto {nombre = "Gushtav ", nafta = 230 ,velocidad = 0 , enamorad= "Peti",truco = nitro }


darVuelta :: Carrera -> Carrera
darVuelta unaCarrera = unCarrera {participantes = } 

 

correrUnAuto:: Auto -> Carrera -> Auto 
correrUnAuto unAuto unaCarrera = cambiarNafta (subtract) (* longitudDeNombre unAuto ) .longitud $ unaCarrrae  unAuto


longitudDeNombre:: Auto -> Auto 
longitudDeNombre unAuto = lenght nombre unAuto   