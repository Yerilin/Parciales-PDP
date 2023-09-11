%Base de Conocimientos 

% vocaloid(nombre,cancion(nombre,minutos))
%cancion(nombre,minutos)

canta(megurineLuka,cancion(nightFever,4)).
canta(megurineLuka,cancion(foreverYoung,5)).
canta(hatsuneMiku,cancion(tellYourdWorld,4)).
canta(gumi,cancion(foreverYoung,4)).
canta(gumi,cancion(tellYourWorld,5)).
canta(seeU,cancion(novemberRain,6)).
canta(seeeu,cancion(nightFever,5)).

conoce(magurineLuka,hatsuneMiku).
conoce(megurineLuka,gumi).
conoce(gumi,seeU).
conoce(seeU,kaito).

novedoso(Cantante):-
    sabeAlmenosDosCanciones(Cantante),
    tiempoTotalDeCanciones(Cantante,Tiempo),
    Tiempo<15.

sabeAlmenosDosCanciones(Cantante):-
    canta(Cantante,Cancion),
    canta(Cantante,OtraCancion),
    Cancion\=OtraCancion.

tiempoTotalDeCanciones(Cantante,TiempoTotal):-
    findall(TiempoCancion,tiempoDeCancion(Cantante,TiempoCancion),ListaDeTiempos),
    sumlist(Tiempos,TiempoTotal).

tiempoDeCancion(Cantante,TiempoCancion):-
    canta(Cantante,Cancion),
    tiempo(Cancion,TiempoCancion).


tiempo(Cancion,Tiempo):-
    tiempo(cancion(_,Tiempo),Tiempo).



%2--

acelerado(Cantante):-
    vocaloid(Cantante),
    not((tiempoDeCancion(Cantante,TiempoCancion),TiempoCancion>4)).


vocaloid(Cantante):-
    canta(Cantante,_).  


% concierto(nombre,pais,cantidad de fama, tipo de concierto)

concierto(mikuExpo,eeuu,2000,gigante(2,6)).
concierto(magicalMirai,japon,3000,gigante(3,10)).
concierto(vocalektVisions,eeuu,1000,mediano(9)).
concierto(mikuFest,argentina,100,pequena(4)).


puedeParticipar(hatsuneMiku,Concierto):-
    concierto(Concierto,_,_,_).


puedeParticipar(Cantante,Concierto):-
    vocaloid(Cantante),
    condicionConcierto(Concierto,Condicion),
    cumpleCondicion(Cantante,Condicion).


condicionConcierto(Concierto,Condicion):-
    concierto(_,_,_,Condicion).

cumpleCondicion(Cantante,gigante(CantidadCanciones,Minimo)):-
    cantidadDeCanciones(Cantante,Cantidad),
    Cantidad >= CantidadCanciones,
    tiempoTotalDeCanciones(Cantante,TiempoTotal),
    TiempoTotal > Minimo.



cantidadDeCanciones(Cantante,Cantidad):-
    findall(TiempoCancion,tiempoDeCancion(Cantante,TiempoCancion),Tiempos),
    length(Tiempos,Cantidad).

cumpleCondicion(Cantante,mediano(TiempoMenor)):-
    tiempoTotalDeCanciones(Cantante,TiempoTotal),
    TiempoTotal < TiempoTotal.

cumpleCondicion(Cantante,pequena(MinimoDeCancion)):-
    canta(Cantante,Cancion),
    tiempo(Cancion,Tiempo),
    Tiempo>MinimoDeCancion.



masFamoso(Cantante):-
    vocaloid(Cantante),
    nivelDeFama(Cantante,NivelMasFamoso),
    forall(nivelDeFama(_,NivelOtro),NivelMasFamoso>NivelOtro).
   

nivelDeFama(Cantante,Nivel):-
    findall(Fama,(puedeParticipar(Cantante,Concierto),famaDeUnConcierto(Concierto,Fama)),famaDeConciertos),
    sumlist(famaDeConciertos,FamaTotal),
    cantidadDeCanciones(Cantante,Cantidad),
    Nivel is Cantidad*FamaTotal.
    


famaDeUnConcierto(Concierto,Fama):-
     concierto(Concierto,_,Fama,_).


unicoEnElConcierto(Cantante,Concierto):-
    puedeParticipar(Cantante,Concierto).
    not((conocido(Cantante,OtroCantante),puedeParticipar(OtroCantante,Caoncierto))).

conocido(Cantante,OtroCantante):-
    conoce(Cantante,OtroCantante).


conocido(Cantante,OtroCantante):-
    conoce(Cantante,UnCantante),
    conocido(UnCantante,OtroCantante).

%Nose muy bien como funciona este ripo de Recursividad 