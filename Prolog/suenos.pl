%Base De Conociemientos 

cree(gabriel,campanita).
cree(gabriel,magoDeOz).
cree(gabriel,cavenaghi).
cree(juan,conejoDePascua).
cree(macarena,reyesMagos).
cree(macarena,magoCapria).
cree(macarena,campanita).

%funtor
% tipoDeSueño( soñador, Sueño_caracteristica)

tieneSueno(gabriel,ganarLoteria([5,7])).
tieneSueno(gabriel,futbolista(arcenal)).
tieneSueno(juan,cantante(_,100000)).
tieneSueno(macarena,cantante(erucaSativa,10000)).

amigos(campanita,reyesMagos).
amigos(reyesMagos,campanita).
amigos(campanita,conejoDePascua).
amigos(conejoDePascua,campanita).
amigos(conejoDePascua,cavenaghi).
amigos(cavenaghi,conejoDePascua).

equipoChico(arcenal).
equipoChico(aldosivi).

enfermo(conejoDePascua).
enfermo(campanita).
enfermo(reyesMagos).

esAmbiciosa(Persona):-
    dificultadDeSuenos(Persona,Dificultad),
    Dificultad > 20.



dificultad(ganarLoteria(UnosNumeros),Dificultad):-
    length(UnosNumeros, Cantidad),
    Dificultad is Cantidad *10.

dificultad(cantante(_,Cantidad),Dificultad):-
    Cantidad > 500000,
    Dificultad is 6 .

dificultad(cantante(_,Cantidad),Dificultad):-
    Cantidad < 500000,
    Dificultad is 4.

dificultad(futbolista(Equipo),Dificultad):-
    not(equipoChico(Equipo)),
    Dificultad is 16 .

dificultad(futbolista(Equipo),Dificultad):-
    equipoChico(Equipo),
    Dificultad is 3.


dificultadDeSuenos(Persona,Dificultad):-
    tieneSueno(Persona,_),
    findall(UnaDificultad,(tieneSueno(Persona,Sueno),dificultad(Sueno,UnaDificultad)),Dificultades),
    sumlist(Dificultades,Dificultad).




tieneQuimica(Persona,Personaje):-
    cree(Persona,Personaje),
    Personaje \= campanita,
    forall(tieneSueno(Persona,Sueno),suenoPuro(Sueno)),
    not(esAmbiciosa(Persona)).


tieneQuimica(Persona,campanita):-
    cree(Persona,campanita),
    tieneSueno(Persona,Sueno),
    dificultad(Sueno,Dificultad),
    Dificultad < 5.


suenoPuro(futbolista).
suenoPuro(cantante(_,Cantidad)):-
    Cantidad < 200000.



puedeAlegrar(_,Persona):-
    tieneSueno(Persona,_).

puedeAlegrar(Personaje,Persona):-
    tieneQuimica(Persona,Personaje),
    not(enfermo(Personaje)).

puedeAlegrar(PersonajePrincipal,Persona):-
    tieneQuimica(Persona,PersonajePrincipal),
    findall(OtroPersonaje,esAmigo(PersonajePrincipal,OtroPersonaje),Backup),
    personaje(Personaje),
    not(enfermoAlgunPersonaje(Backup,Personaje)).

enfermoAlgunPersonaje(Personajes,Personaje):-
    member(Personaje,Personajes),
    enfermo(Personaje).
    


esAmigo(Personaje,OtroPersonaje):-
    amigos(Personaje,OtroPersonaje).

esAmigo(Personaje,UnPersonaje):-
    amigos(Personaje,OtroPersonaje),
    amigos(OtroPersonaje,UnPersonaje).
    
    

personaje(Personaje):-
    cree(_,Personaje).