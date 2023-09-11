% Base de Conocimientos

%Parte 1-Magos
%Funtor

casa(gryffindor).
casa(slytherin).
casa(ravenclaw).
casa(hufflepuff).


odiariaEntrar(harry,slytherin).
odiariaEntrar(draco,hufflepuff).


sangre(harry,mestiza).
sangre(hermione,impura).
sangre(draco,pura).

caracteristica(harry,corajudo).
caracteristica(harry,amistoso).
caracteristica(harry,orgulloso).
caracteristica(harry,inteligente).
caracteristica(draco,inteligente).
caracteristica(draco,orgulloso).
caracteristica(hermione,inteligente).
caracteristica(hermione,orgulloso).
caracteristica(hermione,responsable).

caracteristicaBuscada(gryffindor, coraje).
caracteristicaBuscada(slytherin, orgullo).
caracteristicaBuscada(slytherin, inteligencia).
caracteristicaBuscada(ravenclaw, inteligencia).
caracteristicaBuscada(ravenclaw, responsabilidad).
caracteristicaBuscada(hufflepuff, amistad).

mago(Mago):-
    sangre(Mago,_).


permiteEntrar(Casa,Mago):-
    casa(Casa),
    mago(Mago),
    Casa\=slytherin.

permiteEntrar(slytherin,Mago):-
    sangre(Mago,Tipo),
    Tipo \= impura.

caracterApropiado(Casa,Mago):-
    mago(Mago), casa(Casa),
    forall(caracteristicaBuscada(Casa,Caracteristica),
          caracteristica(Mago,Caracteristica)).



% 3- 
podriaQuedar(Mago,Casa):-
    caracterApropiado(Casa,Mago),
    permiteEntrar(Casa,Mago),
    not(odiariaEntrar(Mago,Casa)).

podriaQuedar(hermione,gryffindor).

%cadenaDeAmistades(Magos):-
    %todosSonMagos(Magos),


%todosSonMagos(Magos):-
    %forall(tieneMago(Magos,Mago),mago(Mago))


%tieneMago(Magos,Mago):-
    