% BASE DE CONOCIMIENTOS

% escribio(Autor, Obra) -> hecho (hay 24 claúsulas)
escribio(elsaBornemann, socorro).
escribio(neilGaiman, sandman).
escribio(alanMoore, watchmen).
escribio(brianAzarello, cienBalas).
escribio(warrenEllis, planetary).
escribio(frankMiller, elCaballeroOscuroRegresa).
escribio(frankMiller, batmanAnioUno).
escribio(neilGaiman, americanGods).
escribio(neilGaiman, buenosPresagios).
escribio(terryPratchett, buenosPresagios).
escribio(isaacAsimov, fundacion).
escribio(isaacAsimov, yoRobot).
escribio(isaacAsimov, elFinDeLaEternidad).
escribio(isaacAsimov, laBusquedaDeLosElementos).
escribio(joseHernandez, martinFierro).
escribio(stephenKing, it).
escribio(stephenKing, misery).
escribio(stephenKing, carrie).
escribio(stephenKing, elJuegoDeGerald).
escribio(julioCortazar, rayuela).
escribio(jorgeLuisBorges, ficciones).
escribio(jorgeLuisBorges, elAleph).
escribio(horacioQuiroga, cuentosDeLaSelva).
escribio(horacioQuiroga, cuentosDeLocuraAmorYMuerte).

% Agregamos qué obras son cómics.

esComic(sandman).
esComic(cienBalas).
esComic(watchmen).
esComic(planetary).
esComic(elCaballeroOscuroRegresa).
esComic(batmanAnioUno).

% Queremos saber si alguien es artista del noveno arte (comics).
% Lo resolvemos por comprensión.
% esArtistaDelNovenoArte/1 predicado con dos claúsulas (una regla y un hecho).
/*
% p ^ q => r -> en discreta
% r <= p ^ q -> en lógico
*/
esArtistaDelNovenoArte(Artista) :- % regla
  escribio(Artista, Obra),
  esComic(Obra).

% En lógico no asignamos. Sí ligamos o unificamos, por eso no hace falta decir que la "Obra" es la misma.

% Y si también queremos aclarar que Art Spiegelman es un artista del noveno arte, hacemos:

esArtistaDelNovenoArte(artSpiegelman). % hecho

% Un artista es reincidente si escribió al menos 2 obras.

esReincidente(Artista) :-
  escribio(Artista, UnaObra),
  escribio(Artista, OtraObra),
  UnaObra \= OtraObra.

% Un predicado inversible me permite realizar consultas existenciales (pasar un _ o una variable sin ligar), además de las invididuales.

% Una obra le gusta a Gus si es Sandman o la escribió Isaac Asimov. Tiene que ser inversible.

% Alternativa 2
leGustaAGus(sandman).

leGustaAGus(Obra):-
  escribio(isaacAsimov, Obra).

% Alternativa 3
leGusta(gus, sandman).

leGusta(gus, Obra) :-
  escribio(isaacAsimov, Obra).

%Ambas alternativas son igual de válidas. La única diferencia es que en una se modela a gus como un individuo y en la otra es el nombre del predicado el que indica a quién le gusta la obra.

% Una obra es un libro cuando NO es un comic.
% not no es inversible!! No puede ligar las variables dentro de su predicado.
% Si a esLibro no le agregamos el generador, no sería un predicado inversible porque el not no es inversible.
% El generador nos "achica" el universo de opciones.

esLibro(Obra) :-
  esObra(Obra), % generador para que esLibro sea inversible
  not(esComic(Obra)).

esObra(Obra) :-
  escribio(_, Obra).

% Una obra es un bestseller si vendió mas de 50mil copias.

% copiasVendidas(Obra,Cantidad)

copiasVendidas(socorro, 10000).
copiasVendidas(sandman, 20000).
copiasVendidas(watchmen, 30000).
copiasVendidas(cienBalas, 40000).
copiasVendidas(planetary, 50000).
copiasVendidas(elCaballeroOscuroRegresa, 60000).
copiasVendidas(batmanAnioUno, 70000).
copiasVendidas(americanGods, 80000).
copiasVendidas(buenosPresagios, 90000).
copiasVendidas(buenosPresagios, 10000).
copiasVendidas(fundacion, 20000).
copiasVendidas(yoRobot, 30000).
copiasVendidas(elFinDeLaEternidad, 30000).
copiasVendidas(laBusquedaDeLosElementos, 40000).
copiasVendidas(martinFierro, 50000).
copiasVendidas(it, 60000).
copiasVendidas(it, 70000).
copiasVendidas(misery, 70000).
copiasVendidas(carrie, 80000).
copiasVendidas(elJuegoDeGerald, 90000).
copiasVendidas(rayuela, 10000).
copiasVendidas(ficciones, 20000).
copiasVendidas(elAleph, 30000).
copiasVendidas(cuentosDeLaSelva, 40000).
copiasVendidas(cuentosDeLocuraAmorYMuerte, 50000).

% Una obra es un bestseller si vendió mas de 50mil copias.

% Opción 1
% esBestseller(Obra) :-
%   copiasVendidas(Obra, 50000).

% Opción 2
esBestseller(Obra) :-
  copiasVendidas(Obra, CantidadDeCopias),
  CantidadDeCopias > 50000.


% Conviene contratar un artista si:
% escribió un bestseller o
% es reincidente.
% Tiene que ser inversible.

convieneContratar(UnArtista) :-
  escribio(UnArtista, UnaObra),
  esBestseller(UnaObra).

convieneContratar(UnArtista) :-
  esReincidente(UnArtista).

%Queremos saber si una obra es rioplatense:
% que es cuando la nacionalidad de su artista es
% platense (Uruguay o Argentina).
% ¡Ojo con repetir lógica!

nacionalidad(elsaBornemann, argentina).
nacionalidad(joseHernandez, argentina).
nacionalidad(julioCortazar, argentina).
nacionalidad(jorgeLuisBorges, argentina).
nacionalidad(horacioQuiroga, uruguaya).
nacionalidad(neilGaiman, britanica).
nacionalidad(alanMoore, britanica).
nacionalidad(warrenEllis, britanica).
nacionalidad(terryPratchett, britanica).
nacionalidad(brianAzarello, estadounidense).
nacionalidad(frankMiller, estadounidense).
nacionalidad(stephenKing, estadounidense).
nacionalidad(isaacAsimov, rusa).

% Opción 1

esObraRioplatense(Obra) :-
  escribio(Obra, Autor),
  nacionalidad(Autor, Nacionalidad),
  platense(Nacionalidad).

platense(argentina).
platense(uruguaya).

% Opción 2 - Repite lógica

esRioPlatense(Obra) :-
	escribio(Obra, AutorOAutora),
	esArtistaPlatense(AutorOAutora).

esArtistaPlatense(Artista) :-
	nacionalidad(Artista, argentina).

esArtistaPlatense(Artista) :-
	nacionalidad(Artista, uruguaya).

/*
Prohibidos:
  ==
  =
  ;
*/

%Queremos saber si una obra es rioplatense:
% que es cuando la nacionalidad de su artista es
% platense (Uruguay o Argentina).
% ¡Ojo con repetir lógica!

% Queremos saber si un artista escribió solo cómics

% escribioSoloComics(AutorOAutora) :-
%   escribio(AutorOAutora, _),
%   not(escribioLibro(AutorOAutora)).

% escribioLibro(AutorOAutora) :-
%   escribio(AutorOAutora, UnaObra),
%   esLibro(UnaObra).


escribioSoloComics(AutorOAutora) :-
  escribio(AutorOAutora, _),
  forall(escribio(AutorOAutora, UnaObra), esComic(UnaObra)).

soloSeEscribieronComics :-
  forall(escribio(_, UnaObra), esComic(UnaObra)).


% type Herramienta = (Nombre, Cantidad)
% herramienta(shuriken, 20)

% Functores
% novela(Genero, CantidadDeCapitulos)
% libroDeCuentos(CantidadDeCuentos)
% libroCientifico(Disciplina)
% bestSeller(Precio, CantidadDePaginas)

esDeGenero(it,                      novela(terror, 11)).
esDeGenero(cuentosDeLaSelva,        libroDeCuentos(10)).
esDeGenero(elUniversoEnUnaTabla,    libroCientifico(quimica)).
esDeGenero(elUltimoTeoremaDeFermat, libroCientifico(matematica)).
esDeGenero(yoRobot,                 bestSeller(700, 253)).
esDeGenero(oyasumiPunpun,           manga).

/*
estaBuena/1 nos dice cuando una obra está buena.
Esto sucede cuando:
- Es una novela policial y tiene menos de 12
  capítulos.
- Es una novela de terror.
- Los libros con más de 10 cuentos siempre son
  buenos.
- Es una obra científica de fisicaCuantica.
- Es un best seller y el precio por página es
  menor a $50.
*/

% Opción 1 
estaBuena(Obra, novela(policial,11)).
estaBuena(Obra, novela(terror,_)).
estaBuena(Obra, libroDeCuentos(11)).
estaBuena(Obra, libroCientifico(fisicaCuantica)).
estaBuena(Obra, bestSeller(15000,300)).
% no esta buena por que nos decia que tenia que tener aridad/1 

/*
% Opción 2
estaBuena(Obra):-
    esDeGenero(Obra,novela(policial, CantidadDeCapitulos)),
    CantidadDeCapitulos<12.

estaBuena(Obra):-
    esDeGenero(Obra,novela(terror,_)).

estaBuena(Obra):-
    esDeGenero(Obra,libroDeCuentos(CantCuentos)),
    10<=CantCuentos.

% Opción 3
estaBuena(Obra) :-
    esDeGenero(Obra, novela(policial, Capitulos)),
    Capitulos < 12.
estaBuena(Obra) :-
    esDeGenero(Obra, novela(terror,_)).
estaBuena(Obra) :-
    esDeGenero(Obra, libroDeCuentos(Cuentos)),
    Cuentos > 10.
estaBuena(Obra) :-
    esDeGenero(Obra, libroCientifico(fisicaCuantica)).
estaBuena(Obra) :-
    esDeGenero(Obra, bestSeller(PrecioPorPagina, _)),
    PrecioPorPagina < 50.

% Opción 4
estaBuena(Obra):-
    esDeGenero(Obra, novela(policial,Capitulos)),
    Capitulos < 12.

estaBuena(Obra):-
    esDeGenero(Obra, novela(terror,_)).

estaBuena(Obra):-
    esDeGenero(Obra, libroDeCuentos(CantidadDeCuentos)),
    CantidadDeCuentos > 10.

estaBuena(Obra):-
    esDeGenero(Obra, cientifico(fisicaCuantica)).

estaBuena(Obra):-
    esDeGenero(Obra, bestSeller(Precio,Paginas)),
    (Precio /Paginas) < 50.

% Opción 5
estaBuena(Obra):-

    esDeGenero(Obra,novela(policial,CantidadePag)),
    CantidadePag<12.
estaBuena(Obra):-
    esDeGenero(Obra,novela(terror,_)),
    esDeGenero(_,libroDeCuentos(Cantcuentos) ),
    CantCuentos>10,

estaBuena(Obra):-
    esDeGenero(Obra,libroCientifico(fisicacuantica))

estaBuena(Obra):-
BestSeller(Obra),

*/
% Opción 6
estaBuena(Obra) :-
  esDeGenero(Obra, Genero),
  esBuenGenero(Genero).       % Polimorfismo

esBuenGenero(libroCientifico(fisicaCuantica)).
esBuenGenero(novela(terror, _)).

esBuenGenero(novela(policial, CantidadDeCapitulos)) :-
  CantidadDeCapitulos < 12.

esBuenGenero(libroDeCuentos(CantidadDeCuentos)) :-
  CantidadDeCuentos > 10.

esBuenGenero(bestSeller(Precio, CantidadDePaginas)) :-
  (Precio / CantidadDePaginas) < 50.

esBuenGenero(manga).