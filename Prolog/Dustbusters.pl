% Base de Conocimientos

herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

tiene(egon,aspiradora(200)).
tiene(egon,trapeador).
tiene(peter,trapeador).
tiene(winston,varitaDeNeutrones).



% tareaPedida(Cliente,Tarea,Metros)
% precio(Tarea,Precio)

%tareaPedida(tarea, cliente, metrosCuadrados).
tareaPedida(ordenarCuarto, dana, 20).
tareaPedida(cortarPasto, walter, 50).
tareaPedida(limpiarTecho, walter, 70).
tareaPedida(limpiarBanio, louis, 15).

%precio(tarea, precioPorMetroCuadrado).
precio(ordenarCuarto, 13).
precio(limpiarTecho, 20).
precio(limpiarBanio, 55).
precio(cortarPasto, 10).
precio(encerarPisos, 7).





satisfaceNecesidad(Persona,Herramienta):-
    tiene(Persona,Herramienta).
satisfaceNecesidad(Persona,aspiradora(Potencia)):- 
    tiene(Persona,aspiradora(UnaPotencia)),
    %UnaPotencia >= Potencia .  ( al no estar ligado no es inversible)
    between(0,UnaPotencia,Potencia).

puedeRealizarTarea(Persona,Tarea):-
    herramientasRequeridas(Tarea,_),
    tiene(Persona,varitaDeNeutrones).

puedeRealizarTarea(Persona,Tarea):-
    tiene(Persona,_),
    requiereHerramienta(Tarea,Herramienta),
    forall(requiereHerramienta(Tarea,Herramienta),puedeRealizarTarea(Persona,Herramienta)).

requiereHerramienta(Tarea,Herramienta):-
    herramientasRequeridas(Tarea,Herramientas),
    member(Herramienta,Herramientas).

aCobrar(Cliente,PrecioACobrar):-
    findall(Precio,precioDeTareaPedida(Cliente,Precio),Precios),
    sumlist(Precios,PrecioACobrar).

precioDeTareaPedida(Ciente,Precio):-
    tareaPedida(Tarea,Cliente,Metros),
    precio(Tarea,Costo),
    Precio is Costo*Metros.

% 5 - Quienes Aceptan el pedidio de un cliente

aceptaraPedido(Persona,Cliente):-
    puedeHacerPedido(Persona,Cliente).
    dispuestoARealizarlo(Persona,Cliente).

puedeHacerPedido(Persona,Cliente):-
    tareaPedida(_,Cliente,_),
    tiene(Persona,_),
    forall(tareaPedida(Tarea,Cliente,_),puedeRealizarTarea(Persona,Tarea)).

dispuestoARealizarlo(ray,Cliente):-
    not(tareaPedida(limpiarTecho,Cliente,_)).

dispuestoARealizarlo(winston,CLiente):-
    aCobrar(Cliente,PrecioACobrar),
    PrecioACobrar > 500 .


dispuestoARealizarlo(egon,Cliente):-
    tareaPedida(Tarea,Cliente,_),
    not(tareaCompleja(Tarea)).

dispuestoARealizarlo(peter,_).

tareaCompleja(Tarea):-
    herramientasRequeridas(Tarea,Herramientas),
    member(limpiarTecho,Herramientas).

%tareaCompleja(limpiarTecho)

tareaCompleja(Tarea):-
    herramientasRequeridas(Tarea,Herramientas),
    length(Herramientas,Cantidad),
    Cantidad >= 2.

% 6- 

