%Base de conocimientos 

viveEnLaMansion(tiaAgatha).
viveEnLaMansion(charles).
viveEnLaMansion(mayordomo).

mato(UnaPersona, Alguien) :-
    viveEnLaMansion(Unapersona),
    odia(UnaPersona,Alguien),
    not(masRicoQue(UnaPersona,Alguien)).


masRicoQueTiaAgatha(UnaPersona,Alguien):-
    viveEnLaMansion(unaPersona),
    not(odia(mayordomo,Alguien)).

    


odia(charles,Alguien):-
    viveEnLaMansion(Alguien),
    not(odia(agatha,Alguien)).

odia(agatha,Alguien):-
    Alguien /= mayordomo.

odia(mayordomo,Alguien):-
    odia(agatha,Alguien).
