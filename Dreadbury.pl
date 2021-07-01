viveEnDreadbury(agatha).
viveEnDreadbury(charles).
viveEnDreadbury(mayordomo).

agathaOdiaA(Persona) :-
    viveEnDreadbury(Persona),
    Persona \= agatha,
    Persona \= mayordomo.

mayordomoOdiaA(Persona) :-
    agathaOdiaA(Persona).

charlesOdiaA(Persona) :-
    viveEnDreadbury(Persona),
    not(agathaOdiaA(Persona)),
    Persona \= charles.

esMasRicoQueAgatha(Persona) :-
    not(mayordomoOdiaA(Persona)),
    viveEnDreadbury(Persona).

primeroOdiaAlSegundo(agatha,charles).
primeroOdiaAlSegundo(mayordomo,charles).
primeroOdiaAlSegundo(charles,mayordomo).
primeroOdiaAlSegundo(charles,agatha).

matoAAgatha(Persona) :-
    primeroOdiaAlSegundo(Persona,agatha),
    viveEnDreadbury(Persona),
    not(esMasRicoQueAgatha(Persona)).

% 1) Al hacer la consulta "matoAAgatha(Persona)." nos devuelve "Persona = charles.", por lo que el misterio esta resuelto; Charles mato a la Tia Agatha.

% 2)
%Existe alguien que odie a milhouse? Hacemos la consulta "primeroOdiaAlSegundo(_,milhouse).", lo que nos devuelve "false.". Por lo que en nuestros datos no existe nadie que odie a Milhouse.
%A quién odia Charles? Hacemos la consulta "charlesOdiaA(Persona).", y tendremos "Persona = agatha; Persona = mayordomo.".
%Quien odia a tía Ágatha? Hacemos la consulta "primeroOdiaAlSegundo(Persona,agatha)." y nos devuelve "Persona = charles.".
%Todos los odiadores y odiados se pueden obtener mediante las consultas "primeroOdiaAlSegundo(Persona,_)." y "primeroOdiaAlSegundo(_,Persona)." respectivamente.
%Es cierto que el mayordomo odia a alguien? Hacemos la consulta "mayordomoOdiaA(_)." y nos da "true .", lo que significa que efectivamente, el mayordomo odia por lo menos a una persona.

