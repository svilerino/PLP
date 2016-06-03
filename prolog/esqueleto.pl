:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.



% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

% diccionario_lista(?Y)
% Si Y no esta instanciado, Y se va a instanciar en las listas de codigos ASCII
% correspondiente a cada palabra presente en el diccionario. Si esta instanciado,
% el resultado del predicado es true si el string que representa pertenece al 
% diccionario actual.
diccionario_lista(Y) :- diccionario(X), string_codes(X, Y).

% juntar_con(?L, ?J, ?R)
% Cuando L esta instanciada y R no, se instancia R con la lista que contiene a cada
% elemento de cada lista de L, intercalandolas con J. En este caso, si J esta instanciada,
% el valor de la misma es el que sera intercalado, y si no lo esta, se intercalara con la 
% variable.
% Cuando R esta instanciada y L no, se instancia L con cada posible lista que haga que L
% intercalado con J sea igual a R. Igual que en el caso anterior, si J esta instanciada se
% tomara su valor, y si no, cada posible valor perteneciente a R.
% Si todo esta instanciado, el predicado verifica que intercalar L con J sea igual a R.
juntar_con([], _, []).
juntar_con([X], _, X).
juntar_con([X | Xs], J, R) :- append(X, [J | Rec], R), juntar_con(Xs, J, Rec), length(Rec, LRec), LRec > 0.

% palabras(?S, ?P)
% Ya sea S o P deben estar instanciados, porque sino se cuelga.
% Cuando S esta instanciado y P no, se instancia P con el resultado de separar S por el átomo espacio.
% Cuando P esta instanciado y S no, se instancia S con el resultado de juntar P (en el sentido de
% la funcion anterior) con el átomo espacio.
palabras([], []).
palabras(S, P) :- juntar_con(P, espacio, S), not((member(Palabra, P), member(espacio, Palabra))).

% asignar_var(+A, MI, MF)
% TODO: analisis de reversibilidad, comentario explicando porque anda
asignar_var(A, MI, MI):- nonvar(A), member((A, _), MI).
asignar_var(A, MI, [(A, _) | MI]):- nonvar(A), not(member((A, _), MI)).

% palabras_con_variables(P, V)
palabras_con_variables(P,V):- mapear(P,MF), aplicar_mapeo_palabras(P,MF,V).

mapear(P,MF):- unir_palabras(P,AS), mapear_atomos(AS,[],MF).

unir_palabras([],[]).
unir_palabras([AS|ASS],R):- unir_palabras(ASS,Rec), append(AS,Rec,R).

mapear_atomos([],M,M).
mapear_atomos([A|AS],MI,MF):- asignar_var(A,MI,M), mapear_atomos(AS,M,MF).

aplicar_mapeo_palabras([],_,[]).
aplicar_mapeo_palabras([AS|ASS],MF,[VS|VSS]):- aplicar_mapeo_atomos(AS,MF,VS), aplicar_mapeo_palabras(ASS,MF,VSS).

aplicar_mapeo_atomos([],_,[]).
aplicar_mapeo_atomos([A|AS],M,[V|VS]):- aplicar_var(A,M,V), aplicar_mapeo_atomos(AS,M,VS).

aplicar_var(A,[(A,V)|_],V).
aplicar_var(A,[(B,_)|M],V):- A \= B, aplicar_var(A,M,V).

% quitar(E, L, R)

% cant_distintos(L, S)

% descifrar(S, M)

% descifrar_sin_espacios(S, M)

% mensajes_mas_parejos(S, M)
