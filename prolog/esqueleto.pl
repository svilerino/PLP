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

% asignar_var(+A, ?MI, ?MF)
%
% MI u MF debe estar instanciada (al menos una). En el caso contrario se entra
% en un ciclo infinito que no recorre todos los posibles valores. TODO: AMPLIAR
%
% Explicaciones de cada Instanciación:
% +A: A debe estar instanciada pues los meta-predicados nonvar(A) de todas las
% clausulas de este predicado asi lo fuerzan. Si no lo estuviera, y se tiene una
% combinacion de variables instanciadas que nos lleva a la primera clausula, esta
% devuelve error ya que el "pattern matching" de la clausula no permite unificar
% a la variable A, y luego el predicado nonvar(A) devuelve false.
%
% ?MI: 
%
% ?MF: 
% TODO: analisis de reversibilidad, comentario explicando porque anda
asignar_var(A, MI, MI):- nonvar(A), member((A, _), MI).
asignar_var(A, MI, [(A, _) | MI]):- nonvar(A), not(member((A, _), MI)).

% palabras_con_variables(+P,-V)
palabras_con_variables(P,V):- actualizar_aplicar_mapeo(P,[],V).

% actualizar_aplicar_mapeo(+P,+M,-V)
%
% Explicaciones de cada Instanciación:
% -V: Si V llegase a estar instanciada, las variables del mismo deberian coincidir
% exactamente con todas las que se obtienen a partir de asignar_var para obtener
% true, lo cual si bien no es imposible, tiene una probabilidad muy muy baja (y
% al problema no le interesan los numeros internos de variables, sino que asignado
% un numero de variable a un átomo, este se respete en el resto de las palabras).
%
% +P: Si P no llega a estar instanciada, se entra en un bucle infinito entre las
% dos primeras clausulas de este predicado (nunca se llega a entrar a la tercera
% clausula). Al no estar instanciadas P y V, para poder aplicar la primer clausula
% se unifican (P = V) y luego se unifican con la lista vacía. Luego se retrocede
% en el backtracking y se entra en la segunda clausula, donde se unifica tanto
% a P como a V con una lista con al menos una lista vacia como elemento, y se
% llama recursivamente al predicado, volviendo a pasar todo lo que se explica a
% aquí. Es decir, se entra en bucle infinito donde P y V terminan siendo unificadas
% entre si y con una lista de listas vacias, donde en cada paso de la recursión
% se agrega una nueva lista vacía.
%
% Si P estuviese semi-instanciada, además, al llegar a uno de sus elementos no
% instanciados se cae en la tercer clausula de este predicado, y se viola la
% especificación de asignar_var(A,MI,MF) al pasarle un A no instanciado.
%
% +M: El motivo principal por el cual se requiere que M esté instanciada es
% que al utilizarse la tercera clausula de este predicado, se utiliza el predicado
% asignar_var(A,MI,MF) con tanto MI y MF no instanciados, violando la especificación
% del predicado.
actualizar_aplicar_mapeo([],_,[]).
actualizar_aplicar_mapeo([ [] |ASS],M,[ [] |VSS]):-
    actualizar_aplicar_mapeo(ASS,M,VSS).
actualizar_aplicar_mapeo([ [A|AS] |ASS],MI,[ [V|VS] |VSS]):-
    asignar_var(A,MI,MF),
    aplicar_var(A,MF,V),
    actualizar_aplicar_mapeo([AS|ASS],MF,[VS|VSS]).

aplicar_var(A,[(A,V)|_],V).
aplicar_var(A,[(B,_)|M],V):- A \= B, aplicar_var(A,M,V).

% quitar(?E,L,R) - L puede ser una lista semi-instanciada
quitar(A,L,R):- exclude(iguales(A),L,R).

iguales(X,Y):- X==Y.

% cant_distintos(L, S)
cant_distintos([],0).
cant_distintos([X|XS],S):- quitar(X,XS,SinX), cant_distintos(SinX,Srec), S is 1+Srec.
%cant_distintos(L, S):- not(ground(L)), numbervars(L), cant_distintos(L,S).
%cant_distintos([A|AS],S):- ground([A|AS]), delete(AS,A,L), cant_distintos(L,Srec), S is 1+Srec.

% descifrar(S, M)
descifrar(S,M):-
    palabras(S,P), palabras_con_variables(P,Pvar),
    maplist(diccionario_lista,Pvar), %Pvar unifica con palabras del diccionario ascii
    juntar_con(Pvar,32,Mascii), %Mascii es Pvar, pero en una sola lista poniendo un espacio entre las palabras.
    string_codes(M,Mascii). %M es Mascii pero en chars

%
% descifrar_sin_espacios(S, M)

% mensajes_mas_parejos(S, M)
