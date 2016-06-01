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

% juntar_con(+L, ?J, ?R)
% L debe estar instanciado siempre, ya que sino se cuelga. Si J esta instanciado y R no,
% R se va a instanciar con la union de L intercalado con J. Si R esta instanciado y J no,
% J se instancia con el unico elemento posible (si existe) que haga que R sea el resultado,
% de intercalar este elemento entre los elementos de L. Si ni J ni R estan instanciados,
% J queda sin instanciar, y R se instancia con la lista intercalada de L, teniendo como
% elemento de intercalacion la variable que se haya elegido como J.
juntar_con([], _, []).
juntar_con([X], _, X).
juntar_con([X | Xs], J, R) :- length(Xs, Len), Len > 0, append(X, [J], Left), juntar_con(Xs, J, Right), append(Left, Right, R).

