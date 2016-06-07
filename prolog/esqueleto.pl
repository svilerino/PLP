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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% diccionario_lista(?Y)
% Si Y no esta instanciado, Y se va a instanciar en las listas de codigos ASCII
% correspondiente a cada palabra presente en el diccionario. Si esta instanciado,
% el resultado del predicado es true si el string que representa pertenece al 
% diccionario actual.
diccionario_lista(Y) :- diccionario(X), string_codes(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% palabras(?S, ?P)
% Ya sea S o P deben estar instanciados, porque sino se cuelga.
% Cuando S esta instanciado y P no, se instancia P con el resultado de separar S por el átomo espacio.
% Cuando P esta instanciado y S no, se instancia S con el resultado de juntar P (en el sentido de
% la funcion anterior) con el átomo espacio.
palabras([], []).
palabras(S, P) :- juntar_con(P, espacio, S), not((member(Palabra, P), member(espacio, Palabra))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% asignar_var(+A, ?MI, ?MF)
%
% +A: A debe estar instanciada pues los meta-predicados nonvar(A) de todas las
% clausulas de este predicado asi lo fuerzan, y si A no estuviese instanciada,
% ninguno de los dos predicados seria satisfacible.
%
% MI u MF debe estar instanciada (al menos una). En el caso contrario se 
% generan tanto en MI como en MF listas infinitas, que terminan con (A, _).
%
% El primer predicado sirve para el caso en que se intenta definir una variable
% A que ya existe en la lista MF. En este caso, MI tiene que unificar con MF.
% El segundo predicado unifica MF con una lista que tiene en la cabeza a la nueva 
% variable A, acompañada en la tupla de una variable fresca, y la cola unificada
% con MI.
% 
% Este punto funciona porque abusa del hecho que prolog reemplaza por variables 
% frescas cuando se escribe un guion bajo. Utilizar member((A,_), MI) para ver
% si la variable A (junto con su variable libre) ya esta definida en la lista 
% funciona porque al ser libre puede unificar con cualquier cosa, incluyendo otra 
% variable libre.
asignar_var(A, MI, MI):- nonvar(A), member((A, _), MI).
asignar_var(A, MI, [(A, _) | MI]):- nonvar(A), not(member((A, _), MI)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% palabras_con_variables(+P,-V)
palabras_con_variables(P,V):- actualizar_aplicar_mapeo(P,[],V).

% actualizar_aplicar_mapeo(+P,+M,-V)
%
% -V: Si V llegase a estar instanciada, las variables del mismo deberian
% coincidir exactamente con todas las que se obtienen a partir de asignar_var
% para obtener true, lo cual si bien no es imposible, tiene una probabilidad
% muy muy baja (y al problema no le interesan los numeros internos de
% variables, sino que asignado un numero de variable a un átomo, este se
% respete en el resto de las palabras).
%
%
% +P: Si P no llega a estar instanciada, se entra en un bucle infinito entre
% las dos primeras clausulas de este predicado (nunca se llega a entrar a la
% tercera clausula). Al no estar instanciadas P y V, para poder aplicar la
% primer clausula se unifican (P = V) y luego se unifican con la lista vacía.
% Luego se retrocede en el backtracking y se entra en la segunda clausula,
% donde se unifica tanto a P como a V con una lista con al menos una lista
% vacia como elemento, y se llama recursivamente al predicado, volviendo a
% pasar todo lo que se explica a aquí. Es decir, se entra en bucle infinito
% donde P y V terminan siendo unificadas entre si y con una lista de listas
% vacias, donde en cada paso de la recursión se agrega una nueva lista vacía.
%
% Si P estuviese semi-instanciada, además, al llegar a uno de sus elementos no
% instanciados se cae en la tercer clausula de este predicado, y se viola la
% especificación de asignar_var(A,MI,MF) al pasarle un A no instanciado.
%
%
% +M: El motivo principal por el cual se requiere que M esté instanciada es que
% al utilizarse la tercera clausula de este predicado, se utiliza el predicado
% asignar_var(A,MI,MF) con tanto MI y MF no instanciados, violando la
% especificación del predicado.
%
actualizar_aplicar_mapeo([],_,[]).
actualizar_aplicar_mapeo([ [] |ASS],M,[ [] |VSS]):-
    actualizar_aplicar_mapeo(ASS,M,VSS).
actualizar_aplicar_mapeo([ [A|AS] |ASS],MI,[ [V|VS] |VSS]):-
    asignar_var(A,MI,MF),
    aplicar_var(A,MF,V),
    actualizar_aplicar_mapeo([AS|ASS],MF,[VS|VSS]).


% aplicar_var(+A,+M,-V)
%
% +A: Si A no llegase a estar instanciada, los resultados obtenidos no son
% correctos y/o completos dependiendo de las instanciaciones de M y V. En
% particular supongamos que M ha de estar instanciada (ver el análisis de dicha
% variable), asi pues las explicaciones a continuación corresponden a los casos
% +V y -V (ya que V es un parámetro reversible del predicado).
%
% Al tratar de utilizar la primer clausula, se unificará la variable A con el
% primer componente del primer elemento/mapeo/tupla de M (que está
% instanciada).  Luego, ocurrirá lo mismo con V y el segundo elemento de esta
% tupla (este o no V instanciada, ya que en ambos casos se está unificando una
% variable con otra).
%
% Si V no estaba instanciada en un número explícito de variable, este resultado
% es un resultado válido, pero existen más resultados válidos si M tiene más
% elementos/tuplas a continuación (A y V unificados con la primera y segunda
% componente -respectivamente- de estas otras tuplas), los cuales no son
% devueltos pues no se puede aplicar la segúnda clausula con estas
% instanciaciones.  Esto ocurre pues la segunda clausula utiliza el predicado
% "\=" con A y B, y si bien B es unificado con el término correspondiente al
% primer elemento de la primera tupla de M, A sigue siendo una variable no
% unificada; y el predicado "\=" requiere que sus dos parámetros sean términos.
% Por lo tanto siempre devuelve "false" y no se consideran los demás
% mapeos/tuplas de M.
%
% En caso de que V estuviera instanciada en un número de variable, ocurrirá
% lo mismo descripto para el caso en el que no lo está, con la salvedad de que
% el resultado obtenido podría incluso ser inválido, ya que se mapea un número
% de variable con otro número de variable, y el número instanciado en V podría
% llegar a ser utilizado en otra tupla de M, con lo cual al unificar V con
% otro número se estarían unificando dos variables de M, y esto viola la
% condición de M de asignar una variable distinta a cada átomo.
%
%
% +M: Si M no llegase a estar instanciada, ocurre (en escencia) lo mismo que
% con el análisis de reversibilidad de A. Se logran unificar las cosas para
% poder coincidir con las condiciones de la primera clausula (y en este caso
% se devuelve un resultado inválido), y luego no se analizan más casos ya que
% ahora no se logra unificar de manera tal de llegar al llamado recursivo de la
% segunda clausula. A continuación se explica porque.
%
% En el caso de la primera clausula, al unificar los parámentros A y V con M
% según la estructura descripta en la clausula, se termina unificando a M con
% una lista de la forma [(A,V) | _Gxxx] (con el valor instanciado de A y V si
% estuvieran instanciadas, o las variables mismas en el caso contrario). Y este
% resultado de M no es necesariamente correcto, ya que depende de en que se
% instancie la cola de la misma (_Gxxx). Es decir, esta instanciación sería
% solo válida si se tiene un predicado que nos asegura que no existe tupla en
% _Gxxx que utilice a A y/o V.
%
% Si se admitiese que el valor de M obtenido por la unificación de la primera
% clausula es correcto, el siguiente inconveniente es que existen más mapeos
% (infinitos, de hecho) los cuales nunca son recorridos pues nunca se llega
% a considerar el llamado recursivo de la segunda clausula. Esto ocurre pues,
% nuevamente, la unificación no logra otorgar un valor a uno de los parámetros
% del predicado "\=", en este caso dicho parámetro es B. B no unifica con
% ningún valor pues M no está instanciada, con lo cual sigue siendo una variable
% (no unificada) cuando se evalúa el predicado "A\=B", el cual da siempre false
% ya que requiere que ambos parámetros sean términos.
%
%
% -V: Teniendo ya a A y M como parámetros instanciados, si V no está instanciada
% su valor siempre se unificará con la segunda componente de la tupla de M que
% tenga como primera componente a A (si existe). Caso contrario, el predicado
% devolvera false.
%
% Si V llegase a estar instanciada, podría dar un resultado válido si justo se
% instancia en el número de variable que corresponde al mapeo de A en M. Pero
% si este no es el caso, se termina unificando V con el mapeo de A en M (es
% decir, se unifican dos números de variable distintos) y se devuelve true. Aquí
% ocurre que la instanciación de V podría ser el número de variable del mapeo
% de otro átomo B en M, lo cual terminaría diciendo que existe un mísmo número
% de variable para los átomos A y B en M, lo cual es incorrecto ya que M es un
% mapeo válido).
%
aplicar_var(A,[(A,V)|_],V).
aplicar_var(A,[(B,_)|M],V):- A \= B, aplicar_var(A,M,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quitar(?E,+L,?R)
% L puede ser una lista semi-instanciada. En caso de que lo sea, R ha de estar
% completamente instanciada.
%
% ?E: El predicado funciona tanto este E instanciada como si no, ya que en
% las clausulas se utilizan operadores que comparan a E visto como término, con
% lo cual trabajar con una variable o algo instanciado será interpretado de la
% misma manera. 
%
%
% +L: Debemos analizar caso por caso para explicar bien por qué L ha de estar al
% menos semi-instanciada:
%
% Si L no está instanciada, R y E lo están: Se entrará en la primera clausula
% sólo si R está instanciada en la lista vacía, en cuyo caso el resultado de L
% será una lista vacía, lo cual es correcto. El inconveniente ocurre luego, pues
% existen otras posibles instanciaciones de L (ej: [E]) que debería devolver
% el predicado pero no lo hace.
%
% Tanto en la segunda como tercera clausula se unifica a L con una no vacía. En
% el caso de la segunda clausula, el predicado "E==X" falla ya que X es una
% variable fresca que se compara contra E, que es una variable instanciada (y
% la comparación de términas fallará). Justamente aquí se dejan de considerar
% las demás posibles instanciaciones válidas de L, pues es la clausula donde
% se considera que L originalmente tenía elementos iguales a E y que fueron
% quitados de R.
%
% En el caso de la clausla restante, se unificara a X con el primer elemento de
% R (es decir, X no es una variable fresca, sino que está instanciada). Si
% "E\==X" evalúa en false, no se devuelve ningún valor más (correctamnete, pues
% R tiene el elemento que se quitó de L), y en el caso contrario se hace el
% llamado recursivo. Si esto último ocurre, se repite todo el proceso sobre la
% cola de R. Si E no se encuentra en el R inicial, entonces el resultado
% terminará diciendo L = R (se recorre todo R y se van unificando los elementos
% de este a los de L siempre que no sean iguales a E).
%
% Si L y E no están instanciadas y R lo está: Ocurre lo mismo que en la
% combinación de instanciaciones previa, ya que los predicados "==" y "\="
% comparan términos, y sea E una variable o una instancia, estos predicados
% darán los mismos resultados.
%
% Si L y R no están instanciadas y E lo está: La primera clausula siempre
% unificará L con R y con []. En el caso de la segunda clausula, ocurrirá lo
% mismo que en los casos analizados previamente (es decir, nunca se evaluará
% en true), y por último se llega a la tercera clausula donde se unifica
% a L y R como dos listas no vacías que tienen el mismo primer elemento. Dicha
% clausula se evaluará en true pues X se unifica con una variable fresca que
% jamás coincidirá con la instanciación de E y además el llamado recursivo
% repetirá los pasos previos con la cola de las listas L y R (que eventualmente
% serán unificadas al reevaluarse la primera clausula). Es decir, L y R siempre
% terminarán unificadas para todos los resultados devueltos, y en cada paso
% se les agregara una variable fresca como elemento, pero nunca se asegura que
% estas variables frescas serán distintas de E en R ni se toman en cuenta
% listas L con elementos E.
%
% Si L, E y R no están instancadas: Dado que los predicados "==" comparan
% términos, tener a E no instanciada tendrá el mismo comportamiento que tenerla
% instanciada, con lo cual se obtienen los mismos resultados que en el caso
% analizado previamente.
%
%
% ?R: Si R no está instanciada, se irán instanciando sus elementos con aquellos
% de L distintos del término E. En caso de estar instanciada, se tratará de
% unificar con los elementos de R con los de L distintos del término E 
%
quitar(_,[],[]).

quitar(E,[X|XS],R):- var(R), E==X, quitar(E,XS,R).
quitar(E,[X|XS],[X|R]):- var(R), E\==X, quitar(E,XS,R).

quitar(E,[X|XS],R):- ground(R), nonvar(E), not(member(E,R)), E=X, quitar(E,XS,R).
quitar(E,[X|XS],[Y|R]):- ground([Y|R]), nonvar(E), E\=Y, Y=X, not(member(E,R)), quitar(E,XS,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cant_distintos(+L, ?S)
%
% Si L no esta instanciada, se llama a quitar(X,XS,SinX) rompiendo la
% precondicion de que L debe estar al menos semi-instanciada con los problemas
% que eso trae.
%
% Particularmente, cant_distintos se cuelga si se pasa una lista no instanciada.
% Dada una lista L instanciada, 
%   Si S esta instanciado, se devuelve true o false dependiendo de la validez de el predicado.
%   Si S no esta instanciado, se devuelve en S la cantidad de elementos distintos de L.
%
cant_distintos([],0).
cant_distintos([X|XS],S):- quitar(X,XS,SinX), cant_distintos(SinX,Srec), S is 1+Srec.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% descifrar(S, M)
%
descifrar(S,M):-
    palabras(S,P), palabras_con_variables(P,Pvar),

    % Unificamos cada Palabra de variables con palabras del diccionario ascii.
    % Podrían haber errores con palabras de igual longitud y letras iguales en
    % la misma posición, filtramos esos casos luego
    maplist(diccionario_lista,Pvar),

    %Mascii es Pvar, pero en una sola lista poniendo un espacio ascii entre las
    %palabras.
    juntar_con(Pvar,32,Mascii),

    %Nos aseguramos que el mapeo sea valido y no haya asignado una misma letra a
    %dos atomos distintos (a sus variables asignadas en palabras_con_variables
    %en realidad)
    quitar(espacio,S,Atomos),cant_distintos(Atomos,Cant_caracteres_distintos),
    quitar(32,Mascii,Letras_ascii), cant_distintos(Letras_ascii,Cant_caracteres_distintos),

    %M es un string que se corresponde con Mascii
    string_codes(M,Mascii).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% descifrar_sin_espacios(S, M)

descifrar_sin_espacios(S,M):-
    not(member(espacio,S)), %verificacion del input
    length(S,Letras),

    %Con_max_espacios es la longitud si se intercala un espacio entre letra y
    %letra en S.
    Con_max_espacios is Letras*2-1,

    between(Letras,Con_max_espacios,Long_S_con_espacios),
    length(S_con_espacios,Long_S_con_espacios),

    %Instanciamos los posibles mensajes de longitud Long_S_con_espacios que al
    %quitarle los espacios sea S
    quitar(espacio,S_con_espacios,S),

    %Optimizacion para no considerar casos que empiezan con un espacio
    not(nth1(1,S_con_espacios,espacio)),

    %Optimizacion para no considerar casos que terminan con un espacio
    not(last(S_con_espacios,espacio)),

    %Optimizacion para no considerar casos que tienen espacios consecutivos
    not(nextto(espacio,espacio,S_con_espacios)),

    descifrar(S_con_espacios,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mensajes_mas_parejos(S, M)
mensajes_mas_parejos(S, M) :-
    setof((Std,Msg),(descifrar_sin_espacios(S,Msg),std_mensaje(Msg,Std)),Msgs_con_std),
    %El resultado esta ordenado y sin repetidos, gracias a setof y que pusimos
    %el STD como primera componente de las tuplas
    Msgs_con_std = [(Std_min,_)|_],
    member((Std,M),Msgs_con_std),
    Std =< Std_min.


% std_mensaje(String,STD)
std_mensaje(S,STD):-
    aggregate_all(bag(L),(split_string(S," "," ",Palabras),member(P,Palabras),string_length(P,L)),Longs),
    desviacion_estandar(Longs,STD).


%desviacion_estandar(List:list(number),STD)
desviacion_estandar(L, STD):-
    length(L,Cant),
    media(L,Media),
    aggregate_all(sum(Sumando),(member(X,L),Sumando is (X-Media)^2),Sum),
    STD is sqrt(Sum / Cant).

%media(List:list(number),Mean)
media(L,M):- length(L,Cant), sum_list(L,Sum), M is Sum / Cant.


%------------- Tests -----------------
%test(#ej, #test)
% Ej1
% leer diccionario como lista de ascii
test(1, 1) :- aggregate_all(bag(X), (cargar("dicc0.txt"), diccionario_lista(X)), 
  [[101, 108], [108, 97], [99, 97, 115, 97], [99, 111, 115, 97]]).  % Notar que esta es la lista de las palabras del dicc pasadas a ascii
% verificar si una palabra como lista de ascii pertenece al dicc.
test(1, 2) :- cargar("dicc0.txt"), diccionario_lista([101, 108]).%Si esta en el dicc
test(1, 3) :- cargar("dicc0.txt"), diccionario_lista([101, 18]).%No esta en el dicc(no unifica, da false)

%Ej2
test(2, 1) :- juntar_con([[x],[x,y],[z]],a, [x, a, x, y, a, z]).

%Ej3
test(3, 1) :- ej(1, S), palabras(S, [[rombo, cuadrado], [perro, cuadrado, sol, cuadrado]]).

%Ej4
%% test(4, 1, LaBolsa) :- asignar_var(rombo, [], M), bagof(X, member((X, _), M), LaBolsa).

%% M = [(rombo, _G4012)],
%% false.
%% ?- asignar_var(cuadrado, [(rombo, _G4012)], M).
%% M = [(cuadrado, _G4013),(rombo, _G4012)],
%% false.
%% ?- asignar_var(rombo, [(cuadrado, _G4013),(rombo, _G4012)], M).
%% M = [(cuadrado, _G4013),(rombo, _G4012)],
%% false.

%Ej 5

%Ej 6
test(6, 1) :-  quitar(z,[A,B,A,z], L), L == [A, B, A].
test(6, 2) :-  quitar(A,[A,B,A,z], L), L == [B,z].

%Ej 7
test(7, 1) :- cant_distintos([0, 3, 0, 3, 4, 5, 6], 5).
test(7, 2) :- cant_distintos([0, 1, 2, 3, 4, 5, 6], 7).

%Ej 8

test(8, 1) :- cargar("dicc0.txt"), ej(1, S), descifrar(S, "la casa").
test(8, 2) :- cargar("dicc0.txt"), ej(2, S), descifrar(S, "la cosa").
test(8, 3) :- cargar("5000_formas.txt"), ej(3, S), descifrar(S, "federación").

% Ej 9
test(9, 1) :- cargar("dicc1.txt"), ej(3, S), bagof(M, descifrar_sin_espacios(S, M), Sal), Sal = ["casa miento", "casa de flor", "casa flor de"].

%Ej 10
test(10, 1) :- cargar("dicc1.txt"), ej(3, S), bagof(M, mensajes_mas_parejos(S, M), MyBag), MyBag = ["casa de flor", "casa flor de"].