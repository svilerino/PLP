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
% MI u MF debe estar instanciada (al menos una). En el caso contrario se entra
% en un ciclo infinito que no recorre todos los posibles valores. TODO: AMPLIAR
%
% +A: A debe estar instanciada pues los meta-predicados nonvar(A) de todas las
% clausulas de este predicado asi lo fuerzan. Si no lo estuviera, y se tiene una
% combinacion de variables instanciadas que nos lleva a la primera clausula, esta
% devuelve error ya que el "pattern matching" de la clausula no permite unificar
% a la variable A, y luego el predicado nonvar(A) devuelve false.
%
%
% ?MI: 
%
%
% ?MF: 
% TODO: analisis de reversibilidad, comentario explicando porque anda
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
% L puede ser una lista semi-instanciada
%
% ?E: El predicado funciona tanto este E instanciada como si no, ya que en
% las clausulas se utilizan operadores que comparan a E visto como término, con
% lo cual trabajar con una variable o algo instanciado será interpretado de la
% misma manera. WARNING!!! VERIFCAR ESTO
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
quitar(E,[X|XS],R):- E==X, quitar(E,XS,R).
quitar(E,[X|XS],[X|R]):- E\==X, quitar(E,XS,R).

%quitar(E,L,R):- exclude(iguales(E),L,R).
%iguales(X,Y) TODO: Enviar mail preguntando como es la reversibilidad de ==
%iguales(X,Y):- X==Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cant_distintos(L, S)
cant_distintos([],0).
cant_distintos([X|XS],S):- quitar(X,XS,SinX), cant_distintos(SinX,Srec), S is 1+Srec.
%cant_distintos(L, S):- not(ground(L)), numbervars(L), cant_distintos(L,S).
%cant_distintos([A|AS],S):- ground([A|AS]), delete(AS,A,L), cant_distintos(L,Srec), S is 1+Srec.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% descifrar(S, M)
descifrar(S,M):-
    palabras(S,P), palabras_con_variables(P,Pvar),
    maplist(diccionario_lista,Pvar), %Pvar unifica con palabras del diccionario ascii
    juntar_con(Pvar,32,Mascii), %Mascii es Pvar, pero en una sola lista poniendo un espacio entre las palabras.
    string_codes(M,Mascii). %M es Mascii pero en chars

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% descifrar_sin_espacios(S, M)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mensajes_mas_parejos(S, M)
