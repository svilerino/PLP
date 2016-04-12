module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let 
    xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x 
    in nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

--------------------------------------------------------------------------------
{- split: Consultamos caracter a caracter (o elemento a elemento de la lista, en realidad)
si el mismo es el elemento separador. Si no lo es lo agregamos a la primera cadena/lista
del resultado (que es una lista de listas), caso contrario insertamos una nueva cadena/lista
vacia al comienzo del resultado.

Por ultimo, filtramos las posibles cadenas/listas vacias que tenga el resultado (esto sería
el paso 'trim' del que se habló en clase.
-}
split :: Eq a => a -> [a] -> [[a]]
split x xs = filter (not . null) (foldr (\y rec -> if y == x then []:rec else (y:head rec):(tail rec)) [[]] xs)


--------------------------------------------------------------------------------
{- longitudPromedioPalabras: Convierte el parametro de entrada (de tipo Texto) en una lista
de palabras, y luego convierte cada palabra a un número representando la longitud de la misma.
Por último, calcula el promedio de todas estas longitudes.

Ej: "Hola, ¿Cómo te va? -> ["Hola,","¿Cómo","te","va?"] -> [5,5,2,3] -> 3.75
-}
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras texto = if null texto then 0.0 else mean (map genericLength (split ' ' texto))


--------------------------------------------------------------------------------
{- cuentas: Se toma cada elemento de la lista original (sin tomar en cuenta los repetidos,
y se cuenta la cantidad de apariciones que tiene en la lista original. Luego "agrega" la
tupla <#aparciones(elemento),elemento> al resultado final) 
-}
apariciones :: Eq a => a -> [a] -> Int
apariciones x xs = length $ filter (x==) xs

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = [(apariciones x xs, x) | x <- nub xs]


--------------------------------------------------------------------------------
{- repeticionesPromedio: Convertimos el texto de entrada a una lista de palabras (con
split), luego convertimos esta en una lista de tuplas <#aparciones,elemento> para por
ultimo calcular el promedio de las componentes izquierdas (convertidas a Float) de
todas estas tuplas.

Ej: "Y dale, dale, dale dale, dale tense" 
	-> [[Y],[dale,],[dale,],[dale],[dale,],[dale],[tense]]
	-> [(1,"Y"),(3,"dale,"),(2,"dale"),(1,"tense")]
	-> [1.0,3.0,2.0,1.0]
	-> 1.75
-}
repeticionesPromedio :: Extractor
repeticionesPromedio texto = if null texto then 0.0 else mean (map (fromIntegral . fst) (cuentas (split ' ' texto)))


--------------------------------------------------------------------------------
{- frecuenciasTokens: Crea una lista por comprensión de funciones (expresadas como
funciones lambda). Cada una de estas funciones tienen un token fijo, y al pasarles
un texto calculan la frecuencia relativa de dicho token respecto de la longitud
del texto.
-}
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ \texto -> if null texto
    then 0.0
    else (fromIntegral (apariciones token texto)) / (fromIntegral (length texto)) | token <- tokens]


--------------------------------------------------------------------------------
{- normalizarExtractor: Básicamente, tomamos el resultado del extractor para cada
texto (feature) y normalizamos a cada uno de ellos dividiendolo por el de mayor
valor absoluto (el màximo feature en módulo). Es decir, normalizamos los features
de un extractor para una lista de textos relativo al feature de mayor valor
absoluto.
-}
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = let
    features = map extractor textos  -- Renombre para el feature obtenido con el extractor para cada texto
    escala = foldl (\rec x -> max rec (abs x)) 0 features --Calculamos el maximo de los features
    in (\texto -> (extractor texto) / (if escala > 0 then escala else 1)) --Funcion devuelta/definida


--------------------------------------------------------------------------------
{- extraerFeatures: Primero "transformamos" (abuso de léxico, nada se "transforma"
en funcional) los extractores en extractores normalizados relativos a los textos.
Luego aplicamos estos extractores normalizados a cada uno de los textos.
-}
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos = let
    extractoresNorm = map (\extractor -> normalizarExtractor textos extractor) extractores
    in  map (\texto -> map (\extractor -> extractor texto) extractoresNorm) textos


--------------------------------------------------------------------------------
--distEuclideana: Trivial (muy). No creemos que requiera aclaraciones.
distEuclideana :: Medida
distEuclideana p q = sqrt (sum (map (**2) (zipWith (-) p q)))


--------------------------------------------------------------------------------
--distCoseno: Idem distEuclideana
prodVectorial :: [Float] -> [Float] -> Float
prodVectorial p q = sum (zipWith (*) p q)

normaVectorial :: [Float] -> Float
normaVectorial p = sqrt (prodVectorial p p)

distCoseno :: Medida
distCoseno p q = (prodVectorial p q) / (normaVectorial p * normaVectorial q)


--------------------------------------------------------------------------------
{- knn: La función knn se apocha en dos funciones auxiliares, distanciasAInstancia
y modaEstadística. Primero comentaremos como funcionan estas funciones auxiliares
y luego pasaremos a explicar knn en si mismo.

La primera, distanciasAInstancia, simplemente calcula la distancia de la
instancia de entrada a todas las instancias de los Datos (es decir, a cada
elemento de Datos). Esta distancia está definida por el parametro medida (que
es una funcion de 2 instancias en un float). Básicamente tomtamos la función
medida y se aplica a todos los elementos de Datos contra la instancia de
entrada.

La segunda, modaEstadística, simplemente toma una lista de elementos y nos
devuelve el elemento de la misma que más apariciones tiene. Para ello, primero
calcula la cantidad de repeticiones de cada elemento (generando una lista de
tuplas <#aparciones,elemento> mediante la función cuentas) para luego utilizar
la extructura foldr para buscar la tupla cuya primer componente (la cantidad
de repeticiones) es la más grande. Por último, nos quedamos sólo con el
elemento (es decir, con la segunda componente de esta tupla).

Ahora sí, knn lo que hace es obtener una lista de distancias de las instancias
de entrenamientos a la instancia de entrada (utilizando la Medida pasada por
parámetro) y luego asociar cada elemento de esta lista (que representa una
distancia) con la etiqueta de la instancia de entrenamiento correspondiente
(esta asociación se realiza mediante zip, conviertiendo los resultados en
una lista de tuplas <distancia,etiqueta>). Luego, esta lista se ordena de
menor a mayor mediante la primer componente de las tuplas y se toman los primeros
k (primer parámentro) elementos (es decir, los k vecinos a menor distancia).
Luego se convierte esta lista de tuplas en una lista de únicamente las segundas
componentes de las mismas (es decir, con las etiquetas) para luego quedarnos con
aquella que se repita más (mediante modaEstadistica).

Ej: (knn 2 [[1,10],[1,5],[5,1]] [f,i,i] distEuclideana) [1,1]
    -> [9.0,4.0,4.0]
    -> [(9.0,f),(4.0,i),(4.0,i)]
    -> [(4.0,i),(4.0,i),(9.0,f)]
    -> [(4.0,i),(4.0,i)]
    -> [i,i]
    -> i
-}
distanciasAInstancia :: Datos -> Medida -> Instancia -> [Float]
distanciasAInstancia datos medida instancia = map (medida instancia) datos

modaEstadistica :: Eq a => [a] -> a
modaEstadistica xs = snd (foldr (\x rec -> if fst x > fst rec then x else rec) (0, head xs) (cuentas xs))

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medida = \instancia -> modaEstadistica (map snd (take k (sort (zip (distanciasAInstancia datos medida instancia) etiquetas))))


--------------------------------------------------------------------------------
{- separarDatos: Esta función es simplemente la aplicación para distintas listas
(la de datos y la de etiquetas) de dos funciones: obtenerSalvoParticion y
obtenerParticion.

Básicamente lo que hacen es particionar una lista de entrada en  n particiones
y devolver la partición p (parámentro), en el caso de obtenerParticion, o 
devolver toda la lista original excepto la partición p (obtenerSalvoParticion).

obtenerPartición lo que hace es calcular el tamaño de cada partición dada la
lista original (y su cantidad de elementos) y el tamaño de la partición pasado
por parámetro, para luego eliminar los elementos de las primeras p-1 particiones
y luego tomar de las elementos restantes los primeros longParticion elementos 
(para p la partición a obtener y longParticion la longitud de una particion).

obtenerSalvoPartición es muy similar a obtenerParticion. Calcula el tamaño de la
partición en base al tamaño de la lista y la cantidad de particiones, y luego
calcula la cantidad de elementos antes y después de la particón a ser "filtrada".
Por último, simplemente tomamos los elementos de todas las particiones previas
a la p (la que va a ser filtrada) y se concatena con todos los elementos post
partición p.
-}
obtenerParticion :: Int -> Int -> [a] -> [a]
obtenerParticion n p xs = let
    longParticion = (length xs) `div` n
    longPrimerParte = (p - 1) * longParticion
    in take longParticion (drop longPrimerParte xs)

obtenerSalvoParticion :: Int -> Int -> [a] -> [a]
obtenerSalvoParticion n p xs = let
    longParticion = (length xs) `div` n
    longPrimerParte = (p - 1) * longParticion
    longUltimaParte = (n - p) * longParticion
    longHastaUltimaParte = longPrimerParte + longParticion
    in take longPrimerParte xs ++ take longUltimaParte (drop longHastaUltimaParte xs) --se podría hacer con reverse reverse y evitar un calculo, aunque reverse no es muy performante... también se podría con un dropWhile

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = (obtenerSalvoParticion n p datos, obtenerParticion n p datos, obtenerSalvoParticion n p etiquetas, obtenerParticion n p etiquetas)


--------------------------------------------------------------------------------
{- accuracy: Simplemente tomamos ambas listas, y generamos una lista de boolean
resultado de comparar elemento a elemento (en la misma posicón) las 2 listas
de entrada. Luego convertimos todos los valores de esta lista True a 1 y los
False a 0. Sumamos y dividimos por la longitud de las listas (que debería ser
la misma).
-}
boolAFloat :: Bool -> Float
boolAFloat b = case b of
    True -> 1.0
    False -> 0.0

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy predicciones etiquetas = sum (map boolAFloat (zipWith (==)  predicciones etiquetas)) / (genericLength predicciones)


--------------------------------------------------------------------------------
{- nFoldCrossValidation: Esta función termina siendo el promedio de una lista
definida por comprensión, donde cada uno de sus elementos es la precisión
(accuracy) de utilizar una partición distinta como conjunto de validación, y
todas las demás como particiones de entrenamiento. Entonces, cada elemento
de esta lista se calcula primero separando los datos en las particiones
(mediante separarDatos) para luego aplicar mediante map el modelo vecinos
más cercanos con K=15 a cada instancia de la partición de validación y luego
calcular el accuracy de dichas aplicaciones.
-}
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean [ obtenerAccuracy (separarDatos datos etiquetas n fold) | fold <- [1..n] ]
    where obtenerAccuracy (datos_train, datos_val, etiquetas_train, etiquetas_val) = accuracy (map (knn 15 datos_train etiquetas_train distEuclideana) datos_val) etiquetas_val
