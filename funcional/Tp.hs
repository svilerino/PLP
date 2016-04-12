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
{- split: La función pasada a foldr lo que hace es agregar caracteres (mientras
no sea un caracter separador) al primer elemento de la lista de resultado (un
string). En el caso base se agrega un string vacío dentro de la lista resultado,
dado que en caso de que el primer caracter leido sea un separador, la lista/string
vacío resultante sera eliminado del resultado final mediante el filter (ya que
como se explico en clase, había que hacer un 'trim' de los resultados).

Ej: split ',' ",Saraza,,Saraza,"
	-> [[],"Saraza",[],"Saraza",[],[]]
	-> ["Saraza","Saraza"]
-}
split :: Eq a => a -> [a] -> [[a]]
split x xs = filter (not . null) (foldr (\y rec -> if y == x then []:rec else (y:head rec):(tail rec)) [[]] xs)


--------------------------------------------------------------------------------
{- longitudPromedioPalabras: No requiere explicación

Ej: longitudPromedioPalabras "Hola, ¿Cómo te va?
	-> ["Hola,","¿Cómo","te","va?"]
	-> [5,5,2,3]
	-> 3.75
-}
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras texto = case texto of
    [] -> 0.0
    otherwhise -> mean (map genericLength (split ' ' texto))


--------------------------------------------------------------------------------
{- cuentas: No Requiere explicación
-}
apariciones :: Eq a => a -> [a] -> Int
apariciones x xs = length $ filter (x==) xs

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = [(apariciones x xs, x) | x <- nub xs]


--------------------------------------------------------------------------------
{- repeticionesPromedio: No requiere explicación.

Ej: "Y dale, dale, dale dale, dale tense" 
	-> [[Y],[dale,],[dale,],[dale],[dale,],[dale],[tense]]
	-> [(1,"Y"),(3,"dale,"),(2,"dale"),(1,"tense")]
	-> [1.0,3.0,2.0,1.0]
	-> 1.75
-}
repeticionesPromedio :: Extractor
repeticionesPromedio texto = case texto of
    [] -> 0.0
    otherwhise -> mean (map (fromIntegral . fst) (cuentas (split ' ' texto)))


--------------------------------------------------------------------------------
{- frecuenciasTokens: Crea una lista por comprensión de funciones (expresadas como
funciones lambda). Cada una de estas funciones tienen un token fijo, y al pasarles
un texto calculan la frecuencia relativa de dicho token respecto de la longitud
del texto.

Se tomó en cuenta el caso en que si el texto que se le pasa a cada función de
frecuenciasTokens es nulo, entonces las funciones devuelvan 0 (tiene sentido
pensar que la frecuencia de cualquier token en un string vacio sea 0, que sería
idéntica a pasarle un texto donde dicho token no aparezca).

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

La decisión de usar "in" en lugar de "where" se consultó por mail, y se decidió
usar "in" por cuestiones de performance de la implementación de ghc.

No se muestra un ejemplo, ya que el resultado es un extractor.
-}
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = let
    features = map extractor textos  -- Renombre para el feature obtenido con el extractor para cada texto
    escala = foldl (\rec x -> max rec (abs x)) 0 features --Calculamos el maximo de los features
    in (\texto -> (extractor texto) / (if escala > 0 then escala else 1)) --Funcion devuelta/definida


--------------------------------------------------------------------------------
{- extraerFeatures: No requiere explicación.

No se muestra ejemplo, ya que los pasos intermedios representarian funciones,
las cuales no sabríamos como explicitar de forma escrita.
-}
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos = let
    extractoresNorm = map (\extractor -> normalizarExtractor textos extractor) extractores
    in  map (\texto -> map (\extractorNormalizado -> extractorNormalizado texto) extractoresNorm) textos


--------------------------------------------------------------------------------
{-distEuclideana: No requiere explicación.

Ej: distEuclideana [4,4] [2,2]
	-> [2,2]
	-> [4,4]
	-> 8
	-> 2.828427
-}
distEuclideana :: Medida
distEuclideana p q = sqrt (sum (map (**2) (zipWith (-) p q)))


--------------------------------------------------------------------------------
{-distCoseno: No requiere explicación.

Ej: distCoseno [4,4] [2,2] --Los ejemplos no muestran el orden de evaluación de Haskell
	-> sum [8,8] / (sqrt(sum [8,8]) * sqrt(sum [4,4]))
	-> 16 / (sqrt(16) * sqrt(8))
	-> 16 / (4*2.82)
	-> 1.4184

-}
prodVectorial :: [Float] -> [Float] -> Float
prodVectorial p q = sum (zipWith (*) p q)

normaVectorial :: [Float] -> Float
normaVectorial p = sqrt (prodVectorial p p)

distCoseno :: Medida
distCoseno p q = (prodVectorial p q) / (normaVectorial p * normaVectorial q)


--------------------------------------------------------------------------------
{- knn: Consideramos que la implementación de esta función es bastante legible
y no requiere de demasiados comentarios que lo expliquen. De lo que por ahí
requiere alguna aclaración es el uso del "if" en la función modaEstadística
y el uso de sort en la definición misma de knn.

Sobre lo primero, surge de considerar el caso en que el conjunto de etiquetas
sea vacío (esto puede ocurrir si se invoca a knn sin Datos ni Etiquetas -las
que les pasa nFoldCrossValidation como datos de entrenamiento-, aunque no tiene
mucho sentido tratar de usar este "programa" así). Entonces, consideramos
lógico que knn devolviese siempre una etiqueta default (en este caso, "i")
si no llegase a ser invocado con datos. Se podría haber usado una devolución
aleatoria de entre las etiquetas posibles como resultado (sería etiquetar de
manera random), pero lo desestimamos ya que al no ser deterministico no se
podría "testear".

Sobre lo segundo, la única aclaración es que sort ordena de menor a mayor,
y en caso de ordenar tuplas, las ordena primero según su primer componente,
y en caso de empate desempata según el orden de la componente siguiente
(de menor a mayor también).

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

modaEstadistica :: [Etiqueta] -> Etiqueta
modaEstadistica xs = if null xs then "i" else snd (foldr (\x rec -> if fst x > fst rec then x else rec) (0, head xs) (cuentas xs))

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medida = \instancia -> modaEstadistica (map snd (take k (sort (zip (distanciasAInstancia datos medida instancia) etiquetas))))


--------------------------------------------------------------------------------
{- separarDatos: No requiere explicación.
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
    in take longPrimerParte xs ++ take longUltimaParte (drop longHastaUltimaParte xs) {-se podría hacer con reverse reverse 
											y evitar un calculo, aunque reverse 
											no funciona rápido, especialmente con
											listas largas-}

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = (obtenerSalvoParticion n p datos, obtenerParticion n p datos, obtenerSalvoParticion n p etiquetas, obtenerParticion n p etiquetas)


--------------------------------------------------------------------------------
{- accuracy: No requiere explicación

Ej: accuracy ["i","f","f","i","i","f"] ["i","i","i","f","f","f"]
	-> [True,False,False,False,False,True]
	-> [1,0,0,0,0,1]
	-> 2 / 6
	-> 0.333333
-}
boolAFloat :: Bool -> Float
boolAFloat b = case b of
    True -> 1.0
    False -> 0.0

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy predicciones etiquetas = sum (map boolAFloat (zipWith (==)  predicciones etiquetas)) / (genericLength predicciones)


--------------------------------------------------------------------------------
{- nFoldCrossValidation: No requiere explicación.
-}
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean [ obtenerAccuracy (separarDatos datos etiquetas n particion_a_validar) | particion_a_validar <- [1..n] ]
    where obtenerAccuracy (datos_train, datos_val, etiquetas_train, etiquetas_val) = accuracy (map (knn 15 datos_train etiquetas_train distEuclideana) datos_val) etiquetas_val
