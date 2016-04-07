module Tp where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

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

{- split: Consultamos caracter a caracter (o elemento a elemento de la lista, en realidad)
si el mismo es el elemento separador. Si no lo es lo agregamos a la primera cadena/lista
del resultado (que es una lista de listas), caso contrario insertamos una nueva cadena/lista
vacia al comienzo del resultado.

Por ultimo, filtramos las posibles cadenas/listas vacias que tenga el resultado (esto sería
el paso 'trim' del que se habló en clase.
-}
split :: Eq a => a -> [a] -> [[a]]
split x xs = filter (not . null) (foldr (\y rec -> if y == x then []:rec else (y:head rec):(tail rec)) [[]] xs)


{- longitudPromedioPalabras: Convierte el parametro de entrada (de tipo Texto) en una lista
de palabras, y luego convierte cada palabra a un número representando la longitud de la misma.
Por último, calcula el promedio de todas estas longitudes.

Ej: "Hola, ¿Cómo te va? -> ["Hola,","¿Cómo","te","va?"] -> [5,5,2,3] -> 3.75
-}
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras texto = mean (map genericLength (split ' ' texto))


{- cuentas: Se toma cada elemento de la lista original (sin tomar en cuenta los repetidos,
y se cuenta la cantidad de apariciones que tiene en la lista original. Luego "agrega" la
tupla <#aparciones(elemento),elemento> al resultado final) 
-}
apariciones :: Eq a => a -> [a] -> Int
apariciones x xs = length $ filter (x==) xs

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = [(apariciones x xs, x) | x <- nub xs]


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
repeticionesPromedio texto = mean (map (fromIntegral . fst) (cuentas (split ' ' texto)))


{- frecuenciasTokens: Crea una lista por comprensión de funciones (expresadas como
funciones lambda). Cada una de estas funciones tienen un token fijo, y al pasarles
un texto calculan la frecuencia relativa de dicho token respecto de la longitud
del texto.
-}
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ \texto -> (fromIntegral (apariciones token texto)) / (fromIntegral (length texto)) | token <- tokens]


{- normalizarExtractor: Básicamente, tomamos el resultado del extractor para cada
texto (feature) y normalizamos a cada uno de ellos dividiendolo por el de mayor
valor absoluto (el màximo feature en módulo). Es decir, normalizamos los features
de un extractor para una lista de textos relativo al feature de mayor valor
absoluto.
-}
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = let
                                                                    features = map extractor textos -- Renombre para el feature obtenido con el extractor para cada texto
                                                                    escala = foldl (\rec x -> max rec (abs x)) 0 features --Calculamos el maximo de los features
                                                                    in (\texto -> (extractor texto) / (if escala > 0 then escala else 1)) --Funcion devuelta/definida


{- extraerFeatures: Primero "transformamos" (abuso de léxico, nada se "transforma"
en funcional) los extractores en extractores normalizados relativos a los textos.
Luego aplicamos estos extractores normalizados a cada uno de los textos.
-}
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos = let
                                                                extractoresNorm = map (\extractor -> normalizarExtractor textos extractor) extractores
                                                                in  map (\texto -> map (\extractor -> extractor texto) extractoresNorm) textos


{- distEuclideana: Trivial (muy). No creemos que requiera aclaraciones.
-}
distEuclideana :: Medida
distEuclideana p q = sqrt (sum (map (**2) (zipWith (-) p q)))


{- distCoseno: Idem distEuclideana
-}
prodVectorial :: [Float] -> [Float] -> Float
prodVectorial p q = sum (zipWith (*) p q)

normaVectorial :: [Float] -> Float
normaVectorial p = sqrt (prodVectorial p p)

distCoseno :: Medida
distCoseno p q = (prodVectorial p q) / (normaVectorial p * normaVectorial q)


distanciasAInstancia :: Datos -> Medida -> Instancia -> [Float]
distanciasAInstancia datos medida instancia = map (medida instancia) datos

modaEstadistica :: Eq a => [a] -> a
modaEstadistica xs = snd (foldr (\x rec -> if fst x > fst rec then x else rec) (0, head xs) (cuentas xs))

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medida = \instancia -> modaEstadistica (map snd (take k (sort (zip (distanciasAInstancia datos medida instancia) etiquetas))))

obtenerSalvoParticion :: Int -> Int -> [a] -> [a]
obtenerSalvoParticion n p xs = let
                                                    longParticion = (length xs) `div` n
                                                    longPrimerParte = (p - 1) * longParticion
                                                    longUltimaParte = (n - p) * longParticion
                                                    longHastaUltimaParte = longPrimerParte + longParticion
                                                    in take longPrimerParte xs ++ take longUltimaParte (drop longHastaUltimaParte xs)

obtenerParticion :: Int -> Int -> [a] -> [a]
obtenerParticion n p xs = let
                                            longParticion = (length xs) `div` n
                                            longPrimerParte = (p - 1) * longParticion
                                            in take longParticion (drop longPrimerParte xs)

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = (obtenerSalvoParticion n p datos, obtenerParticion n p datos, obtenerSalvoParticion n p etiquetas, obtenerParticion n p etiquetas)

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy predicciones etiquetas= sum (map boolAFloat (zipWith (==)  predicciones etiquetas)) / (genericLength predicciones)
        where boolAFloat b
                    | b == True = 1.0
                    | otherwise = 0.0

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean [ obtenerAccuracy (separarDatos datos etiquetas n fold) | fold <- [1..n] ]
        where obtenerAccuracy (x_train, x_val, y_train, y_val) = accuracy (map (knn 15 x_train y_train distEuclideana) x_val) y_val

-- Hasta aca va el tp. Ahora implementamos un clasificador nuevo

idf :: Char -> [Set Char] -> Float
idf token sets = log (genericLength sets / (1.0 + (sum (map (\set -> boolAFloat(Set.member token set)) sets))))
    where boolAFloat b
                    | b == True = 1.0
                    | otherwise = 0.0

tfIdf :: Char -> [Set Char] -> Extractor                     
tfIdf token sets = \texto -> let 
                        frecuenciaToken = apariciones token texto
                        resultado   | frecuenciaToken == 0 = 0
                                    | otherwise = fromIntegral (frecuenciaToken) * (idf token sets)
                        in resultado

setAparicionesTokens :: [Texto] -> [Set Char]
setAparicionesTokens textos = map (\texto -> Set.fromList [token | token <- tokens, token `elem` texto] ) textos 
                        
tfIdfTokens :: [Texto] -> [Extractor]
tfIdfTokens textos = let sets = setAparicionesTokens textos in [ tfIdf token sets | token <- tokens ]

sumarPesos :: (Eq a, Ord a) => [(Float, a)] -> [(Float, a)]
sumarPesos xs = foldr (\x rec -> if null rec then [x] else chequearYAgregar x rec) [] (sort xs) 
                                where 
                                    chequearYAgregar x rec = if snd (head rec) == snd x then agregarACabeza x rec else [x] ++ rec
                                    agregarACabeza x rec = [(fst x + fst (head rec), snd x)] ++ tail rec

modaEstadisticaPesada :: (Eq a, Ord a) => [(Float, a)] -> a
modaEstadisticaPesada xs = snd (foldr (\x rec -> if fst x > fst rec then x else rec) (head xs) (sumarPesos xs))

invertirDistancias :: [(Float, a)] -> [(Float, a)]
invertirDistancias = map (\(peso, etiqueta) -> (1.0 /  peso, etiqueta))

knnPesado :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knnPesado k datos etiquetas medida = \instancia -> modaEstadisticaPesada (invertirDistancias (take k (sort (zip (distanciasAInstancia datos medida instancia) etiquetas))))

type ConstructorModelo = (Datos -> [Etiqueta] ->Modelo)

nFoldCrossValidationGenerico :: Int -> Datos -> [Etiqueta] -> ConstructorModelo -> Float
nFoldCrossValidationGenerico n datos etiquetas constructorModelo = mean [ obtenerAccuracy (separarDatos datos etiquetas n fold) | fold <- [1..n] ]
        where obtenerAccuracy (x_train, x_val, y_train, y_val) = accuracy (map (constructorModelo x_train y_train) x_val) y_val
