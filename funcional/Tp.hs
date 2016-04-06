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
tryClassifier x y = let xs = extraerFeaturesFast ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

split :: Eq a => a -> [a] -> [[a]]
split x xs = filter (not . null) (foldr (\y rec -> if y == x then []:rec else (if null rec then [[y]] else (y:head rec):(tail rec))) [] xs)

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras texto = mean (map genericLength (split ' ' texto))

apariciones :: Eq a => a -> [a] -> Int
apariciones x xs = length $ filter (x==) xs

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = [(apariciones x xs, x) | x <- nub xs]

repeticionesPromedio :: Extractor
repeticionesPromedio texto = mean (map (fromIntegral . fst) (cuentas (split ' ' texto)))

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ \xs -> (fromIntegral (apariciones token xs)) / (fromIntegral (length xs)) | token <- tokens]

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = (\texto -> (extractor texto) / (if escala > 0 then escala else 1))
    where
        features = map extractor textos        
        escala = foldl (\rec x -> max rec (abs x)) 0 features

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos = map (\texto -> map (\extractor -> (normalizarExtractor textos extractor) texto) extractores) textos

normalizarExtractorDadosFeatures :: [Feature] -> Extractor -> Extractor
normalizarExtractorDadosFeatures features extractor = (\texto -> (extractor texto) / (if escala > 0 then escala else 1))
    where escala = foldl (\rec x -> max rec (abs x)) 0 features

extraerFeaturesFast :: [Extractor] -> [Texto] -> Datos
extraerFeaturesFast extractores textos = map (\texto -> map (\extractor -> extractor texto) extractoresNormalizados) textos
    where
        extractoresNormalizados = zipWith normalizarExtractorDadosFeatures datosNoNormalizados extractores
        datosNoNormalizados = map (\extractor -> map (\texto -> extractor texto) textos) extractores

distEuclideana :: Medida
distEuclideana p q = sqrt (sum (map (**2) (zipWith (-) p q)))

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
obtenerSalvoParticion n p xs = take ((p - 1) * longParticion) xs ++ take ((n - p) * longParticion) (drop (p * longParticion) xs)
        where longParticion = (length xs) `div` n

obtenerParticion :: Int -> Int -> [a] -> [a]
obtenerParticion n p xs = take longParticion (drop ((p - 1) * longParticion) xs)
        where longParticion = (length xs) `div` n

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
