module Ejercicio13 where

import Tp
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

-- Inverse Document Frecuency: inverso de la cantidad de doucmentos en los que el termino aparece
idf :: Ord a => a -> [Set a] -> Float
idf termino sets = log (genericLength sets / (1.0 + (sum (map (\set -> boolAFloat(Set.member termino set)) sets))))

-- TF-IDF: Term Frecuency (apariciones del termino en el texto) multiplicado por IDF del termino
tfIdf :: Ord a => a -> [Set a] -> ([a] -> Feature)
tfIdf termino sets = \texto -> let 
    frecuenciaToken = apariciones termino texto
    resultado     | frecuenciaToken == 0 = 0
                        | otherwise = fromIntegral (frecuenciaToken) * (idf termino sets)
    in resultado

-- Para ser eficientes, de cada texto extraemos un Set con los tokens que aparecen en el mismo
setAparicionesTokens :: [Texto] -> [Set Char]
setAparicionesTokens textos = map (\texto -> Set.fromList [token | token <- tokens, token `elem` texto] ) textos 

-- Conjunto de extractores tfIdf (uno por cada token)
tfIdfTokens :: [Texto] -> [Extractor]
tfIdfTokens textos = let sets = setAparicionesTokens textos in [ tfIdf token sets | token <- tokens ]

-- El enunciado esta conceptualmente mal, la funcion distCoseno en realidad implementa la similitud
-- Coseno, que es lo opuesto de la distancia. Con esta nueva distancia el clasificador deberia andar mejor
distCosenoPosta :: Medida
distCosenoPosta p q = 1 - distCoseno p q

-- Dado una lista de elementos con pesos, da una nueva lista con una unica entrada por elemento, y la
-- suma de todos los pesos de ese elemento en la lista original
sumarPesos :: (Eq a, Ord a) => [(Float, a)] -> [(Float, a)]
sumarPesos xs = foldr (\x rec -> if null rec then [x] else chequearYAgregar x rec) [] (sort xs) 
    where 
        chequearYAgregar x rec = if snd (head rec) == snd x then agregarACabeza x rec else [x] ++ rec
        agregarACabeza x rec = [(fst x + fst (head rec), snd x)] ++ tail rec

-- Moda con Peso: En lugar de contar las apariciones, compara por la suma de los pesos de los elementos
modaEstadisticaPesada :: (Eq a, Ord a) => [(Float, a)] -> a
modaEstadisticaPesada xs = snd (foldr (\x rec -> if fst x > fst rec then x else rec) (head xs) (sumarPesos xs))

invertirDistancias :: [(Float, a)] -> [(Float, a)]
invertirDistancias = map (\(peso, etiqueta) -> (1.0 /  peso, etiqueta))

-- Weighted K Nearest Neighbours: Algoritmo similar a KNN, pero a cada uno de los K vecinos le asigna un peso
-- correspondiente a el inverso de las distancias. De esta forma, los mas cercanos tienen mas voto
knnPesado :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knnPesado k datos etiquetas medida = \instancia -> modaEstadisticaPesada (invertirDistancias (take k (sort (zip (distanciasAInstancia datos medida instancia) etiquetas))))

type ConstructorModelo = (Datos -> [Etiqueta] ->Modelo)
type ConstructorExtractores = ([Texto] -> [Extractor])

extraerFeaturesSinNorm :: [Extractor] -> [Texto] -> Datos
extraerFeaturesSinNorm extractores textos = map (\texto -> map (\extractor -> extractor texto) extractores) textos

mediaPorColumna :: [[Float]] -> [Float]
mediaPorColumna xss = [ mean (map (!!(col - 1)) xss) | col <-[1..length (head xss)] ]

aplicarFold :: ([Texto], [Etiqueta], [Texto], [Etiqueta]) -> ConstructorExtractores -> [ConstructorModelo] -> [Float]
aplicarFold (x_train, x_val, y_train, y_val) constructorExtractores constructoresModelo = let
    extractores = constructorExtractores x_train
    features_train = extraerFeaturesSinNorm (extractores) x_train
    features_val = extraerFeaturesSinNorm (extractores) x_val
    modelos = map (\constructorModelo -> constructorModelo features_train y_train) constructoresModelo
    in map (\modelo -> accuracy (map modelo features_val) y_val) modelos

separarDatosGenerico :: [a] -> [b] -> Int -> Int -> ([a], [a], [b], [b])
separarDatosGenerico datos etiquetas n p = (obtenerSalvoParticion n p datos, obtenerParticion n p datos, obtenerSalvoParticion n p etiquetas, obtenerParticion n p etiquetas)

-- N Fold CrossValidation aplicable genericamente a varios modelos: dada una lista de funciones que "entrenan"  modelos tomando un
-- conjunto de datos y etiquetas y devolviendo el modelo y lo utiliza para hacer el cross validation
nFoldCrossValidationGenerico :: Int -> [Texto] -> [Etiqueta] -> ConstructorExtractores -> [ConstructorModelo] -> [Float]
nFoldCrossValidationGenerico n textos etiquetas constructorExtractores constructoresModelo = let
    aplicaciones = [ aplicarFold (separarDatosGenerico textos etiquetas n fold) constructorExtractores constructoresModelo | fold <- [1..n] ]
    in mediaPorColumna aplicaciones