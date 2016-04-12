module Ejercicio13 where

import Tp
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

-- Document Frecuency: cantidad de doucmentos en los que el termino aparece
-- Se espera que cada set contenga los terminos que aparecen en el texto correspondiente
df :: Ord a => [Set a] -> a -> Float
df sets termino = sum (map (\set -> boolAFloat(Set.member termino set)) sets)
        
-- Inverse Document Frecuency: inverso de la document frecuency. Se le suma 1 al denominador para
-- evitar dividir por cero
idf :: Ord a => [Set a] ->  a -> Float
idf sets termino = log (genericLength sets / (1.0 + (df sets termino)))

-- TF-IDF: Term Frecuency (apariciones del termino en el elemento, ya sea buscando tokens en un texto,
-- o palabras en arreglos de texto) multiplicado por IDF del termino
tfIdf :: Ord a => [Set a] -> a -> ([a] -> Feature)
tfIdf sets termino = \elemento -> let 
    frecuenciaToken = apariciones termino elemento
    resultado     | frecuenciaToken == 0 = 0
                  | otherwise = fromIntegral (frecuenciaToken) * (idf sets termino)
    in resultado

-- Para ser eficientes, de cada texto extraemos un Set con los tokens que aparecen en el mismo
setAparicionesTokens :: [Texto] -> [Set Char]
setAparicionesTokens textos = map (\texto -> Set.fromList [token | token <- tokens, token `elem` texto] ) textos 

-- Conjunto de extractores tfIdf (uno por cada token)
tfIdfTokens :: [Texto] -> [Extractor]
tfIdfTokens textos = let sets = setAparicionesTokens textos in [ tfIdf sets token | token <- tokens ]

-- Version mas eficiente que nub, usando Sets
unicos :: (Ord a) => [a] -> [a]
unicos xs = Set.toList (Set.fromList xs)

-- Conjunto de extractores tfIdf (uno por cada palabra en el dataset que aparece mas de apariciones_minimas veces)
-- terminos representa cada termino que aparece en cualquier texto del conjunto de textos. Terminos importantes 
-- representa a los que aparecen en mas de apariciones_minimas textos. Finalmente, sets_filtrados son los sets
-- que contienen unicamente a los terminos importantes
tfIdfTerminos :: Float -> [Texto] -> [Extractor]
tfIdfTerminos apariciones_minimas textos = let 
    spliteados = map (split ' ') textos
    terminos = unicos (concat spliteados)
    sets = map (Set.fromList) spliteados 
    terminos_importantes = filter (\termino -> df sets termino > apariciones_minimas) terminos
    sets_filtrados = map (\texto_spliteado -> Set.fromList (filter (\termino -> termino `elem` terminos_importantes) texto_spliteado)) spliteados
    in [ (\texto -> (tfIdf sets_filtrados termino) (split ' ' texto)) | termino <- terminos_importantes ]
    
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

-- No siempre queremos normalizar en nuestros modelos
extraerFeaturesSinNorm :: [Extractor] -> [Texto] -> Datos
extraerFeaturesSinNorm extractores textos = map (\texto -> map (\extractor -> extractor texto) extractores) textos


-- Calcula el accuracy dado un set de entrenamiento y uno de validacion, para el modelo construido con constructorModelo.
-- Primero construye los extractores, luego computa los features con estos, despues crea el modelo usando solo los features
-- de entrenamiento  y finalmente realiza el calculo de la accuracy
aplicarParticion :: ([Texto], [Etiqueta], [Texto], [Etiqueta]) -> ConstructorExtractores -> ConstructorModelo -> Float
aplicarParticion (x_train, x_val, y_train, y_val) constructorExtractores constructorModelo = let
    extractores = constructorExtractores x_train
    features_train = extraerFeaturesSinNorm (extractores) x_train
    features_val = extraerFeaturesSinNorm (extractores) x_val
    modelo = constructorModelo features_train y_train
    in accuracy (map modelo features_val) y_val

-- Version de Separar Datos que sirve tanto para [Texto] como para Datos, porque como nuestros extractores necesitan los textos
-- para ser construidos, necesitamos generar los folds sobre los textos
separarDatosGenerico :: [a] -> [b] -> Int -> Int -> ([a], [a], [b], [b])
separarDatosGenerico datos etiquetas n p = (obtenerSalvoParticion n p datos, obtenerParticion n p datos, obtenerSalvoParticion n p etiquetas, obtenerParticion n p etiquetas)

-- N Fold CrossValidation aplicable genericamente: dada una funcion que "entrena" un modelo tomando un conjunto de textos y etiquetas
-- y devolviendo el modelo, utiliza el modelo creado para hacer el cross validation. Tambien toma una funcion
-- que dado los textos, devuelve la lista de extractores a usar (TF-IDF necesita el dataset para crear el extractor).
-- Con todo esto, esta funcion aplica nFoldCrossValidation sobre el modelo.
nFoldCrossValidationGenerico :: Int -> [Texto] -> [Etiqueta] -> ConstructorExtractores -> ConstructorModelo -> Float
nFoldCrossValidationGenerico n textos etiquetas constructorExtractores constructorModelo = let
    aplicaciones = map (\fold -> aplicarParticion (separarDatosGenerico textos etiquetas n fold) constructorExtractores constructorModelo) [1..n]
    in mean aplicaciones