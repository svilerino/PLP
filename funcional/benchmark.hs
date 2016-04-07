import System.IO
import System.Directory
import Data.List
import Tp
import Ejercicio13
--import Solucion
import System.Random

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do setStdGen $ mkStdGen 1234
                randomPosition <- getStdRandom $ randomR (0, length xs - 1)
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

procesarArchivo archivo = do
                            hndl <- openFile archivo ReadMode
                            contents <- hGetContents hndl
                            putStr $ if (length contents) > 0 then "" else ""
                            hClose hndl
                            return contents

readAll category = do
  let folder = "./" ++ category ++ "/"
  all <- getDirectoryContents folder
  let filtered =  filter (isSuffixOf ".txt") all
  let tags = (replicate (length filtered) category)
  setCurrentDirectory folder
  contents <- mapM (\x -> procesarArchivo x) filtered
  setCurrentDirectory "../"
  return $ (tags, contents)

crearKnn :: Int -> Medida -> ConstructorModelo
crearKnn k medida = (\x y -> knn k x y medida)

crearKnnPesado :: Int -> Medida -> ConstructorModelo
crearKnnPesado k medida = (\x y -> knnPesado k x y medida)

mejorModelo :: (Int, Float) -> (Int, Float) -> (Int, Float)
mejorModelo (k1, score1) (k2, score2) = if score1 > score2 then (k1, score1) else (k2, score2)

correrConModelo :: [Texto] -> [Etiqueta] -> ConstructorExtractores -> (Int -> Medida -> ConstructorModelo) -> Medida -> [(Int, Float)]
correrConModelo textos etiquetas constructorExtractores metaConstructorModelo medida = let
        cantVecinos = [ k | k <- [3..21], k `mod` 2 == 1 ]
        constructoresModelo =[metaConstructorModelo k medida | k <- cantVecinos ]
        resultados = nFoldCrossValidationGenerico 5 textos etiquetas constructorExtractores constructoresModelo
        in zip cantVecinos resultados

--correrConExtractores :: String -> [Texto] -> [Etiqueta] -> ConstructorExtractores -> IO (String, (String, Int, Float))
correrConExtractores nombreFeatures textos etiquetas constructorExtractores = do
    print $ "Features: " ++ nombreFeatures
    print "Knn con Distancia Euclideana"
    let knn_euclid_res = correrConModelo textos etiquetas constructorExtractores (crearKnn) distEuclideana 
    let knn_euclid_mejor = foldr1 mejorModelo knn_euclid_res
    print $ show knn_euclid_res
    print $ show knn_euclid_mejor
    print "KnnPesado con Distancia Euclideana"
    let knnPesado_euclid_res = correrConModelo textos etiquetas constructorExtractores (crearKnnPesado) distEuclideana 
    let knnPesado_euclid_mejor = foldr1 mejorModelo knnPesado_euclid_res
    print $ show knnPesado_euclid_res
    print $ show knnPesado_euclid_mejor
    print "Knn con Distancia Coseno"
    let knn_coseno_res = correrConModelo textos etiquetas constructorExtractores (crearKnn) distEuclideana 
    let knn_coseno_mejor = foldr1 mejorModelo knn_coseno_res
    print $ show knn_coseno_res
    print $ show knn_coseno_mejor
    print "KnnPesado con Distancia Coseno"
    let knnPesado_coseno_res = correrConModelo textos etiquetas constructorExtractores (crearKnnPesado) distEuclideana 
    let knnPesado_coseno_mejor = foldr1 mejorModelo knnPesado_coseno_res
    print $ show knnPesado_coseno_res
    print $ show knnPesado_coseno_mejor
    let mejor = foldr1 mejorModelo [knn_euclid_mejor, knnPesado_euclid_mejor, knn_coseno_mejor, knnPesado_coseno_mejor]
    return (nombreFeatures, mejor)

constructorFeatures1 :: ConstructorExtractores
constructorFeatures1 = (\_ -> [longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens)

constructorFeatures2 :: ConstructorExtractores
constructorFeatures2 = (\textos -> tfIdfTokens textos)

constructorFeatures3 :: ConstructorExtractores
constructorFeatures3 = (\textos -> [longitudPromedioPalabras, repeticionesPromedio] ++ (tfIdfTokens textos))

main = do
    (tags1, contents1) <- readAll "funcional"
    (tags2, contents2) <- readAll "imperativo"
    print $ "Funcional: " ++ (show $ length tags1) ++ " instancias"
    print $ "Imperativo: " ++ (show $ length tags2) ++ " instancias"
    let x = (contents1 ++ contents2)
    let y = (tags1 ++ tags2)
    shuffled <- shuffle (zip x y)
    let (x_shuffled, y_shuffled) = unzip shuffled
    resultFeatures1 <- correrConExtractores "Enunciado" x_shuffled y_shuffled constructorFeatures1
    resultFeatures2 <- correrConExtractores "TF-Idf Tokens" x_shuffled y_shuffled constructorFeatures2
    resultFeatures3 <- correrConExtractores "TF-Idf Tokens, Repeticiones y LongitudPromedioPalabras Normalizado" x_shuffled y_shuffled constructorFeatures3
    print $ show resultFeatures1
    print $ show resultFeatures2
    print $ show resultFeatures3
    print "Fin"