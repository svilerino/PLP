import System.IO
import System.Directory
import Data.List
import Tp
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

crossValidate :: String -> (Int -> Medida -> ConstructorModelo) -> Medida -> Datos -> [Etiqueta] -> ((String, Int, Float), [(Int, Float)])
crossValidate nombreModelo constructorKnn medida datos etiquetas = let
                                             resultados = [(k, nFoldCrossValidationGenerico 5 datos etiquetas (constructorKnn k medida)) | k <- [3..21], k `mod` 2 == 1 ]
                                             (mejorK, mejorScore) = foldr (\(k, score) rec -> if score > (snd rec) then (k, score) else rec) (head resultados) resultados
                                             in ((nombreModelo, mejorK, mejorScore), resultados)
                                             
mejorModelo :: (String, Int, Float) -> (String, Int, Float) -> (String, Int, Float)
mejorModelo (nombre1, k1, score1) (nombre2, k2, score2) = if score1 > score2 then (nombre1, k1, score1) else (nombre2, k2, score2)
                                      
correrConFeatures :: String -> Datos -> [Etiqueta] -> IO (String, (String, Int, Float))
correrConFeatures nombreFeatures datos etiquetas = do
    print $ "Features: " ++ nombreFeatures
    print "Knn con Distancia Euclideana"
    let (knn_euclid_mejor, knn_euclid_res) = crossValidate "Knn_DistEuclideana" (crearKnn) distEuclideana datos etiquetas  
    print $ show knn_euclid_res
    print "KnnPesado con Distancia Euclideana"
    let (knnPesado_euclid_mejor, knnPesado_euclid_res) = crossValidate "KnnPesado_DistEuclideana" (crearKnnPesado) distEuclideana datos etiquetas  
    print $ show knnPesado_euclid_res
    print "Knn con Distancia Coseno"
    let (knn_cosine_mejor, knn_cosine_res) = crossValidate "Knn_DistCoseno" (crearKnn) distCosenoPosta datos etiquetas  
    print $ show knn_cosine_res
    print "KnnPesado con Distancia Coseno"
    let (knnPesado_cosine_mejor, knnPesado_cosine_res) = crossValidate "KnnPesado_DistCoseno" (crearKnnPesado) distCosenoPosta datos etiquetas  
    print $ show knnPesado_cosine_res
    let mejor = foldr1 mejorModelo [knn_euclid_mejor, knnPesado_euclid_mejor, knn_cosine_mejor, knnPesado_cosine_mejor]
    return (nombreFeatures, mejor)

main = do
    (tags1, contents1) <- readAll "funcional"
    (tags2, contents2) <- readAll "imperativo"
    print $ "Funcional: " ++ (show $ length tags1) ++ " instancias"
    print $ "Imperativo: " ++ (show $ length tags2) ++ " instancias"
    let x = (contents1 ++ contents2)
    let y = (tags1 ++ tags2)
    shuffled <- shuffle (zip x y)
    let (x_shuffled, y_shuffled) = unzip shuffled
    let features = let
                    result = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x_shuffled
                    in result
    let features2 = let 
                    tfIdfExtractores = (tfIdfTokens x_shuffled)
                    result = map (\texto -> map (\extractor -> extractor texto) tfIdfExtractores) x_shuffled
                    in result
    let features3 = let
                    tfIdfExtractores = (tfIdfTokens x_shuffled)
                    result = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ tfIdfExtractores) x_shuffled
                    in result
    resultFeatures <- correrConFeatures "Enunciado" features y_shuffled
    resultFeatures2 <- correrConFeatures "TF-Idf Tokens" features2 y_shuffled
    resultFeatures3 <- correrConFeatures "TF-Idf Tokens, Repeticiones y LongitudPromedioPalabras Normalizado" features3 y_shuffled
    print $ show resultFeatures
    print $ show resultFeatures2
    print $ show resultFeatures3