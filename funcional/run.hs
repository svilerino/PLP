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

constructorFeatures1 = (\textos -> let 
    extractores = (tfIdfTokens textos)
    in extractores)

constructorFeatures2 = (\textos -> let 
    extractores = (tfIdfTokens textos) ++ (tfIdfTerminos 100 textos)
    in extractores)
    
constructorFeatures3 = (\textos -> let 
    extractores = (tfIdfTokens textos) ++ (tfIdfTerminos 25 textos)
    in extractores)
    
constructorModelo1 = (\x y -> knnPesado 11 x y distCosenoPosta)

constructorModelo2 = (\x y -> knnPesado 5 x y distCosenoPosta)

constructorModelo3 = (\x y -> knnPesado 13 x y distCosenoPosta)

main = do
    (tags1, contents1) <- readAll "funcional"
    (tags2, contents2) <- readAll "imperativo"
    -- (tagsUnk, contentsUnk) <- readAll "alumnos"
    print $ "Funcional: " ++ (show $ length tags1) ++ " instancias"
    print $ "Imperativo: " ++ (show $ length tags2) ++ " instancias"
    let x = (contents1 ++ contents2)
    let y = (tags1 ++ tags2)
    shuffled <- shuffle (zip x y)
    let (x_shuffled, y_shuffled) = unzip shuffled
    print $ "Accuracy promedio Knn 15 distEuclideana y Features del Enunciado: " ++ (show $ tryClassifier x_shuffled y_shuffled)
    print $ "Accuracy promedio KnnPesado 11 distCosenoPosta y Features TF-IDF de los tokens: " ++ (show $ nFoldCrossValidationGenerico 5 x_shuffled y_shuffled constructorFeatures1 constructorModelo1)
    print $ "El siguiente modelo puede tardar varios minutos en correr"
    print $ "Accuracy promedio KnnPesado 5 distCosenoPosta y Features TF-IDF de los tokens y las palabras que aparecen en mas de 100 programas: " ++ (show $ nFoldCrossValidationGenerico 5 x_shuffled y_shuffled constructorFeatures2 constructorModelo2)
    print $ "El siguiente modelo puede tardar mas de quince minutos en correr"
    print $ "Accuracy promedio KnnPesado 13 distCosenoPosta y Features TF-IDF de los tokens y las palabras que aparecen en mas de 25 programas: " ++ (show $ nFoldCrossValidationGenerico 5 x_shuffled y_shuffled constructorFeatures3 constructorModelo3)
    -- print $ tryClassifierUnk x_shuffled y_shuffled contentsUnk
    let length_class_1 = genericLength (filter (\x -> x == (head y)) y)
    let random_acc = length_class_1 / (genericLength y)
    print $ "random value: " ++ show random_acc
