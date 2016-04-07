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
    print $ "Accuracy promedio knnPesado 3: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 3 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 5: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 5 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 7: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 7 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 9: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 9 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 11: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 11 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 13: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 13 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 15: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 15 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 17: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 17 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 19: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 19 x y distEuclideana))
    print $ "Accuracy promedio knnPesado 21: " ++ (show $ tryClassifierGenerico x_shuffled y_shuffled (\x y -> knnPesado 21 x y distEuclideana))
    print $ "Accuracy promedio: " ++ (show $ tryClassifier x_shuffled y_shuffled)
    -- print $ tryClassifierUnk x_shuffled y_shuffled contentsUnk
    let length_class_1 = genericLength (filter (\x -> x == (head y)) y)
    let random_acc = length_class_1 / (genericLength y)
    print $ "random value: " ++ show random_acc
