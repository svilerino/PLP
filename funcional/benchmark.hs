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

main = do
    (tags1, contents1) <- readAll "funcional"
    (tags2, contents2) <- readAll "imperativo"
    print $ "Funcional: " ++ (show $ length tags1) ++ " instancias"
    print $ "Imperativo: " ++ (show $ length tags2) ++ " instancias"
    let x = (contents1 ++ contents2)
    let y = (tags1 ++ tags2)
    shuffled <- shuffle (zip x y)
    let (x_shuffled, y_shuffled) = unzip shuffled
    let features = let extractores = (tfIdfTokens x_shuffled) in map (\texto -> map (\extractor -> extractor texto) extractores) x_shuffled

    print "Knn con Distancia Coseno"
    print $ show [(k, nFoldCrossValidationGenerico 5 features y_shuffled (crearKnn k distCoseno)) | k <- [3..21], k `mod` 2 == 1 ]
    print "KnnPesado con Distancia Coseno"
    print $ show [(k, nFoldCrossValidationGenerico 5 features y_shuffled (crearKnnPesado k distCoseno)) | k <- [3..21], k `mod` 2 == 1 ]
    print "Knn con Distancia Euclideana"
    print $ show [(k, nFoldCrossValidationGenerico 5 features y_shuffled (crearKnn k distEuclideana)) | k <- [3..21], k `mod` 2 == 1 ]
    print "KnnPesado con Distancia Euclideana"
    print $ show [(k, nFoldCrossValidationGenerico 5 features y_shuffled (crearKnnPesado k distEuclideana)) | k <- [3..21], k `mod` 2 == 1 ]
