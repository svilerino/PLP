import System.IO
import Control.Monad
import System.Directory
import Data.List
import Tp
import Ejercicio13
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

constructorFeatures cant = (\textos -> let 
    extractores = (tfIdfTokens textos) ++ (tfIdfTerminos cant textos)
    in extractores)
    
evaluarModelo vecinos cantTerminos textos etiquetas = do
    let res = nFoldCrossValidationGenerico 5 textos etiquetas (constructorFeatures cantTerminos) (\x y -> knnPesado vecinos x y distCosenoPosta)
    print $ "Accuracy promedio KnnPesado " ++ (show vecinos) ++ " distCosenoPosta y Features TF-IDF de los tokens y las palabras que aparecen en mas de " ++ (show cantTerminos) ++ " programas: " ++ (show res)
    return res

main = do
    (tags1, contents1) <- readAll "funcional"
    (tags2, contents2) <- readAll "imperativo"
    print $ "Funcional: " ++ (show $ length tags1) ++ " instancias"
    print $ "Imperativo: " ++ (show $ length tags2) ++ " instancias"
    let x = (contents1 ++ contents2)
    let y = (tags1 ++ tags2)
    shuffled <- shuffle (zip x y)
    let (x_shuffled, y_shuffled) = unzip shuffled
    forM_ [500, 250, 100, 50, 25, 10, 5, 2] (\terminos -> do
        forM_ [3, 5, 7, 9, 11, 13, 15, 17, 19, 21] (\vecinos -> do
            evaluarModelo vecinos terminos x_shuffled y_shuffled))
    