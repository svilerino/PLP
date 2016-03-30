import System.IO
import System.Directory
import Data.List
import Tp
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
    (tagsUnk, contentsUnk) <- readAll "alumnos"
    print $ "Funcional: " ++ (show $ length tags1) ++ " instancias"
    print $ "Imperativo: " ++ (show $ length tags2) ++ " instancias"
    let x = (contents1 ++ contents2)
    let y = (tags1 ++ tags2)
    shuffled <- shuffle (zip x y)
    let (x_shuffled, y_shuffled) = unzip shuffled
    print $ tryClassifier x_shuffled y_shuffled
    print $ tryClassifierUnk x_shuffled y_shuffled contentsUnk
    let length_class_1 = genericLength (filter (\x -> x == (head y)) y)
    let random_acc = length_class_1 / (genericLength y)
    print $ "random value: " ++ show random_acc
