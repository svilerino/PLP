-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
            "split" ~: testsSplit,
            "longitudPromedioPalabras" ~: testsLongitudPromedioPalabras,
            "cuentas" ~: testsCuentas,
            "repeticionesPromedio" ~: testsRepeticionesPromedio,
            "frecuenciaTokens" ~: testsFrecuenciaTokens,
            "normalizarExtractor" ~: testsNormalizarExtractor,
            "extraerFeatures" ~: testsExtraerFeatures,
            "distEuclideana" ~: testsDistEuclideana,
            "distCoseno" ~: testsDistCoseno,
            "knn" ~: testsKnn,
            "separarDatos" ~: testsSepararDatos,
            "accuracy" ~: testsAccuracy,
            "nFoldCrossValidation" ~: testsNFoldCrossValidation
            ]

testsSplit = test [
            split ',' "" ~?= [],
            split ',' ",,,,,,," ~?= [],
            split '|' "Diego |Armando| Maradona" ~?= ["Diego ","Armando"," Maradona"],
            split ',' ",PLP," ~?= ["PLP"],
            split ',' " ,PLP, " ~?= [" ","PLP"," "],
            split ',' "hola PLP, bienvenidos!" ~?= ["hola PLP"," bienvenidos!"]
            ]

testsLongitudPromedioPalabras = test [
            longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4,
            longitudPromedioPalabras "" ~?= 0.0,
            longitudPromedioPalabras "Uno uno uNo unO" ~?= 3.0,
            longitudPromedioPalabras "Saracantunga,todos,queremos,saracatunga" ~?= 39.0
            ]

testsCuentas = test [
            cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
            cuentas "" ~?= [],
            cuentas ['x','x'] ~?= [(2,'x')], --The most interesting man in the world
            cuentas [1,2,3] ~?= [(1,1),(1,2),(1,3)],
            cuentas "alfa" ~?= [(2,'a'),(1,'l'),(1,'f')]
            ]

testsRepeticionesPromedio = test [
            repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5,
            repeticionesPromedio "" ~?= 0.0,
            repeticionesPromedio "1 2 3 4" ~?= 1.0,
            repeticionesPromedio "Maradona, Maradona" ~?= 1.0
            ]

testsFrecuenciaTokens = test [
            (head frecuenciaTokens) "use_snake_case !" ~?= 0.125,
            all (\f -> f "" == 0.0) frecuenciaTokens ~?= True,
            all (\f -> f tokens == 1/(genericLength tokens)) frecuenciaTokens ~?= True,
            map (\f -> f "€") frecuenciaTokens ~?= take (length tokens) (repeat 0.0)
            ]

ts = ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"]

{- Con ts ya sabemos que longitudPromedioPalabras dará más de 1, y con
repeticiones promedio pasa exactamente lo contrario.
-}
testsNormalizarExtractor = test [
                normalizarExtractor [] longitudPromedioPalabras "Hola, que tul?" ~?= longitudPromedioPalabras "Hola, que tul?",
                normalizarExtractor [] repeticionesPromedio "Hola, que tul?" ~?= repeticionesPromedio "Hola, que tul?",
                normalizarExtractor ts longitudPromedioPalabras "" ~?= longitudPromedioPalabras "",
                normalizarExtractor ts repeticionesPromedio "" ~?= repeticionesPromedio "",
                normalizarExtractor ts longitudPromedioPalabras "" ~?= repeticionesPromedio "",
                all (\x -> x>=(-1) && 1>=x) (map (normalizarExtractor ts longitudPromedioPalabras) ts) ~?= True,
                all (\x -> x>=(-1) && 1>=x) (map (normalizarExtractor ts repeticionesPromedio) ts) ~?= True
            ]

testsExtraerFeatures = test [
            extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ts ~?= [[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]],
            extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] [] ~?= [],
            extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] [""] ~?= [[0.0,0.0]],
            extraerFeatures [repeticionesPromedio, longitudPromedioPalabras] (head ts : [""] ++ tail ts) ~?= [[0.6666667,0.33333334],[0.0,0.0],[1.0,0.12962963],[0.6666667,1.0]]
            ]

testsDistEuclideana = test [
            distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464,
            distEuclideana [0.0,0.0] [0.0,0.0] ~?= 0.0,
            distEuclideana [1.0,1.0] [1.0,1.0] ~?= 0.0,
            distEuclideana [0] [0] ~?= 0,
            distEuclideana [0] [4] ~?= 4,
            distEuclideana [0.0,0.0] [1.0,1.0] ~?= sqrt(2),
            distEuclideana [0.0,0.0] [-1.0,-1.0] ~?= sqrt(2),
            distEuclideana [0.5,0.5] [-0.5,-0.5] ~?= sqrt(2)
            ]

testsDistCoseno = test [
            distCoseno [0,3,4] [0,-3,-4]  ~?= -1,
            distCoseno [4] [1] ~?= 1.0,
            distCoseno [1,0] [0,1] ~?= 0
            ]

testsKnn = test [
            (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "f",
            (knn 1 [] [] distEuclideana) [10] ~?= "i", --Caso Borde (valor default si no hay datos de entrenamiento)
            (knn 1 [[0]] ["f"] distEuclideana) [10] ~?= "f", --Caso Borde
            (knn 1 [[0],[0]] ["i","f"] distEuclideana) [0] ~?= "f", --Caso Borde (Debe ser "f" pues el sort termina ordenando alfabeticamente las etiquetas en caso de empate en la distancia)
            (knn 1 [[0],[2],[5],[10]] ["i","f","i","f"] distEuclideana) [-100] ~?= "i", --Test de cantidad de Vecinos
            (knn 3 [[0],[2],[5],[10]] ["i","f","f","f"] distEuclideana) [-100] ~?= "f", --Test de modaEstadistica
            (knn 1 [[0],[2],[5],[10]] ["i","f","i","f"] distEuclideana) [4] ~?= "i", --Test de Cercania
            (knn 3 [[0],[2],[5],[10]] ["i","f","f","f"] distEuclideana) [6] ~?= "f" --Test de Cercania
            ]

xs = [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] :: Datos
y = ["1","2","3","4","5","6","7"]

testsSepararDatos = test [
            separarDatos xs y 3 2 ~?= ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]], [[3.0,3.0],[4.0,4.0]], ["1","2","5","6"], ["3","4"]),
            separarDatos xs y 1 1 ~?= ([], xs , [], y),
            separarDatos xs y (length xs) 1 ~?= (tail xs, [head xs], tail y, [head y])
            ]

testsAccuracy = test [
            accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6,
            accuracy ["f"] ["f"] ~?= 1,
            accuracy ["i"] ["f"] ~?= 0,
            accuracy ["f","f","f","f"] ["i","i","f","f"] ~?= 0.5
            ]

testsNFoldCrossValidation = test [
            nFoldCrossValidation 1 [[0],[5],[8],[10]] ["i","i","i","i"] ~?= 1, --Como hay una sola particion, no queda particion de entrenamiento, y se cae en el valor default
            nFoldCrossValidation 4 [[0],[5],[8],[10]] ["i","i","f","f"] ~?= 0, --Debe fallar porque se toman 15 vecinos
            nFoldCrossValidation 2 [[x] | x<-[1..20]] (take 10 (repeat "i") ++ take 10 (repeat "f")) ~?= 0, --Como son 2 particiones, y las etiquetas estan "particionadas" de igual manera, una particion nunca servirá para la otra
            nFoldCrossValidation 20 [[x] | x<-[1..20]] (foldr (\_ rec -> "i":"f":rec) [] (take 10 (repeat 1))) ~?= 0, --Como los vecinos son 15, tomé tantas particiones como datos hay y las etiquetas estan intercaladas, siempre sucedera que los vecinos estarán intercalados, y con impares vecinos tendre siempre un elemento más con la etiqueta errónea.
            nFoldCrossValidation 2 [[x] | x<-[1..20]] (foldr (\_ rec -> "i":"f":rec) [] (take 10 (repeat 1))) ~?= 0.5 --Se razona a partir del test previo.
            ]
