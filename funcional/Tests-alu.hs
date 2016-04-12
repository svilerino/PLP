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
            --"normalizarExtractor" ~: testsNormalizarExtractor,
            "extraerFeatures" ~: testsExtraerFeatures,
            "distEuclideana" ~: testsDistEuclideana,
            "distCoseno" ~: testsDistCoseno,
            "knn" ~: testsKnn,
            "separarDatos" ~: testsSepararDatos,
            "accuracy" ~: testsAccuracy--,
            --"nFoldCrossValidation" ~: testsNFoldCrossValidation
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

--testsNormalizarExtractor = test [
--            ]

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
            (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "f"
            ]

xs = [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] :: Datos
y = ["1","2","3","4","5","6","7"]

testsSepararDatos = test [
            separarDatos xs y 3 2 ~?= ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]], [[3.0,3.0],[4.0,4.0]], ["1","2","5","6"], ["3","4"]),
            separarDatos xs y 1 1 ~?= ([], xs , [], y),
            separarDatos xs y (length xs) 1 ~?= (tail xs, [head xs], tail y, [head y])
            ]

testsAccuracy = test [
            accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6
            ]

--testsNFoldCrossValidation = test [
--            ]
