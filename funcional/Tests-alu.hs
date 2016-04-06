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
            split ',' ",PLP," ~?= ["PLP"],
            split ',' " ,PLP, " ~?= [" ","PLP"," "],
            split ',' "hola PLP, bienvenidos!" ~?= ["hola PLP"," bienvenidos!"]
            ]

testsLongitudPromedioPalabras = test [
            longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4
            ]

testsCuentas = test [
            cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")]
            ]

testsRepeticionesPromedio = test [
            repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5
            ]

testsFrecuenciaTokens = test [
            (head frecuenciaTokens) "use_snake_case !" ~?= 0.125
            ]

ts = ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"]

--testsNormalizarExtractor = test [
--            ]

testsExtraerFeatures = test [
            extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ts ~?= [[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]]
            ]

testsDistEuclideana = test [
            distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464
            ]

testsDistCoseno = test [
            distCoseno [0,3,4] [0,-3,-4]  ~?= -1
            ]

testsKnn = test [
            (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "f"
            ]

xs = [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] :: Datos
y = ["1","2","3","4","5","6","7"]

testsSepararDatos = test [
            separarDatos xs y 3 2 ~?= ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]], [[3.0,3.0],[4.0,4.0]], ["1","2","5","6"], ["3","4"])
            ]

testsAccuracy = test [
            accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6
            ]

--testsNFoldCrossValidation = test [
--            ]