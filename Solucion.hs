--Teoria de la computacion - Semestre 2 - Año 2025 
--Tarea Final - Mauricio Quintero(167070) y Oliver Sosa(224254)

module Solucion where

-- 1.1 Representación de dominios y soluciones

-- lista exterior -> conjunciones
-- lista interior -> clausulas
-- tupla -> nombre variable, true indica directo, false que está negada
type DomA = [[(String, Bool)]]
type SolA = [(String, Bool)]

-- Abajo explicaciones
type DomB = (Ps, D, R, E, B, M, V)
type SolB = Ps

-- punto de control (secuencial desde 1)
type P = Int
-- puntos de control
type Ps = [P]
-- matriz distancias.
type D = [(P, P, Int)]
-- distancias a la base
type R = [(P, Int)]
-- pares de exlusión
type E = [(P, P)]
-- prioridades de los puntos
type B = [(P, Int)]
-- límite de distancia
type M = Int
-- prioridad mínima requerida
type V = Int

-- 1.2 Verificadores en tiempo polinomial

verifyA :: (DomA, SolA) -> Bool
verifyA (lc, s) = foldr (\c b -> or (assignI c s) && b) True lc

assignI :: [(String, Bool)] -> SolA -> [Bool]
assignI ll i = map (\(s,b) -> case lookup s i of {
                                    Nothing -> error "La variable no está en la interpretación";
                                    Just x -> x == b  -- representamos en el dominio que una variable acompañado de true significa representación directa
}) ll

-- tests
formSat1 :: DomA
formSat1 = [[("x1", True), ("x2", False), ("x3", True)],
            [("x1", False), ("x2", True), ("x3", True)],
            [("x2", True), ("x3", True)]]

formInsat2 :: DomA
formInsat2 = [[("x1", True)],
              [("x2", True), ("x3", True)],
              [("x1", False)]]

interpretacion1 :: SolA
interpretacion1 = [("x1", True), ("x2", False), ("x3", True)]
-- true
testVerifyA1 :: Bool
testVerifyA1 = verifyA (formSat1, interpretacion1)

-- false
testVerifyA2 :: Bool
testVerifyA2 = verifyA (formInsat2, interpretacion1)

verifyB :: (DomB, SolB) -> Bool
verifyB (d, s) = undefined