--Teoria de la computacion - Semestre 2 - Año 2025 
--Tarea Final - Mauricio Quintero(167070) y Oliver Sosa(224254)

module Solucion where
import Data.List

-- 1.1 Representación de dominios y soluciones

-- lista exterior -> conjunciones
-- lista interior -> clausulas
-- tupla -> nombre literal, true indica directo, false que está negado
type DomA = [[(String, Bool)]]
type SolA = [(String, Bool)]

-- Abajo explicaciones
type DomB = (Ps, D, R, E, B, M, V)
type SolB = Ps

-- punto de control (secuencial desde 1)
type P = Int
-- puntos de control
type Ps = [P]
-- matriz distancias. Como tupla de puntos el primer element de la tupla para facilitar lookup
type D = [((P, P), Int)]
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


-- O(d)*O(lp) + O(b) + O(e*lp)
verifyB :: (DomB, SolB) -> Bool
verifyB ((ps, d, r, e, b, m, v), lp) = pathDistance d r lp <= m -- comienzo y final en la base implícito en pathDistance
                                    && sum (map snd b) >= v -- como hay que pasar por todos los puntos, sumo todas las prioridades
                                    && checkPath e lp

-- O(d)*O(lp)
pathDistance :: D -> R -> Ps-> Int
pathDistance d r lp = getDistanceToBase r (head lp) + innerPathDistance d r lp

-- O(d)*O(lp)
innerPathDistance :: D -> R -> Ps-> Int
innerPathDistance d r [p] = getDistanceToBase r p
innerPathDistance d r (p:np:ps) = getDistance d p np + innerPathDistance d r (np:ps)

-- O(d)
getDistance :: D -> P -> P -> Int
getDistance d pa pb =  case lookup (pa, pb) d of {
                          Nothing -> error "No existe alguno de los puntos";
                          Just x -> x
}

-- O(r)
getDistanceToBase :: R -> P -> Int
getDistanceToBase r p = case lookup p r of {
                          Nothing -> error "No existe el punto";
                          Just x -> x
}

-- O(e*lp)
checkPath :: E -> SolB -> Bool
checkPath e [p] = True
checkPath e (p:np:ps) = not (any (\(pa, pb) -> pa == p && pb == np) e) && checkPath e (np:ps) 

-- tests
domB1 :: DomB
domB1 = ([1, 2, 3],
         [((1, 2), 5), ((2, 1), 5), ((1, 3), 7), ((3, 1), 7), ((2, 3), 9), ((3, 2), 9)],
         [(1, 5), (2, 4), (3, 1)],
         [(1, 3)],
         [(1, 1), (2, 2), (3, 3)],
         17,
         4)

domB2 :: DomB
domB2 = ([1, 2, 3],
         [((1, 2), 5), ((2, 1), 5), ((1, 3), 7), ((3, 1), 7), ((2, 3), 9), ((3, 2), 9)],
         [(1, 5), (2, 4), (3, 1)],
         [(1, 3)],
         [(1, 1), (2, 2), (3, 3)],
         2,
         4)

lp1 :: SolB
lp1 = [3, 1, 2]

-- true
testVerifyB1 :: Bool
testVerifyB1 = verifyB (domB1, lp1)

-- false
testVerifyB2 :: Bool
testVerifyB2 = verifyB (domB2, lp1)

-- 1.3 Resolución en tiempo exponencial
solveA :: DomA -> SolA
solveA f = case find (\s -> verifyA (f, s)) (interpretaciones (literales f)) of {
              Nothing -> error "no hay solución";
              Just x -> x
}

literales :: DomA -> [String]
literales = foldr literalesAdd []

literalesAdd :: [(String, Bool)] -> [String] -> [String]
literalesAdd c ls = map fst (filter (\(l,b) -> notElem l ls) c) ++ ls

interpretaciones :: [String] -> [SolA]
interpretaciones [] = [[]]
interpretaciones (l:ls) = [(l, False) : x | x <- interpretaciones ls] ++ [(l, True) : x | x <- interpretaciones ls] 

-- test
testLiterales1 :: [String]
testLiterales1 = literales formSat1

testLiterales2 :: [String]
testLiterales2 = literales formInsat2

testSolveA1 :: SolA
testSolveA1 = solveA formSat1

testSolveA2 :: SolA
testSolveA2 = solveA formInsat2


solveB :: DomB -> SolB
solveB (ps, d, r, e, b, m, v) = case find (\p -> verifyB ((ps, d, r, e, b, m, v), p)) (allPaths ps) of {
                                  Nothing -> error "no hay solución";
                                  Just x -> x
}

-- permutaciones en haskell https://stackoverflow.com/questions/60514699/permutations-in-haskell-involving-list-comprehensions-recursion-and-the-delete
allPaths :: Ps -> [SolB]
allPaths [] = [[]]
allPaths ps = [ i:j | i <- ps, j <- allPaths $ delete i ps ]

-- test
testSolveB1 :: SolB
testSolveB1 = solveB domB1

testSolveB2 :: SolB
testSolveB2 = solveB domB2

-- 1.4 Reducción polinomial entre problemas
reduceAToB :: DomA -> DomB
reduceAToB = undefined
-- La estrategia consiste en pasar mediante reducciones conocidas desde SAT hasta TSP, y la reducción de TSP a B la hacemos nosotros
-- Para demostrar que TSP es reducible a B, tenemos que encontrar una función f que para todo instancia de TSP que decida True, corresponda a una instancia de B que decida True.
-- De la misma forma, f debe corresponder a toda instancia de TSP que decida False, una de B que decida False.
-- El dominio de TSP, es un grafo completo no dirigido G con V vértices, donde cada arista le corresponde un costo, además se proporciona un valor M y el predicado es existe un camino
-- Hamilitonino tal que la suma del costo de las aristas es menor o igual a M. Podemos mapear M al mismo valor M (como límite de distancia) en el problema B. Los vértices V corresponden
-- los puntos de control P, y le sumamos la base como otro vértice del grafo para el problema B, y unimos todos los puntos de control con la base para hacer al grafo completo. El costo
-- de todas estas es aristas es 0, para no afectar los resultados provenientes de TSP. Para las prioridades de cada punto, y el valor mínimo de prioridad tomamos el valor trivial 0.
-- Para las exclusiones de pares tomamos el valor trivial conjunto vacío. De esta forma, podemos observar que cualquier solución del TSP es tambén
-- una solución de B, y que si TSP no tiene solución, B tampoco la tendrá ya que la condición de encontrar un camino que no supere el valor M no se cumple.
-- Es fácil demostrar que la función f es polinómica, porque un grafo se puede representar como una matriz v*v, y el costo de copiarla, tomando la cantidad de vértices V es V². Luego
-- agregar la base y todas las aristas (una por cada vértice), así como definir las prioridades de cada punto en 0 es de costo V, por lo que V² + V es V². Las otras operaciones
-- se realizan en costo 1. Por lo tanto f es polinómica.
-- Al encontrar una función f que permite mapear el TSP a B en tiempo polinomial, conservando los valores de verdad para cada instancia, nos permite afirmar que reducimos el problema TSP
-- a B. Como ya sabemos que TSP es NP-Completo, por reducirse desde SAT (gracias a Cook-Levin), podemos afirmar que el problema B es NP-Completo.
-- La estrategia para llegar desde SAT a TSP es: SAT -> 3-CNF-SAT -> CLIQUE -> VERTEX-COVER -> HC -> TSP.`




