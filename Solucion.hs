--Teoria de la computacion - Semestre 2 - Año 2025 
--Tarea Final - Mauricio Quintero(167070) y Oliver Sosa(224254)

module Solucion where

-- 1.1 Representaci´on de dominios y soluciones

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
verifyA (lc, s) = foldr (\c->) True lc

verifyB :: (DomB, SolB) -> Bool
verifyB (d, s) = undefined