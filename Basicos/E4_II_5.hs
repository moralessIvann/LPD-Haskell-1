{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 20-03-21
    
    Descripcion: *** Raices por metodo de biseccion ***
        El método de bisección para calcular un cero de una función en el intervalo [a, b] se basa en el teorema de Bolzano:
          - Si f(x) es una función continua en el intervalo [a, b], y si, además, en los extremos del intervalo la función f(x)
            toma valores de signo opuesto (f(a) * f(b) < 0), entonces existe al menos un valor c en (a, b) para el que f(c) = 0.
        El método para calcular un cero de la función f en el intervalo [a, b] con un error menor que e consiste en tomar el 
        punto medio del intervalo c = (a + b)/2 y considerar los siguientes casos:
          - Si |f(c)| < e, hemos encontrado una aproximación del punto que anula f en el intervalo con un error aceptable.
          - Si f(c) tiene signo distinto de f(a), repetir el proceso en el intervalo [a, c].
          - Si no, repetir el proceso en el intervalo [c, b].
        
        >>> Ejemplo:
            biseccion (\x -> x^2 - 3) 0 5 0.01
            >> 1.7333984375
-}

module E4_II_5 where

biseccion :: (Double -> Double) -> Double -> Double -> Double -> Double
biseccion fx a b e
    | abs (fx c) < e = c
    | (fx a)*(fx c) < 0 = biseccion fx a c e
    | otherwise = biseccion fx c b e
    where c = (a + b) / 2