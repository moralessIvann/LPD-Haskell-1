{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 12-03-21
    
    Descripcion: *** Sumador de Euros ***
        Sumar monedas tal que es la suma de los euros correspondientes:
        - a monedas de 1 euro
        - b de 2 euros
        - c de 5 euros
        - d de 10 euros
        - e de 20 euros
        >>> Ejemplo:
            sumEuro [3, 0, 4, 0, 2]
            >> 63
            sumEuro [3]
            >> 3
-}

module E4_II_1 where

sumEuro :: Num a => [a] -> a
sumEuro xs = sum (zipWith (*) xs [1, 2, 5, 10, 20])