{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 12-03-21
    
    Descripcion: *** Números amigos ***
        Dos números amigos son dos números enteros positivos a y b tales que la suma de los 
        divisores propios de uno es igual al otro número y viceversa.
        >>> Ejemplo:
            220 -> 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y 110
            >>> Suman 284
            284 -> 1, 2, 4, 71 y 142
            >>> Suman 220
-}

module E4_II_2 where

numFriends :: Integral a => a -> a -> Bool
numFriends a b
    | a <= 1 || b <= 1 = False
    | otherwise = do
        (sumatoria a == b) && (sumatoria b == a)
            where sumatoria n = sum (init [ x | x <- [1..n], (mod n x) == 0 ])