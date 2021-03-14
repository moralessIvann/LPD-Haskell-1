{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 14-03-21
    
    Descripcion: *** Fibonacci para n terminos ***
        Determina los numeros de la secuencias fibonacci hasta el termino N
        >>> Ejemplo:
            fibonacci 15
            >> [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
-}

module E4_I_5 where

fibonacci :: (Num a, Enum a, Ord a) => a -> [a]
fibonacci x = do
    let fibo' a = if a < 2 then a else fibo' (a-1) + fibo' (a-2)
    [ fibo' a | a <- [1..x]]