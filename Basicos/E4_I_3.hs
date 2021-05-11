{-
    Creador: Julio Cesar Martinez Gambino
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 14-03-21
    
    Descripcion: *** Suma recursiva ***
        Sumar recursivamente los elementos de un vector de tamaño n
-}

module E4_I_3 where

suma :: [Int] -> Int  
suma [] = 0
suma (x:xs) = x + suma xs