{-
    Creador: Iván Morales 
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 12-03-21
    
    Descripcion: *** Factorial de un número ***
        Escriba un programa en computadora para evaluar el factorial de 
        un numero recursivamente.
-}

module E4_I_4 where

    factorial :: Int -> Int 
    factorial 0 = 1
    factorial n = n * factorial(n-1)