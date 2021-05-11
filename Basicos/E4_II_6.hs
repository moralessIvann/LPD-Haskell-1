{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 14-03-21
    
    Descripcion: *** Maximo Comun Divisor ***
        Utilizando el algoritmo de Euclideano, utilizamos el residuo de la division
        de dos numeros, cuando el residuo de cero, nuestro MCD sera el ultimo divisor utilizado
        >>> Ejemplo:
            mcd 1032 180
            >> 12
            -- mcd 180 ( mod 1032 180 )
            -- mcd 132 ( mod 180 132 )
            -- mcd 48 ( mod 132 48 )
            -- mcd 36 ( mod 48 36 )
            -- mcd 12 ( mod 36 12 ) -- mod 36 12 = 0
            -- abs(12)
-}

module E4_II_6 where

mcd :: Integral t => t -> t -> t
mcd a b = if b == 0 then abs(a) else mcd b (mod a b)