{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 12-03-21
    
    Descripcion: *** Aprocimacion de arcsin ***
        Mediante la seria de Maclaurin realizaremos una aproximacion de arcsin(0.57)
-}

module E4_II_4 where
    
arcsin :: (Floating p, Ord p) => p -> p
arcsin 0 = 0
arcsin x = do
    let max_iteration = 30

    let factorial' n = do
        if n == 0 then 1
        else n * factorial'(n-1)
    
    let ntermino n = (x**((2*n)+1)) * ((factorial' (2*n)) / ((4**n) * ((factorial' n)**2) * ((2*n)+1)))
    --
    -- opcion utilizando listas
    --sum (map (\n -> ntermino n) [0..max_iteration])
    --
    -- opcion utilizando recursividad
    let sumatoria' n = do
        if n < 0 then 0
        else (ntermino n) + sumatoria'(n-1)
    sumatoria' max_iteration