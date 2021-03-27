{-
    Creador: Julio Cesar Martinez Gambino
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 25-03-21
    
    Descripcion: *** Ordenamiento de vector QuickSort ***
-}

part::Int -> [Int] -> [Int] ->  [Int] -> ([Int],[Int])
part x [] ps gs = (ps,gs)
part x (y:xs) ps gs
    | y < x = part x xs (y:ps) gs
    | otherwise = part x xs ps (y:gs)


qsort :: [Int] ->[Int]  
qsort [] = []
qsort (x:xs) = (qsort ps) ++ (x:(qsort gs))
                where (ps, gs) = part x xs [] []