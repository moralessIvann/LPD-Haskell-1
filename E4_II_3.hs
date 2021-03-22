{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 21-03-21
    
    Descripcion: *** Cajero Automaico ***
        Un cajero automático dispone de billetes de 500, 200, 100, 50 y 20. Solo necesita introducir una
        cantidad y el programa le indica cuantos billetes y de que denominacion obtendra, al igual que el
        cambio o restante en su transaccion.
        >>> Ejemplo:
            atm 1280
            >> ->  3 de $500
            >> ->  1 de $200
            >> ->  1 de $100
            >> ->  1 de $50
            >> ->  1 de $20
            >> ->  Cambio = 10
-}

module E4_II_3 where

atm :: (Show a, Integral a) => a -> IO ()
atm x = atmi x (filter (<=x) [20,50,100,200,500])
  where
    atmi a v = do
      if value >= 1 then putStrLn ( "->  " ++ (show value) ++ " de $" ++ (show (last v))) else putStr ""
      if (length v)-1 > 0 then atmi nextValue (init v) 
      else do
        putStrLn ("->  Cambio = " ++ show nextValue)
        return ()
      where
        value = div a (last v)
        nextValue = (a - (value * (last v)))