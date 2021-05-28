
--  Bibliotecas                                             
import Data.List 
import System.IO
 

-- ***  Implementación del juego  *** -- 

profundidadDeBusqueda :: Int
profundidadDeBusqueda = 6
 

--    1|2|3
--    -+-+-
--    4|5|6
--    -+-+-
--    7|8|9

type Posicion = Int 
type Posiciones = [Posicion]
--  posiciones de [1..9]


data Tablero = Tab Posiciones Posiciones 
               deriving Show 


tableroInicial :: Tablero
tableroInicial = Tab [] []
--  Tab [] []
 

turnoDeX :: Tablero -> Bool
turnoDeX (Tab xs os) = 
    length xs == length os
-- verificar turno de X u O
-- turnoDeX (Tab [3] [])   ==  False
-- turnoDeX (Tab [3] [7])  ==  True
-- turnoDeX (Tab [3,4] [7,8])  ==  True
 

pone :: Tablero -> Posicion -> Tablero
pone (Tab xs os) p | turnoDeX (Tab xs os) = Tab (p:xs) os
                   | otherwise            = Tab xs (p:os)
-- tablero obtenido despues de turno de un jugador
-- pone (Tab [3]   [5]) 8  ==  Tab [8,3] [5]
-- pone (Tab [8,3] [5]) 4  ==  Tab [8,3] [5,4]
 

completo :: Tablero -> Bool
completo (Tab xs os) = length xs + length os == 9
-- verificar si el tablero 'X' u 'O' está completo
-- completo (Tab [3,9,5,8,4] [1,7,6,2])  ==  True
-- completo (Tab [3,9,5,8,4] [1,7,6])    ==  False 
 

subconjunto :: Posiciones -> Posiciones -> Bool
subconjunto s1 s2 = all (`elem` s2) s1
-- verifica si 's1' es un subconjunto de 's2'
-- subconjunto [3,2,5] [6,2,4,5,3]  ==  True
-- subconjunto [3,2,5] [6,2,4,3]    ==  False
 

tieneLinea :: Posiciones -> Bool
tieneLinea ps = 
    subconjunto [1,2,3] ps ||subconjunto [4,5,6] ps ||subconjunto [7,8,9] ps ||
    subconjunto [1,4,7] ps ||subconjunto [2,5,8] ps ||subconjunto [3,6,9] ps ||
    subconjunto [1,5,9] ps ||subconjunto [3,5,7] ps
-- verifica si la lista de posiciones 'ps' contiene una línea horizontal, vertical o diagonal
-- tieneLinea [2,5,3,6,4]  ==  True
-- tieneLinea [2,5,3,6]  ==  False


tieneGanador :: Tablero -> Bool
tieneGanador (Tab xs os) = tieneLinea xs || tieneLinea os
-- verifica si el tablero 't' tiene un ganador (si hay linea en tablero)
-- tieneGanador (Tab [2,5,3,6] [1,7,8,9])  ==  True
-- tieneGanador (Tab [2,5,3,9] [1,6,8,7])  ==  False


--  *** Construcción del árbol de juego   (algoritmo 'minimax' para la PC) *** --

data Arbol a = Nodo a [Arbol a]
-- representar los árboles compuestos por nodos con una lista de hijos
 

posicionesLibres :: Tablero -> Posiciones
posicionesLibres (Tab xs os) = [1..9] \\ (xs++os)
-- lista de las posiciones libres del tablero 'X' u 'O' 
-- posicionesLibres (Tab [3,2] [1,7])  ==  [4,5,6,8,9]

siguientesTableros :: Tablero -> [Tablero]
siguientesTableros t 
    | tieneGanador t = [] 
    | otherwise      = map (pone t) (posicionesLibres t)
-- lista de tableros obtenidos cada vez que se agrega 'X' u 'O' 
 

construyeArbol :: Tablero -> Arbol Tablero
construyeArbol t = 
    Nodo t (map construyeArbol (siguientesTableros t)) 
-- se construye el arbol correspondiente al tablero t
-- Tab [7,1,6,2] [5,4,3]


type Valor = Int
-- representar el valor de los tableros
 

valores :: [Arbol (Valor,Tablero)] -> [Valor]
valores vts = [v | Nodo (v,_) _ <- vts]
 
 
maximiza :: Arbol Tablero -> Arbol (Valor,Tablero)
maximiza (Nodo t []) | tieneGanador t = Nodo (-1,t) []
                     | otherwise      = Nodo (0,t) []                                        
maximiza (Nodo t ts) = Nodo (maximum (valores vts),t) vts
    where vts = map minimiza ts
 
 
minimiza :: Arbol Tablero -> Arbol (Valor,Tablero)
minimiza (Nodo t []) | tieneGanador t = Nodo (1,t) []
                     | otherwise      = Nodo (0,t) []
minimiza (Nodo t ts) = Nodo (minimum (valores vts),t) vts
    where vts = map maximiza ts
 
 
poda :: Int -> Arbol a -> Arbol a
poda n (Nodo x as) | n == 0    = Nodo x []
                   | otherwise = Nodo x (map (poda (n-1)) as)
-- eliminar nodos del arbol (reducir la profundidad)
 

selecciona :: Arbol (Valor,Tablero) -> Tablero
selecciona (Nodo (v,_) ts) = 
    head [t | Nodo (v',t) _ <- ts, v'==v]
-- seleccionar el tableros con la mejor valoracion
 
mejorMovimiento :: Tablero -> Tablero
mejorMovimiento = 
    selecciona . maximiza . poda profundidadDeBusqueda . construyeArbol
-- es el tablero correspondiente al mejor movimiento


-- *** Dibujo del tablero *** --
muestraPosicion :: Tablero -> Posicion -> String
muestraPosicion (Tab xs os) p 
    | p `elem` xs = "X"
    | p `elem` os = "O"
    | otherwise   = show p

 
muestraLinea :: Tablero -> [Posicion] -> String
muestraLinea t = 
    concat . intersperse "|" . map (muestraPosicion t)
 
 
muestraTablero :: Tablero -> String
muestraTablero t = 
    muestraLinea t [1..3] ++ "\n-+-+-\n" ++
    muestraLinea t [4..6] ++ "\n-+-+-\n" ++
    muestraLinea t [7..9]
--  X|X|O
--  -+-+-
--  O|5|X
--  -+-+-
--  X|O|9


-- *** Control del juego *** --
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering           
  putStrLn "Tic Tac Toe"                    
  putStrLn (muestraTablero tableroInicial)   
  putStr "Comienza el juego? (s/n) "         
  l <- getLine                               
  if head l `elem` "sS"                      
     then humano tableroInicial              
     else computadora tableroInicial         

 
humano :: Tablero -> IO ()
humano t = do 
  putStr "\nIndica el lugar donde colocar la ficha: " 
  l <- getLine                                         
  let t' = pone t (read l :: Posicion)                
  putStrLn (muestraTablero t')                        
  if tieneGanador t'                                  
     then putStrLn "Has ganado."                      
     else if completo t'                              
             then putStrLn "Empate."                  
             else computadora t'                      
 
 
computadora :: Tablero -> IO ()
computadora t = do
  putStrLn "\nMi jugada:"            
  let t' = mejorMovimiento t         
  putStrLn (muestraTablero t')       
  if tieneGanador t'                 
     then putStrLn "He ganado."      
     else if completo t'             
             then putStrLn "Empate." 
             else humano t'          