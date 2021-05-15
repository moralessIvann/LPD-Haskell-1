{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 14-05-21
    
    Descripcion: ***
-}

import Arbol
import Relaciones
import System.IO


------------------------------------------------
abuelos child g g2 = do
    writeFile "temp/abuelos.log" ""
    p <- padres child g
    fathers treeOrigin p "" "" g2 "temp/abuelos.log"
    readFile "temp/abuelos.log"


padres child p = do
    writeFile "temp/padres.log" ""
    fathers treeOrigin child "" "" p "temp/padres.log"
    readFile "temp/padres.log"

------------------------------------------------

-- Pareja
pareja name = do
    writeFile "temp/couple.log" ""
    couple treeOrigin name "temp/couple.log"
    readFile "temp/couple.log"


-- Abuelos
abueloPaterno child = abuelos child M M
abuelaPaterno child = abuelos child M F
abueloMaterno child = abuelos child F M
abuelaMaterno child = abuelos child F F


-- Padres
padre child = padres child M
madre child = padres child F


-- Hijos
hijos name = do
    writeFile "temp/childrens.log" ""
    childrens treeOrigin name "" "temp/childrens.log"
    readFile "temp/childrens.log"

hija name = do
    writeFile "temp/childrens.log" ""
    children treeOrigin name F "temp/childrens.log"
    readFile "temp/childrens.log"

hijo name = do
    writeFile "temp/childrens.log" ""
    children treeOrigin name M "temp/childrens.log"
    readFile "temp/childrens.log"


-- Hermanos
hermanos name = do
    writeFile "temp/childrens.log" ""
    pq <- padre name
    if length(pq)<1 then readFile "temp/childrens.log" 
    else do
        childrens treeOrigin pq name "temp/childrens.log"
        readFile "temp/childrens.log"

hermano name = do
    writeFile "temp/brothers.log" ""
    pq <- padre name
    if length(pq)<1 then readFile "temp/brothers.log"
    else do
        brother treeOrigin pq name M "temp/brothers.log"
        readFile "temp/brothers.log"

hermana name = do
    writeFile "temp/brothers.log" ""
    pq <- padre name
    if length(pq)<1 then readFile "temp/brothers.log"
    else do
        brother treeOrigin pq name F "temp/brothers.log"
        readFile "temp/brothers.log"


-- Suegros
suegro name = do
    cp <- pareja name
    padre cp
suegra name = do
    cp <- pareja name
    madre cp


-- Cuñados (hermanos de pareja)
cunados name = do
    pa <- pareja name
    hermanos pa

cunado name = do
    pa <- pareja name
    hermano pa

cunada name = do
    pa <- pareja name
    hermana pa


-- Nietos
nieto name = do
    ch <- hija name
    writeFile "temp/childrens2.log" ""
    children treeOrigin ch M "temp/childrens2.log"
    readFile "temp/childrens2.log"

nieta name = do
    ch <- hija name
    writeFile "temp/childrens2.log" ""
    children treeOrigin ch F "temp/childrens2.log"
    readFile "temp/childrens2.log"


-- Yerno | Nuera
yerno name = do
    ch <- hija name
    pareja ch

nuera name = do
    ch <- hijo name
    pareja ch 


-- Tio (hermano de papa|mama)
tio name = do
    writeFile "temp/padres2.log" ""
    fathers treeOrigin name "" "" M "temp/padres2.log"
    pa <- readFile "temp/padres2.log"
    hermano pa

tia name = do
    writeFile "temp/padres2.log" ""
    fathers treeOrigin name "" "" M "temp/padres2.log"
    pa <- readFile "temp/padres2.log"
    hermana pa


-- Primos (hij@ de ti@)
primo name = do
    t <- tio name
    hijo t

prima name = do
    t <- tio name
    hija t