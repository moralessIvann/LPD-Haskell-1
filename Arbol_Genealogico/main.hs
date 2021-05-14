import Arbol
import Relaciones
import System.IO

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

-- Hermanos
hermanos name = do
    writeFile "temp/childrens.log" ""
    pq <- padre name
    childrens treeOrigin pq name "temp/childrens.log"
    readFile "temp/childrens.log"