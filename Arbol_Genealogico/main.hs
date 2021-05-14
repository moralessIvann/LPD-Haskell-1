import Arbol
import System.IO

-- Busqueda generica de padres
fathers (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson dad mom gender filename
    | namePerson == n = do
        if show(gender) == "F" then 
            appendFile filename mom 
        else appendFile filename dad
    | otherwise = do
        if not(show(xs) == "[]") then 
            fathers xs namePerson dad mom gender filename 
        else appendFile filename ""

        if not(show(c) == "[]") then 
            if show(g) == "F" then
                fathers c namePerson p n gender filename
            else fathers c namePerson n p gender filename
        else appendFile filename ""


-- Busqueda generica de esposos
couple (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson filename
    | namePerson == n = appendFile filename p
    | otherwise = do
        if not(show(xs) == "[]") then 
            couple xs namePerson filename 
        else appendFile filename ""

        if not(show(c) == "[]") then 
            couple c namePerson filename
        else appendFile filename ""

-- Busqueda de hijos
childrens (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson child filename
    | namePerson == n || namePerson == p = do
        let x = [if not(n==child) then n ++ "," else "" | People{gender=g, name=n, partner=p, genPartner=gp, childs=c} <- c]
        appendFile filename (init $ unwords x)
    | otherwise = do
        if not(show(xs) == "[]") then 
            childrens xs namePerson child filename 
        else appendFile filename ""

        if not(show(c) == "[]") then 
            childrens c namePerson child filename
        else appendFile filename ""


abuelos child g g2 = do
    writeFile "temp/abuelos.log" ""
    p <- padres child g
    fathers treeOrigin p "" "" g2 "temp/abuelos.log"
    readFile "temp/abuelos.log"


padres child p = do
    writeFile "temp/padres.log" ""
    fathers treeOrigin child "" "" p "temp/padres.log"
    readFile "temp/padres.log"


---------------------------------------------------------------------

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