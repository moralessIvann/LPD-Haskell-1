module Relaciones where

import Arbol

-- Busqueda generica de padres
fathers [] _ _ _ _ filename = appendFile filename ""
fathers (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson dad mom gender filename
    | namePerson == n = do
        if show(gender) == "F" then 
            appendFile filename mom 
        else appendFile filename dad
    | otherwise = do
        fathers xs namePerson dad mom gender filename
        if show(g) == "F" then
            fathers c namePerson p n gender filename
        else fathers c namePerson n p gender filename


-- Busqueda generica de esposos
couple [] _ filename = appendFile filename ""
couple (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson filename
    | namePerson == n = appendFile filename p
    | namePerson == p = appendFile filename n
    | otherwise = do
        couple xs namePerson filename
        couple c namePerson filename


-- Busqueda de hijos
childrens [] _ _ filename = appendFile filename ""
childrens (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson child filename
    | namePerson == n || namePerson == p = do
        let x = [if not(n==child) then n ++ "," else "" | People{gender=g, name=n, partner=p, genPartner=gp, childs=c} <- c]
        appendFile filename (init $ unwords x)
    | otherwise = do
        childrens xs namePerson child filename
        childrens c namePerson child filename

------------------------------------------------------------------------------------------

abuelos child g g2 = do
    writeFile "temp/abuelos.log" ""
    p <- padres child g
    fathers treeOrigin p "" "" g2 "temp/abuelos.log"
    readFile "temp/abuelos.log"


padres child p = do
    writeFile "temp/padres.log" ""
    fathers treeOrigin child "" "" p "temp/padres.log"
    readFile "temp/padres.log"
