{-
    Creador: Abraham Medina Carrillo
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 14-05-21
    
    Descripcion: ***
-}

module Relaciones where

import Arbol

-- Busqueda generica de padres
fathers [] _ _ _ _ filename = appendFile filename ""
fathers (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson dad mom gender filename
    | namePerson == n = do
        if gender == F then 
            appendFile filename mom 
        else appendFile filename dad
    | otherwise = do
        fathers xs namePerson dad mom gender filename
        if g == F then
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


children [] _ _ filename = appendFile filename ""
children (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson gen filename
    | namePerson == n || namePerson == p = do
        let x = [if show(gen)==show(ga) then na else "" | People{gender=ga, name=na, partner=_, genPartner=_, childs=_} <- c]
        let x2 = filter (\x -> length x > 0) (x)
        if length(x2) > 0 then appendFile filename (head x2) else appendFile filename ""
    | otherwise = do
        children xs namePerson gen filename
        children c namePerson gen filename


-- Busqueda de hijos
writeBrothers cx filename ch gen = do
    let x = [if (not(na==ch) && gen==ga) then na else "" | People{gender=ga, name=na, partner=p, genPartner=gp, childs=c} <- cx]
    let x2 = filter (\x -> length x > 0) (x)
    if length(x2) > 0 then appendFile filename (head x2) else appendFile filename ""

--
brother [] _ _ _ filename = appendFile filename ""
brother (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson child gen filename
    | namePerson == n || namePerson == p = writeBrothers c filename child gen
    | otherwise = do
        brother xs namePerson child gen filename
        brother c namePerson child gen filename