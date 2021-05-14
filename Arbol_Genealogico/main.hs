import Arbol

-- padres :: [Tree] -> String -> String -> String -> Gender -> String
padres [] _ _ v _ = putStr ""
padres (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson dad mom gender = do
    if show(g) == "F" then padres c namePerson p n gender else padres c namePerson n p gender

    if namePerson == n then do
        if show(gender) == "F" then putStr mom else putStr dad
    else padres xs namePerson dad mom gender


papa child = do
    putStr(">> El papa de " ++ child ++ " es ")
    padres treeOrigin child "" "" M
    putStrLn ""

mama child = do
    putStr(">> La mama de " ++ child ++ " es ")
    -- padres treeOrigin child "" "" F
    putStrLn ""


-- Aun no funciona
-- hermanos [] _ x = putStr ""
-- hermanos (People{gender=g, name=n, partner=p, genPartner=gp, childs=c}:xs) namePerson parent = do
--     hermanos c namePerson n
--     if namePerson == n then do
--         putStrLn (show xs ++ " " ++ parent)
--     else hermanos xs namePerson parent