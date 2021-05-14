module Arbol where

data Gender = F | M deriving Show
data Tree = Empty | People { 
    gender :: Gender, 
    name :: String,
    partner :: String,
    genPartner :: Gender,
    childs :: [Tree]
} deriving Show

treeOrigin = [People {
    gender = M,
    name = "Arthur",
    partner = "Molly Prewett",
    genPartner = F,
    childs = [
        People {
            gender = M,
            name = "Ron",
            partner = "Hermione",
            genPartner = F,
            childs = [
                People {
                    gender = M,
                    name = "Hugo",
                    partner = "",
                    genPartner = F,
                    childs = []
                },
                People {
                    gender = F,
                    name = "Rose",
                    partner = "",
                    genPartner = M,
                    childs = []
                }
            ]
        },
        People {
            gender = M,
            name = "George",
            partner = "Angelina Johnson",
            genPartner = F,
            childs = [
                People {
                    gender = M,
                    name = "Fred",
                    partner = "",
                    genPartner = F,
                    childs = []
                },
                People {
                    gender = F,
                    name = "Roxanne",
                    partner = "",
                    genPartner = M,
                    childs = []
                }
            ]
        },
        People {
            gender = M,
            name = "Bill",
            partner = "Fleur Delacour",
            genPartner = F,
            childs = [
                People {
                    gender = F,
                    name = "Victoire",
                    partner = "",
                    genPartner = M,
                    childs = [
                        People {
                            gender = M,
                            name = "Teddy Lupin",
                            partner = "",
                            genPartner = F,
                            childs = []
                        }
                    ]
                },
                People {
                    gender = F,
                    name = "Dominique",
                    partner = "",
                    genPartner = M,
                    childs = []
                },
                People {
                    gender = M,
                    name = "Louis",
                    partner = "",
                    genPartner = F,
                    childs = []
                }
            ]
        },
        People {
            gender = M,
            name = "Percy",
            partner = "Andrey",
            genPartner = F,
            childs = [
                People {
                    gender = F,
                    name = "Lucy",
                    partner = "",
                    genPartner = M,
                    childs = []
                },
                People {
                    gender = F,
                    name = "Molly",
                    partner = "",
                    genPartner = M,
                    childs = []
                }
            ]
        },
        People {
            gender = F,
            name = "Ginny",
            partner = "Harry Potter",
            genPartner = M,
            childs = [
                People {
                    gender = F,
                    name = "Lily Luna",
                    partner = "",
                    genPartner = M,
                    childs = []
                },
                People {
                    gender = M,
                    name = "James Sirius",
                    partner = "",
                    genPartner = F,
                    childs = []
                },
                People {
                    gender = M,
                    name = "Albus Severus",
                    partner = "",
                    genPartner = F,
                    childs = []
                }
            ]
        }
    ]
}]