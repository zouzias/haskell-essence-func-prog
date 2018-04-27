module Term where

type  Name = String
type Position = Integer
pos0 = 0

data  Term =  Var Name
                       |  Con Int
                       |  Add Term Term
                       |  Lam Name Term
                       |  App Term Term
                       |  At Position Term