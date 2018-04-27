module Main where
import Numeric
import ErrorMonad (E, unitE, bindE, errorE, E(Success), E(Error))

main :: IO ()
main = do
        putStrLn $ "Output is: " ++ test term0
        putStrLn $ "Output is: " ++ test term1


type  Name = String

data  Term =  Var Name
                       |  Con Int
                       |  Add Term Term
                       |  Lam Name Term
                       |  App Term Term

data Value =  Wrong
                       |  Num Int
                       |  Fun (Value -> E Value)

type  Environment =  [(Name, Value)]


showval :: Value -> String
showval Wrong =  "<wrong>"
showval (Num i) =  show i
showval (Fun f) =  "<function>"


-- Show for ErrorMonad
showE (Success a) = "Success: " ++ showval a
showE (Error s) = "Error: " ++ s

interp :: Term -> Environment -> E Value
interp (Var x) e =  llookup x e
interp (Con i) e =  unitE (Num i)
interp (Add u v) e =  interp u e `bindE` (\a ->
                     interp v e `bindE` (\b ->
                     add a b))
interp (Lam x  v) e = unitE (Fun (\a -> interp v ((x, a):e)))
interp (App t u) e = interp t e `bindE` (\f ->
                                    interp u e `bindE` (\a ->
                                    apply f a))


llookup :: Name -> Environment -> E Value
llookup x [] =  errorE ("unbounded variable" ++ x)
llookup x ((y, b):e) =  if  x==y  then  unitE b  else  llookup x e


add :: Value -> Value -> E Value
add (Num i) (Num j) =  unitE (Num (i + j))
add a b =  errorE ("should be numbers: " ++ showval a
                                       ++ "," ++ showval b)

apply :: Value -> Value -> E Value
apply (Fun k) a = k a
apply f a = errorE ("should be function: " ++ showval f)

test :: Term -> String
test t =  showE (interp t [])


term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
                   (Add (Con 10) (Con 11)))

term1 = (App (Con 1) (Con 2))