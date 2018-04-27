module Main where
import Term (Name, Term(..), pos0)
import Numeric
import ErrorMonad (E, unitE, bindE, errorE, E(Success), E(Error),
                   P, unitP, bindP, errorP, resetP)

main :: IO ()
main = do
        putStrLn $ "Output is: " ++ test term0
        putStrLn $ "Output is: " ++ test term1




data Value =  Wrong
                       |  Num Int
                       |  Fun (Value -> P Value)

type  Environment =  [(Name, Value)]


showval :: Value -> String
showval Wrong =  "<wrong>"
showval (Num i) =  show i
showval (Fun f) =  "<function>"


-- Show for PMonad
showE (Success a) = "Success: " ++ showval a
showE (Error s) = "Error: " ++ s

showP m = showE (m pos0)


interp :: Term -> Environment -> P Value
interp (Var x) e =  llookup x e
interp (Con i) e =  unitP (Num i)
interp (Add u v) e =  interp u e `bindP` (\a ->
                     interp v e `bindP` (\b ->
                     add a b))
interp (Lam x  v) e = unitP (Fun (\a -> interp v ((x, a):e)))
interp (App t u) e = interp t e `bindP` (\f ->
                                    interp u e `bindP` (\a ->
                                    apply f a))

-- For at terms
interp (At p t ) e = resetP p (interp t e)


llookup :: Name -> Environment -> P Value
llookup x [] =  errorP ("unbounded variable" ++ x)
llookup x ((y, b):e) =  if  x==y  then  unitP b  else  llookup x e


add :: Value -> Value -> P Value
add (Num i) (Num j) =  unitP (Num (i + j))
add a b =  errorP ("should be numbers: " ++ showval a
                                       ++ "," ++ showval b)

apply :: Value -> Value -> P Value
apply (Fun k) a = k a
apply f a = errorP ("should be function: " ++ showval f)

test :: Term -> String
test t =  showP (interp t [])


term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
                   (Add (Con 10) (Con 11)))

term1 = (App (Con 1) (Con 2))