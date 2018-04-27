module ErrorMonad where
import Term(Position)

data E a = Success a | Error String
unitE a = Success a
errorE s = Error s

bindE (Success a) k = k a
bindE (Error s) k = Error s

-- For variation two (additions)
type P a = Position -> E a

unitP a = \p -> unitE a
errorP s = \p -> errorE (show p ++ " : " ++ s)

m `bindP`  k = \p -> m p `bindE` (\x -> k x p)

-- To change the position
resetP :: Position -> P x -> P x
resetP q m = \p -> m q
