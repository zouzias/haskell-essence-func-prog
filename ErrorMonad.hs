module ErrorMonad where

data E a = Success a | Error String
unitE a = Success a
errorE s = Error s

bindE (Success a) k = k a
bindE (Error s) k = Error s
