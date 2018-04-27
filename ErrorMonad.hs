module ErrorMonad where

data E a = Success a | Error String
unitE = Success
errorE = Error

bindE (Success a) k = k a
bindE (Error s) k = Error s
