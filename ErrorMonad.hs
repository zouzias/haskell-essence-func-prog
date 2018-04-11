module ErrorMonad where

data M a = Success a | Error String
unitM a = Success a
errorM s = Error s

bindM (Success a) k = k a
bindM (Error s) k = Error s
