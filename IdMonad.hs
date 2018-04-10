module IdMonad where
-- Identity Monad
type M a = a
unitM a = a
bindM a k = k a