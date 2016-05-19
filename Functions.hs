{-# LANGUAGE FlexibleInstances #-}

import Equation
import Sust
import Term
import Theorems

instantiate :: Sust s => Equation -> s -> Equation
instantiate (Equa expL expR) s = (Equa (sust s expL) (sust s expR))

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equa t1 t2) expr (Var z) = Equa (sust (t1, Var z) expr) (sust (t2, Var z) expr)

infer :: Sust s => Float -> s -> Term -> Term -> Equation
infer n s (Var z) expr = leibniz (instantiate (prop n) s) expr (Var z)