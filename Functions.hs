--{-# LANGUAGE DataKinds #-}

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

step :: Sust s => Term -> Float -> s -> Term -> Term -> Term
step t n s (Var z) expr
						| t == leftTerm = rightTerm--(infer n s (Var z) expr) = secondTerm (infer n s (Var z) expr)
						| t == rightTerm = leftTerm--(infer n s (Var z) expr) = firstTerm (infer n s (Var z) expr)
						| otherwise = error "Couldn't match expression E depending on variable z"
						where (Equa leftTerm rightTerm) = infer n s (Var z) expr

with :: String
with = "with"

using :: String
using = "using"

lambda :: String
lambda = "lambda"

--statement :: String -> Float -> String -> Sust -> String -> String -> Term -> Term -> String
--statement