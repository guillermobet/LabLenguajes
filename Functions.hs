module Functions where

import Sust
import Term
import Theorems

data Void = With | Using | Lambda

with :: Void
with = With

using :: Void
using = Using

lambda :: Void
lambda = Lambda

-----------------------------------------------------------

instantiate :: Sust s => Equation -> s -> Equation
instantiate (Equa expL expR) s = (Equa (sust s expL) (sust s expR))

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equa t1 t2) expr (Var z) = Equa (sust (t1, Var z) expr) (sust (t2, Var z) expr)

infer :: Sust s => Float -> s -> Term -> Term -> Equation
infer n s (Var z) expr = leibniz (instantiate (prop n) s) expr (Var z)

step :: Sust s => Term -> Float -> s -> Term -> Term -> Term
step t n s (Var z) expr
						| t == leftTerm = rightTerm
						| t == rightTerm = leftTerm
						| otherwise = error "Invalid inference rule"
						where (Equa leftTerm rightTerm) = infer n s (Var z) expr

statement :: Sust s => Float -> Void -> s -> Void -> Void -> Term -> Term -> Term -> IO Term
statement n _ s _ _ (Var z) expr ioTerm = let x = (step ioTerm n s (Var z) expr) in
											do
												putStrLn $ " ===\t\t\t\t<statement " ++ show n ++ " with (" ++ show s ++ ") using lambda " ++ [z] ++ ".(" ++ showTerm expr ++ ")>"
												putStrLn $ "\t" ++ show x
												return x

proof :: Equation -> IO Term
proof (Equa exprL exprR) = do
								putStrLn $ "\nprooving " ++ (showTerm exprL) ++ " === " ++ (showTerm exprR) ++ "\n\n\t" ++ show exprL
								return exprL

done :: Equation -> Term -> IO()
done (Equa exprL exprR) ioTerm
								| ioTerm == exprR = do putStrLn $ "\nproof successful\n"
								| otherwise = do putStrLn $ "\nproof unsuccessful\n"