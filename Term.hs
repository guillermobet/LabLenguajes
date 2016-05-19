{-# LANGUAGE FlexibleInstances #-}

import Theorems

instance Show Term where show = showTerm

showTerm :: Term -> String

showTerm (Var p) = [p]

showTerm T = "true"

showTerm F = "false"

showTerm (Not (Var p)) = "¬" ++ showTerm(Var p)
showTerm (Not p) = "¬ (" ++ showTerm(p) ++ ")"

showTerm (Or (Var p) (Var q)) = showTerm(Var p) ++ " \\/ " ++ showTerm(Var q)
showTerm (Or (Var p) q) = showTerm(Var p) ++ " \\/ (" ++ showTerm q ++ ")"
showTerm (Or p (Var q)) = "(" ++ showTerm p ++ ") \\/ " ++ showTerm(Var q)
showTerm (Or p q) = "(" ++ showTerm p ++ ") \\/ (" ++ showTerm q ++ ")"

showTerm (And (Var p) (Var q)) = showTerm(Var p) ++ " /\\ " ++ showTerm(Var q)
showTerm (And (Var p) q) = showTerm(Var p) ++ " /\\ (" ++ showTerm q ++ ")"
showTerm (And p (Var q)) = "(" ++ showTerm p ++ ") /\\ " ++ showTerm(Var q)
showTerm (And p q) = "(" ++ showTerm p ++ ") /\\ (" ++ showTerm q ++ ")"

showTerm (Impl (Var p) (Var q)) = showTerm(Var p) ++ " ==> " ++ showTerm(Var q)
showTerm (Impl (Var p) q) = showTerm(Var p) ++ " ==> (" ++ showTerm q ++ ")"
showTerm (Impl p (Var q)) = "(" ++ showTerm p ++ ") ==> " ++ showTerm(Var q)
showTerm (Impl p q) = "(" ++ showTerm p ++ ") ==> (" ++ showTerm q ++ ")"

showTerm (Equiv (Var p) (Var q)) = showTerm(Var p) ++ " <==> " ++ showTerm(Var q)
showTerm (Equiv (Var p) q) = showTerm(Var p) ++ " <==> (" ++ showTerm q ++ ")"
showTerm (Equiv p (Var q)) = "(" ++ showTerm p ++ ") <==> " ++ showTerm(Var q)
showTerm (Equiv p q) = "(" ++ showTerm p ++ ") <==> (" ++ showTerm q ++ ")"

showTerm (Nequiv (Var p) (Var q)) = showTerm(Var p) ++ " !<==> " ++ showTerm(Var q)
showTerm (Nequiv (Var p) q) = showTerm(Var p) ++ " !<==> (" ++ showTerm q ++ ")"
showTerm (Nequiv p (Var q)) = "(" ++ showTerm p ++ ") !<==> " ++ showTerm(Var q)
showTerm (Nequiv p q) = "(" ++ showTerm p ++ ") !<==> (" ++ showTerm q ++ ")"

-----------------------------------------------------------

class Sust s where
	sust :: s -> Term -> Term
	sust s (Or x y) = Or (sust s x) (sust s y)
	sust s (And x y) = And (sust s x) (sust s y)
	sust s (Impl x y) = Impl (sust s x) (sust s y)
	sust s (Equiv x y) = Equiv (sust s x) (sust s y)
	sust s (Nequiv x y) = Nequiv (sust s x) (sust s y)

(=:) :: Term -> Term -> (Term, Term)
(=:) a b = (a,b)

instance Sust (Term, Term) where
	sust (a, Var b) (Var c)
							| b == c = a
							| otherwise = (Var c)
	sust (a, Var b) (And c d) = And (sust (a, Var b) c) (sust (a, Var b) d)
	sust (a, Var b) (Or c d) = Or (sust (a, Var b) c) (sust (a, Var b) d)
	sust (a, Var b) (Not c) = Not (sust (a, Var b) c)
	sust (a, Var b) (Impl c d) = Impl (sust (a, Var b) c) (sust (a, Var b) d)
	sust (a, Var b) (Equiv c d) = Equiv (sust (a, Var b) c) (sust (a, Var b) d)
	sust (a, Var b) (Nequiv c d) = Nequiv (sust (a, Var b) c) (sust (a, Var b) d)
	sust (a, Var b) T = T
	sust (a, Var b) F = F

instance Sust (Term, (Term, Term), Term) where
	sust (a, (b, Var c), Var d) (Var e)
							| c == e = a
							| d == e = b
							| otherwise = (Var e)
	sust (a, (b, Var c), Var d) (And e f) = And (sust (a, (b, Var c), Var d) e) (sust (a, (b, Var c), Var d) f)
	sust (a, (b, Var c), Var d) (Or e f) = Or (sust (a, (b, Var c), Var d) e) (sust (a, (b, Var c), Var d) f)
	sust (a, (b, Var c), Var d) (Not e) = Not (sust (a, (b, Var c), Var d) e)
	sust (a, (b, Var c), Var d) (Impl e f) = Impl (sust (a, (b, Var c), Var d) e) (sust (a, (b, Var c), Var d) f)
	sust (a, (b, Var c), Var d) (Equiv e f) = Equiv (sust (a, (b, Var c), Var d) e) (sust (a, (b, Var c), Var d) f)
	sust (a, (b, Var c), Var d) (Nequiv e f) = Nequiv (sust (a, (b, Var c), Var d) e) (sust (a, (b, Var c), Var d) f)
	sust (a, (b, Var c), Var d) T = T
	sust (a, (b, Var c), Var d) F = F

instance Sust (Term, Term, (Term, Term), Term, Term) where
	sust (a, b, (c, Var d), Var e, Var f) (Var g) 
							| d == g = a
							| e == g = b
							| f == g = c
							| otherwise = (Var g)
	sust (a, b, (c, Var d), Var e, Var f) (And g h) = And (sust (a, b, (c, Var d), Var e, Var f) g) (sust (a, b, (c, Var d), Var e, Var f) h)
	sust (a, b, (c, Var d), Var e, Var f) (Or g h) = Or (sust (a, b, (c, Var d), Var e, Var f) g) (sust (a, b, (c, Var d), Var e, Var f) h)
	sust (a, b, (c, Var d), Var e, Var f) (Not g) = Not (sust (a, b, (c, Var d), Var e, Var f) g)
	sust (a, b, (c, Var d), Var e, Var f) (Impl g h) = Impl (sust (a, b, (c, Var d), Var e, Var f) g) (sust (a, b, (c, Var d), Var e, Var f) h)
	sust (a, b, (c, Var d), Var e, Var f) (Equiv g h) = Equiv (sust (a, b, (c, Var d), Var e, Var f) g) (sust (a, b, (c, Var d), Var e, Var f) h)
	sust (a, b, (c, Var d), Var e, Var f) (Nequiv g h) = Nequiv (sust (a, b, (c, Var d), Var e, Var f) g) (sust (a, b, (c, Var d), Var e, Var f) h)
	sust (a, b, (c, Var d), Var e, Var f) T = T
	sust (a, b, (c, Var d), Var e, Var f) F = F

instantiate :: Sust s => Equation -> s -> Equation
instantiate (Equa expL expR) s = (Equa (sust s expL) (sust s expR))

instance Show Equation where show = showEquation

showEquation :: Equation -> String
showEquation (Equa expL expR) = "\t" ++ (showTerm expL) ++ "\n === \n\t" ++ (showTerm expR)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equa t1 t2) expr (Var z) = Equa (sust (t1, Var z) expr) (sust (t2, Var z) expr)

infer :: Sust s => Float -> s -> Term -> Term -> Equation
infer n s (Var z) expr = leibniz (instantiate (prop n) s) expr (Var z)