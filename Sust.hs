{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Sust where

import Term

class Show s => Sust s where
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

-----------------------------------------------------------

instance Show (Term, Term) where show (a, b) = show a ++ " =: " ++ show b

instance Show (Term, (Term, Term), Term) where show (a, x, b) = show a ++ ", " ++ show x ++ ", " ++ show b

instance Show (Term, Term, (Term, Term), Term, Term) where show (a, b, x, c, d) = show a ++ ", " ++ show (b, x, c) ++ ", " ++ show d