module Term where

data Term =   Var Char
			| T
			| F
			| Not Term
			| Or Term Term
			| And Term Term
			| Impl Term Term
			| Equiv Term Term
			| Nequiv Term Term
			deriving (Eq)

-----------------------------------------------------------

true :: Term; true = T
false :: Term; false = F
a :: Term; a = Var 'a'
b :: Term; b = Var 'b'
c :: Term; c = Var 'c'
d :: Term; d = Var 'd'
e :: Term; e = Var 'e'
f :: Term; f = Var 'f'
g :: Term; g = Var 'g'
h :: Term; h = Var 'h'
i :: Term; i = Var 'i'
j :: Term; j = Var 'j'
k :: Term; k = Var 'k'
l :: Term; l = Var 'l'
m :: Term; m = Var 'm'
n :: Term; n = Var 'n'
o :: Term; o = Var 'o'
p :: Term; p = Var 'p'
q :: Term; q = Var 'q'
r :: Term; r = Var 'r'
s :: Term; s = Var 's'
t :: Term; t = Var 't'
u :: Term; u = Var 'u'
v :: Term; v = Var 'v'
w :: Term; w = Var 'w'
x :: Term; x = Var 'x'
y :: Term; y = Var 'y'
z :: Term; z = Var 'z'

-----------------------------------------------------------

neg :: Term -> Term
neg p = Not p

(\/) :: Term -> Term -> Term
(\/) p q = Or p q

(/\) :: Term -> Term -> Term
(/\) p q = And p q

(==>) :: Term -> Term -> Term
(==>) p q = Impl p q

(<==>) :: Term -> Term -> Term
(<==>) p q = Equiv p q

(!<==>) :: Term -> Term -> Term
(!<==>) p q = Nequiv p q

infixl 3 \/
infixl 3 /\
infixr 2 ==>
infixl 1 <==>
infixl 1 !<==>

-----------------------------------------------------------

-- Arreglar lo de true, false y not!!

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