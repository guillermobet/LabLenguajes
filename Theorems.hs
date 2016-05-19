module Theorems where

{-
	Tipo de dato Term, que especifica los diferentes valores válidos
	en las expresiones lógicas mediante sus constructores

	Tipo de dato Equation, que está formado por dos términos lógicos
-}

data Term =   Var Char
			| T
			| F
			| Not Term
			| Or Term Term
			| And Term Term
			| Impl Term Term
			| Equiv Term Term
			| Nequiv Term Term
			--deriving (Eq)

data Equation = Equa Term Term

-----------------------------------------------------------

{-	Funciones constantes de términos válidos en expresiones lógicas, 
	retornan un elemento de tipo Term
-}

true :: Term; true = T
false :: Term; false = F
a :: Term; a = Var 'a'
b :: Term; b = Var 'b'
c :: Term; c = Var 'c'
d :: Term; d = Var 'd
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

{-
	Definición de las operaciones válidas en la lógica proposicional,
	con sus respectivas precedencias y asociaciones.
-}

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

(===) :: Term -> Term -> Equation
(===) = Equa

infixl 3 \/
infixl 3 /\
infixr 2 ==>
infixl 1 <==>
infixl 1 !<==>
infixl 0 ===

-----------------------------------------------------------

prop :: Float -> Equation
prop num
	| num == 3.1  = (p <==> q) <==> r === p <==> (q <==> r)     -- axiom
	| num == 3.2  = (p <==> q) === (q <==> p)                   -- axiom
	| num == 3.3  = true === p <==> p                           -- axiom
	| num == 3.4  = p === p <==> true                           -- axiom
	| num == 3.5  = (p <==> q) <==> q === p                     -- theorem
	| num == 3.6  = p === (q <==> q) <==> p                     -- theorem
	| num == 3.7  = (p <==> p) <==> (q <==> q) === true         -- theorem
	| num == 3.8  = false === neg true                          -- axiom
	| num == 3.9  = neg (p <==> q) === neg p <==> q             -- axiom
	| num == 3.101 = p !<==> q === neg (p <==> q)               -- axiom
	| num == 3.11 = neg p === q <==> (p <==> neg q)             -- theorem
	| num == 3.12 = neg (neg p) === p                           -- theorem
	| num == 3.13 = neg false === true                          -- theorem
	| num == 3.14 = p !<==> q === neg p <==> q                  -- theorem
	| num == 3.15 = neg p <==> p === false                      -- theorem
	| num == 3.16 = p !<==> q === q !<==> p                     -- theorem
	| num == 3.17 = (p !<==> q) !<==> r === p !<==> (q !<==> r) -- theorem
	| num == 3.18 = (p !<==> q) <==> r === p !<==> (q <==> r)   -- theorem
	| num == 3.19 = (p !<==> q) <==> r === p <==> (q !<==> r)   -- theorem
	| num == 3.201 = neg p <==> q === p <==> neg q              -- theorem
	| num == 3.21 = q === (p !<==> q) <==> neg p                -- theorem
	| num == 3.22 = neg p === p <==> false                      -- theorem
	| num == 3.23 = p !<==> q === r <==> (p <==> (q !<==> r))   -- theorem
	| num == 3.24 = p \/ q === q \/ p                           -- axiom
	| num == 3.25 = (p \/ q) \/ r === p \/ (q \/ r)             -- axiom
	| num == 3.26 = p \/ p === p                                -- axiom
	| num == 3.27 = p \/ (q <==> r) === p \/ q <==> p \/ r      -- axiom
	| num == 3.28 = p \/ neg p === true                         -- axiom
	| num == 3.29 = p \/ true === true                          -- theorem
	| num == 3.301 = p \/ false === p                           -- theorem
	| num == 3.31 = p \/ (q \/ r) === (p \/ q) \/ (p \/ r)      -- theorem
	| num == 3.32 = p \/ q === p \/ neg q <==> p                -- theorem
	| otherwise = error "The statement doesn't exists"