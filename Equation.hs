module Equation where

import Sust
import Term

data Equation = Equa Term Term deriving (Eq)

instance Show Equation where show = showEquation

(===) :: Term -> Term -> Equation
(===) = Equa
infixl 0 ===

showEquation :: Equation -> String
showEquation (Equa expL expR) = "\t" ++ (showTerm expL) ++ "\n === \n\t" ++ (showTerm expR)

--firstTerm :: Equation -> Term
--firstTerm (Equa t1 t2) = t1

--secondTerm :: Equation -> Term
--secondTerm (Equa t1 t2) = t2