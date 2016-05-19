--{-# LANGUAGE FlexibleInstances #-}

module Equation where

import Sust
import Term

data Equation = Equa Term Term

instance Show Equation where show = showEquation

(===) :: Term -> Term -> Equation
(===) = Equa
infixl 0 ===

showEquation :: Equation -> String
showEquation (Equa expL expR) = "\t" ++ (showTerm expL) ++ "\n === \n\t" ++ (showTerm expR)