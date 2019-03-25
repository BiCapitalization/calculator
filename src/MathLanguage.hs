{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module MathLanguage
    (Expression (..),
    eval) where

data Expression :: * where
    Constant :: Double -> Expression
    Neg :: Expression -> Expression
    Add :: Expression -> Expression -> Expression
    Sub :: Expression -> Expression -> Expression
    Mul :: Expression -> Expression -> Expression
    Div :: Expression -> Expression -> Expression
    deriving (Show, Eq)

eval :: Expression -> Double
eval (Constant val) = val
eval (Neg ex) = -(eval ex)
eval (Add ex1 ex2) = (eval ex1) + (eval ex2)
eval (Sub ex1 ex2) = (eval ex1) - (eval ex2)
eval (Mul ex1 ex2) = (eval ex1) * (eval ex2)
eval (Div ex1 ex2) = (eval ex1) / (eval ex2)
