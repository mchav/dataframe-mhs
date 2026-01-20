module DataFrame.Expression where

import DataFrame.Column

data Expr a where
    Col :: String -> Expr a
    Lit :: a -> Expr a
    UnaryOp :: (Columnable b) => (b -> a) -> Expr b -> Expr a
    BinaryOp :: (Columnable b, Columnable c) => (c -> b -> a) -> Expr c -> Expr b -> Expr a
