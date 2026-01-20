module DataFrame.Eval where

import DataFrame.Column
import DataFrame.Core
import DataFrame.Expression

import Data.Maybe

interpret :: (Columnable a) => Expr a -> DataFrame -> Column
interpret (Lit v) df = fromList (replicate n v)
  where
    n = case (columns df) of
        ((_, CInt xs) : _) -> length xs
        ((_, CDouble xs) : _) -> length xs
        ((_, CString xs) : _) -> length xs
        ((_, CBool xs) : _) -> length xs
interpret (Col name) df = fromMaybe (fromList ([] :: [String])) (lookup name (columns df))
interpret (UnaryOp f expr) df = unary f (interpret expr df)
interpret (BinaryOp f l r) df = binary f (interpret l df) (interpret r df)
