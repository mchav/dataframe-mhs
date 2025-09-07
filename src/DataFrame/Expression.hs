module DataFrame.Expression where

data Expr a =
      Col String
    | Lit a
    | UnaryIntOp (Int -> a) (Expr Int)
    | UnaryDoubleOp (Double -> a) (Expr Double)
    | UnaryStringOp (String -> a) (Expr String)
    | UnaryBoolOp (Bool -> a) (Expr Bool)

    -- Binary Ops
    | BinaryIntToIntOp (Int -> Int -> a) (Expr Int) (Expr Int)
    | BinaryIntToDoubleOp (Int -> Double -> a) (Expr Int) (Expr Double)
    | BinaryIntToStringOp (Int -> String -> a) (Expr Int) (Expr String)
    | BinaryIntToBoolOp (Int -> Bool -> a) (Expr Int) (Expr Bool)

    | BinaryDoubleToIntOp (Double -> Int -> a) (Expr Double) (Expr Int)
    | BinaryDoubleToDoubleOp (Double -> Double -> a) (Expr Double) (Expr Double)
    | BinaryDoubleToStringOp (Double -> String -> a) (Expr Double) (Expr String)
    | BinaryDoubleToBoolOp (Double -> Bool -> a) (Expr Double) (Expr Bool)
    
    | BinaryStringToIntOp (String -> Int -> a) (Expr String) (Expr Int)
    | BinaryStringToDoubleOp (String -> Double -> a) (Expr String) (Expr Double)
    | BinaryStringToStringOp (String -> String -> a) (Expr String) (Expr String)
    | BinaryStringToBoolOp (String -> Bool -> a) (Expr String) (Expr Bool)

    | BinaryBoolToIntOp (Bool -> Int -> a) (Expr Bool) (Expr Int)
    | BinaryBoolToDoubleOp (Bool -> Double -> a) (Expr Bool) (Expr Double)
    | BinaryBoolToStringOp (Bool -> String -> a) (Expr Bool) (Expr String)
    | BinaryBoolToBoolOp (Bool -> Bool -> a) (Expr Bool) (Expr Bool)
