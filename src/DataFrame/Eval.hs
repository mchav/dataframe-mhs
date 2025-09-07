module DataFrame.Eval where

import DataFrame.Core
import DataFrame.Column
import DataFrame.Expression

interpret :: (ToColumn a) => Expr a -> DataFrame -> Column
interpret (Col name) df = case lookup name (columns df) of
    Nothing -> fromList ([] :: [[Char]])
    Just c  -> c
interpret (Lit value) df = let
        ixs = case (columns df) of
            ((_, (CInt xs)): rest)    -> map fst xs
            ((_, (CDouble xs)): rest) -> map fst xs
            ((_, (CString xs)): rest) -> map fst xs
            ((_, (CBool xs)): rest)   -> map fst xs
            []                         -> ([] :: [Int])
    in toColumn (zip ixs (replicate (length ixs) value))
interpret (UnaryIntOp f e) df = transform f (interpret e df)
interpret (UnaryDoubleOp f e) df = transform f (interpret e df)
interpret (UnaryStringOp f e) df = transform f (interpret e df)
interpret (UnaryBoolOp f e) df = transform f (interpret e df)
interpret (BinaryIntToIntOp f l r) df = case (interpret l df, interpret r df) of
    -- Assumes indicies are the same.
    -- TODO: We could line these up.
    (CInt xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryIntToDoubleOp f l r) df = case (interpret l df, interpret r df) of
    (CInt xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryIntToStringOp f l r) df = case (interpret l df, interpret r df) of
    (CInt xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryIntToBoolOp f l r) df = case (interpret l df, interpret r df) of
    (CInt xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
-- | Double
interpret (BinaryDoubleToIntOp f l r) df = case (interpret l df, interpret r df) of
    (CDouble xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryDoubleToDoubleOp f l r) df = case (interpret l df, interpret r df) of
    (CDouble xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryDoubleToStringOp f l r) df = case (interpret l df, interpret r df) of
    (CDouble xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryDoubleToBoolOp f l r) df = case (interpret l df, interpret r df) of
    (CDouble xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
-- | String
interpret (BinaryStringToIntOp f l r) df = case (interpret l df, interpret r df) of
    (CString xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryStringToDoubleOp f l r) df = case (interpret l df, interpret r df) of
    (CString xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryStringToStringOp f l r) df = case (interpret l df, interpret r df) of
    (CString xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryStringToBoolOp f l r) df = case (interpret l df, interpret r df) of
    (CString xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
-- | Bool
interpret (BinaryBoolToIntOp f l r) df = case (interpret l df, interpret r df) of
    (CBool xs, CInt ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryBoolToDoubleOp f l r) df = case (interpret l df, interpret r df) of
    (CBool xs, CDouble ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryBoolToStringOp f l r) df = case (interpret l df, interpret r df) of
    (CBool xs, CString ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
interpret (BinaryBoolToBoolOp f l r) df = case (interpret l df, interpret r df) of
    (CBool xs, CBool ys) -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
    _ -> error "Type mismatch"
