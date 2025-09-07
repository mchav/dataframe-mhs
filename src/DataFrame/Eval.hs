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
interpret (BinaryIntToIntOp f l r) df = case interpret l df of
    CInt xs -> case interpret r df of
        -- Assumes indices are the same.
        CInt ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryIntToDoubleOp f l r) df = case interpret l df of
    CInt xs -> case interpret r df of
        -- Assumes indices are the same.
        CDouble ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryIntToStringOp f l r) df = case interpret l df of
    CInt xs -> case interpret r df of
        -- Assumes indices are the same.
        CString ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryIntToBoolOp f l r) df = case interpret l df of
    CInt xs -> case interpret r df of
        -- Assumes indices are the same.
        CBool ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
-- | Double
interpret (BinaryDoubleToIntOp f l r) df = case interpret l df of
    CDouble xs -> case interpret r df of
        -- Assumes indices are the same.
        CInt ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryDoubleToDoubleOp f l r) df = case interpret l df of
    CDouble xs -> case interpret r df of
        -- Assumes indices are the same.
        CDouble ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryDoubleToStringOp f l r) df = case interpret l df of
    CDouble xs -> case interpret r df of
        -- Assumes indices are the same.
        CString ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryDoubleToBoolOp f l r) df = case interpret l df of
    CDouble xs -> case interpret r df of
        -- Assumes indices are the same.
        CBool ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
-- | String
interpret (BinaryStringToIntOp f l r) df = case interpret l df of
    CString xs -> case interpret r df of
        -- Assumes indices are the same.
        CInt ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryStringToDoubleOp f l r) df = case interpret l df of
    CString xs -> case interpret r df of
        -- Assumes indices are the same.
        CDouble ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryStringToStringOp f l r) df = case interpret l df of
    CString xs -> case interpret r df of
        -- Assumes indices are the same.
        CString ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryStringToBoolOp f l r) df = case interpret l df of
    CString xs -> case interpret r df of
        -- Assumes indices are the same.
        CBool ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
-- | Bool
interpret (BinaryBoolToIntOp f l r) df = case interpret l df of
    CBool xs -> case interpret r df of
        -- Assumes indices are the same.
        CInt ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryBoolToDoubleOp f l r) df = case interpret l df of
    CBool xs -> case interpret r df of
        -- Assumes indices are the same.
        CDouble ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryBoolToStringOp f l r) df = case interpret l df of
    CBool xs -> case interpret r df of
        -- Assumes indices are the same.
        CString ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
interpret (BinaryBoolToBoolOp f l r) df = case interpret l df of
    CBool xs -> case interpret r df of
        -- Assumes indices are the same.
        CBool ys -> toColumn (zipWith (\(a, b) (a', b') -> (a, f b b')) xs ys)
        _ -> error "Wrong type"
    _ -> error "Wrong type"
