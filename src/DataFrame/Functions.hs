{-# LANGUAGE FlexibleInstances #-}
module DataFrame.Functions where

import DataFrame.Core
import DataFrame.Column
import DataFrame.Expression
import DataFrame.Eval

-- | Each function must have a type class that specialies it to the
-- | right type.

class ExprEq a where
    eq :: Expr a -> Expr a -> Expr Bool

instance ExprEq Int where
    eq l r = BinaryIntToIntOp (==) l r

instance ExprEq Double where
    eq l r = BinaryDoubleToDoubleOp (==) l r

instance ExprEq [Char] where
    eq l r = BinaryStringToStringOp (==) l r

instance ExprEq Bool where
    eq l r = BinaryBoolToBoolOp (==) l r


class ExprGeq a where
    geq :: Expr a -> Expr a -> Expr Bool

instance ExprGeq Int where
    geq l r = BinaryIntToIntOp (>=) l r

instance ExprGeq Double where
    geq l r = BinaryDoubleToDoubleOp (>=) l r

instance ExprGeq [Char] where
    geq l r = BinaryStringToStringOp (>=) l r

instance ExprGeq Bool where
    geq l r = BinaryBoolToBoolOp (>=) l r

-- For addition
class ExprAdd a where
    add :: Expr a -> Expr a -> Expr a

instance ExprAdd Int where
    add l r = BinaryIntToIntOp (+) l r

instance ExprAdd Double where
    add l r = BinaryDoubleToDoubleOp (+) l r

instance ExprAdd [Char] where
    add l r = error "Cannot add strings"

instance ExprAdd Bool where
    add l r = error "Cannot add bools"

-- | Num instance
instance (ToColumn a, Num a, ExprAdd a) => Num (Expr a) where
    (+) = add

filterWhere :: Expr Bool -> DataFrame -> DataFrame
filterWhere expr df = case interpret expr df of
    (CBool xs) -> let
            ixs = map fst (filter snd xs)
        in fromNamedColumns $ map (\(i, v) -> (i, atIndicies ixs v)) (columns df)
    _          -> error "Should not be possible"

derive :: ToColumn a => String -> Expr a -> DataFrame -> DataFrame
derive name expr df = DataFrame ((columns df) ++ [(name, interpret expr df)])
