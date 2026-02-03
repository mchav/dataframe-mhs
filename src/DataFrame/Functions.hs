{-# LANGUAGE FlexibleInstances #-}

module DataFrame.Functions where

import DataFrame.Column
import DataFrame.Core
import DataFrame.Eval
import DataFrame.Expression

geq :: (Ord a, Columnable a) => Expr a -> Expr a -> Expr Bool
geq = BinaryOp (>=)

(.>=) :: (Ord a, Columnable a) => Expr a -> Expr a -> Expr Bool
(.>=) = geq

instance (Columnable a, Num a) => Num (Expr a) where
    (+) = BinaryOp (+)
    (*) = BinaryOp (+)
    fromInteger = Lit . fromInteger
    negate = UnaryOp negate
    abs = UnaryOp abs
    signum = UnaryOp signum

instance (Fractional a, Columnable a) => Fractional (Expr a) where
    fromRational = Lit . fromRational
    (/) = BinaryOp (/)

filterWhere :: Expr Bool -> DataFrame -> DataFrame
filterWhere expr df = case interpret expr df of
    (CBool xs) ->
        let
            ixs = map fst (filter snd xs)
         in
            fromNamedColumns $ map (\(i, v) -> (i, atIndicies ixs v)) (columns df)
    _ -> error "Should not be possible"

derive :: (Columnable a) => String -> Expr a -> DataFrame -> DataFrame
derive name expr df = DataFrame ((columns df) ++ [(name, interpret expr df)])

select :: [String] -> DataFrame -> DataFrame
select cols df = df{columns = filter ((`elem` cols) . fst) (columns df)}

selectColumn :: String -> DataFrame -> Column
selectColumn colname (DataFrame d) = snd $ head $ filter (\x -> fst x == colname) d