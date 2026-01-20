{-# LANGUAGE FlexibleInstances #-}

module DataFrame.Column where

-- | Column space with a closed type universe.
data Column
    = CInt [(Int, Int)]
    | CDouble [(Int, Double)]
    | CString [(Int, String)]
    | CBool [(Int, Bool)]

columnType :: Column -> String
columnType (CInt _) = "Int"
columnType (CDouble _) = "Double"
columnType (CString _) = "String"
columnType (CBool _) = "Bool"

instance Show Column where
    show (CInt xs) = show (map snd xs)
    show (CDouble xs) = show (map snd xs)
    show (CString xs) = show (map snd xs)
    show (CBool xs) = show (map snd xs)

class Columnable a where
    toColumn :: [(Int, a)] -> Column
    fromColumn :: Column -> [(Int, a)]
    unary :: (Columnable b) => (b -> a) -> Column -> Column

    -- TODO: Implementations should align columns.
    -- Right now we assume columns are the same.
    binary :: (Columnable b, Columnable c) => (c -> b -> a) -> Column -> Column -> Column

instance Columnable Int where
    toColumn = CInt
    fromColumn (CInt xs) = xs
    unary f c = toColumn (map (\(i, v) -> (i, f v)) (fromColumn c))
    binary f x y = toColumn (zipWith (\(i, v) (i', v') -> (i, f v v')) (fromColumn x) (fromColumn y))

instance Columnable Double where
    toColumn = CDouble
    fromColumn (CDouble xs) = xs
    unary f c = toColumn (map (\(i, v) -> (i, f v)) (fromColumn c))
    binary f x y = toColumn (zipWith (\(i, v) (i', v') -> (i, f v v')) (fromColumn x) (fromColumn y))

instance Columnable [Char] where
    toColumn = CString
    fromColumn (CString xs) = xs
    unary f c = toColumn (map (\(i, v) -> (i, f v)) (fromColumn c))
    binary f x y = toColumn (zipWith (\(i, v) (i', v') -> (i, f v v')) (fromColumn x) (fromColumn y))

instance Columnable Bool where
    toColumn = CBool
    fromColumn (CBool xs) = xs
    unary f c = toColumn (map (\(i, v) -> (i, f v)) (fromColumn c))
    binary f x y = toColumn (zipWith (\(i, v) (i', v') -> (i, f v v')) (fromColumn x) (fromColumn y))

fromList :: (Columnable a) => [a] -> Column
fromList xs = toColumn (zip [0 ..] xs)

atIndicies :: [Int] -> Column -> Column
atIndicies ixs (CInt xs) = CInt (filter ((`elem` ixs) . fst) xs)
atIndicies ixs (CDouble xs) = CDouble (filter ((`elem` ixs) . fst) xs)
atIndicies ixs (CString xs) = CString (filter ((`elem` ixs) . fst) xs)
atIndicies ixs (CBool xs) = CBool (filter ((`elem` ixs) . fst) xs)
