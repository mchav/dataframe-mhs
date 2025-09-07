{-# LANGUAGE FlexibleInstances #-}

module DataFrame.Column where

-- | Column space with a closed type universe.
data Column = CInt [(Int, Int)]
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

class ToColumn a where
  toColumn :: [(Int, a)] -> Column

instance ToColumn Int    where toColumn = CInt
instance ToColumn Double where toColumn = CDouble
instance ToColumn [Char] where toColumn = CString
instance ToColumn Bool where toColumn = CBool

fromList :: ToColumn a => [a] -> Column
fromList xs = toColumn (zip [0..] xs)

-- | Map-like function
class Transform a where
    transform :: ToColumn b => (a -> b) -> Column -> Column

instance Transform Int where
    transform f (CInt xs) = toColumn (map (\(i, v) ->(i, f v)) xs)
    transform _ _ = error "Expected Int"

instance Transform Double where
    transform f (CDouble xs) = toColumn (map (\(i, v) ->(i, f v)) xs)
    transform _ _ = error "Expected Double"

instance Transform [Char] where
    transform f (CString xs) = toColumn (map (\(i, v) ->(i, f v)) xs)
    transform _ _ = error "Expected String"

instance Transform Bool where
    transform f (CBool xs) = toColumn (map (\(i, v) ->(i, f v)) xs)
    transform _ _ = error "Expected Bool"

atIndicies :: [Int] -> Column -> Column
atIndicies ixs (CInt xs) = CInt (filter ((`elem` ixs) . fst) xs)
atIndicies ixs (CDouble xs) = CDouble (filter ((`elem` ixs) . fst) xs)
atIndicies ixs (CString xs) = CString (filter ((`elem` ixs) . fst) xs)

getIndicies :: Column -> [Int]
getIndicies (CInt xs) = map fst xs
getIndicies (CDouble xs) = map fst xs
getIndicies (CString xs) = map fst xs

columnLength :: Column -> Int
columnLength (CInt xs) = length xs
columnLength (CDouble xs) = length xs
columnLength (CString xs) = length xs
columnLength (CBool xs) = length xs
