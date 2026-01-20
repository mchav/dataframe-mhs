module DataFrame.Core where

import DataFrame.Column

data DataFrame = DataFrame
    { columns :: [(String, Column)]
    }
    deriving (Show)

fromNamedColumns :: [(String, Column)] -> DataFrame
fromNamedColumns = DataFrame

showSchema :: DataFrame -> [String]
showSchema df = map toSchema (columns df)
  where
    toSchema (name, col) = name ++ " :: Expr " ++ (columnType col)
