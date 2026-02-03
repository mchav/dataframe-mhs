module DataFrame.Permutation where 

import DataFrame.Column
import DataFrame.Core
import DataFrame.Functions

import Data.List (sortOn)
import Data.Ord (Down(..))

-- A bit ugly, but unfortunately because Column is an ADT,
-- I can't hoist f into a single `where`-clause for getOrderedIndices.
f :: (a, (b, c)) -> c
f (_, (_, x)) = x

data SortOrder = Asc String | Desc String

getOrderedIndices :: Bool -> Column -> [Int]
getOrderedIndices True (CInt xs) = map fst $ sortOn f $ zip [0..] xs
getOrderedIndices False (CInt xs) = map fst $ sortOn (Down . f) $ zip [0..] xs
getOrderedIndices True (CDouble xs) = map fst $ sortOn f $ zip [0..] xs
getOrderedIndices False (CDouble xs) = map fst $ sortOn (Down . f) $ zip [0..] xs
getOrderedIndices True (CString xs) = map fst $ sortOn f $ zip [0..] xs
getOrderedIndices False (CString xs) = map fst $ sortOn (Down . f) $ zip [0..] xs
getOrderedIndices True (CBool xs) = map fst $ sortOn f $ zip [0..] xs
getOrderedIndices False (CBool xs) = map fst $ sortOn (Down . f) $ zip [0..] xs

orderListBy :: [Int] -> [a] -> [a]
orderListBy ixs xs = map (xs !!) ixs

orderColumnBy :: [Int] -> Column -> Column
orderColumnBy ixs (CInt xs) = CInt $ orderListBy ixs xs
orderColumnBy ixs (CDouble xs) = CDouble $ orderListBy ixs xs
orderColumnBy ixs (CString xs) = CString $ orderListBy ixs xs
orderColumnBy ixs (CBool xs) = CBool $ orderListBy ixs xs

orderDataFrameBy :: [Int] -> DataFrame -> DataFrame
orderDataFrameBy ixs (DataFrame df) = fromNamedColumns $ map (\(colname, col) -> (colname, orderColumnBy ixs col)) df

sortBy :: SortOrder -> DataFrame -> DataFrame
sortBy (Asc colname) df = orderDataFrameBy (getOrderedIndices True (selectColumn colname df)) df 
sortBy (Desc colname) df = orderDataFrameBy (getOrderedIndices False (selectColumn colname df)) df 