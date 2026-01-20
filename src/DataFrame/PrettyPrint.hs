module DataFrame.PrettyPrint (
    renderMarkdownTable,
) where

import qualified Data.List as L
import Data.Maybe
import DataFrame.Column
import DataFrame.Core

colMap :: Column -> [(Int, String)]
colMap (CInt xs) = [(i, show v) | (i, v) <- xs]
colMap (CDouble xs) = [(i, show v) | (i, v) <- xs]
colMap (CString xs) = [(i, v) | (i, v) <- xs]

escapeCell :: String -> String
escapeCell =
    concatMap
        ( \c -> case c of
            '|' -> "\\|"
            '\n' -> "<br>"
            _ -> [c]
        )

data Align = LeftA | RightA

colAlign :: Column -> Align
colAlign (CString _) = LeftA
colAlign _ = RightA

fit :: Int -> String -> String
fit w s =
    let t = escapeCell s
     in if length t <= w then t else take (max 1 (w - 1)) t ++ "…"

pad :: Align -> Int -> String -> String
pad LeftA w s = s ++ replicate (max 0 (w - length s)) ' '
pad RightA w s = replicate (max 0 (w - length s)) ' ' ++ s

alignMarker :: Align -> Int -> String
alignMarker LeftA w = ":" ++ replicate (max 0 (w - 1)) '-'
alignMarker RightA w = replicate (max 0 (w - 1)) '-' ++ ":"

renderMarkdownTable :: Maybe Int -> DataFrame -> String
renderMarkdownTable mW (DataFrame cols) =
    let names = map fst cols
        maps = map (colMap . snd) cols
        aligns = map (colAlign . snd) cols
        allIdx = L.sort . L.nub $ concatMap (map fst) maps

        autoW =
            let headerParts = "row" : names
                cellLens = concatMap (map (length . escapeCell) . (map snd)) maps
                headerLens = map length headerParts
                rowLens = map (length . show) allIdx
             in maximum (1 : (cellLens ++ headerLens ++ rowLens))

        w = max 3 $ maybe autoW id mW

        fmtHeaderCell a s = pad a w (fit w s)
        fmtCell a s = pad a w (fit w s)

        header =
            let first = fmtHeaderCell RightA "row"
                rest = zipWith (\nm _ -> fmtHeaderCell LeftA nm) names aligns
             in "| " ++ L.intercalate " | " (first : rest) ++ " |\n"

        sep =
            let first = alignMarker RightA w
                rest = map (`alignMarker` w) aligns
             in "| " ++ L.intercalate " | " (first : rest) ++ " |\n"

        row i =
            let first = fmtCell RightA (show i)
                rest =
                    zipWith3
                        (\m a _ -> fmtCell a (fromMaybe "" (lookup i m)))
                        maps
                        aligns
                        names
             in "| " ++ L.intercalate " | " (first : rest) ++ " |\n"
     in header ++ sep ++ concatMap row allIdx
