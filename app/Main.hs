module Main (main) where

import Control.Monad

import qualified DataFrame.Core as D
import qualified DataFrame.Column as D
import qualified DataFrame.Expression as D
import qualified DataFrame.Functions as D
import qualified DataFrame.PrettyPrint as D

main :: IO ()
main = do
  let high = D.Col "High Temperature (Celcius)" :: D.Expr Int
  let highs = (take 1000 (cycle [20])) ++ [(24 :: Int), 20, 22, 23, 25, 26, 26]
  let lows = (take 1000 (cycle [20])) ++ [(14 :: Int), 13, 13, 13, 14, 15, 15]
  forM_ [0..999] $ \ _ -> do
    let df = D.fromNamedColumns [
                ("Day", D.fromList (take (length highs) (cycle ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]))),
                ("High Temperature (Celcius)", D.fromList highs),
                ("Low Temperature (Celcius)", D.fromList lows)]
    let hotDays = D.filterWhere (high `D.geq` D.Lit 25)  df
    putStrLn ""
    putStrLn (D.renderMarkdownTable (Just 10) hotDays)
