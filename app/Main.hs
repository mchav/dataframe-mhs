module Main (main) where

import Control.Monad
import Data.Function

import qualified DataFrame.Core as D
import qualified DataFrame.Column as D
import qualified DataFrame.Expression as D
import qualified DataFrame.Functions as D
import qualified DataFrame.PrettyPrint as D

main :: IO ()
main = do
  let highs = [(24 :: Int), 20, 22, 23, 25, 26, 26]
  let lows = [(14 :: Int), 13, 13, 13, 14, 15, 15]
  let df = D.fromNamedColumns [
                ("Day", D.fromList (take (length highs) (cycle ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]))),
                ("High Temperature (Celcius)", D.fromList highs),
                ("Low Temperature (Celcius)", D.fromList lows)]

  -- Typed column references
  let high = D.Col "High Temperature (Celcius)" :: D.Expr Int
  let low = D.Col "Low Temperature (Celcius)" :: D.Expr Int

  -- Some expressions.
  let hotDays = df
              & D.filterWhere (high `D.geq` D.Lit 25)
              & D.derive "total" (high + low) 
              & D.derive "year" (D.Lit (2025 :: Int)) 
  putStrLn ""
  putStrLn (D.renderMarkdownTable (Just 10) hotDays)
