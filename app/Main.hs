module Main (main) where

import Control.Monad

import DataFrame ((|>))
import qualified DataFrame as D

main :: IO ()
main = do
    let highs = [(24 :: Double), 20, 22, 23, 25, 26, 26]
    let lows = [(14 :: Double), 13, 13, 13, 14, 15, 15]
    let df =
            D.fromNamedColumns
                [ ("Day", D.fromList (take (length highs) (cycle ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"])))
                , ("High Temperature (Celcius)", D.fromList highs)
                , ("Low Temperature (Celcius)", D.fromList lows)
                ]

    -- Typed column references
    let high = D.Col "High Temperature (Celcius)" :: D.Expr Double
    let low = D.Col "Low Temperature (Celcius)" :: D.Expr Double

    -- Some expressions.
    let hotDays =
            df
                |> D.filterWhere (high `D.geq` 25)
                |> D.derive "high_fahrenheit" (toFahrenheit high)
                |> D.derive "low_fahrenheit" (toFahrenheit low)
                |> D.derive "average (celsius)" ((high + low) / 2)
    putStrLn ""
    putStrLn (D.renderMarkdownTable (Just 10) hotDays)

toFahrenheit :: D.Expr Double -> D.Expr Double
toFahrenheit t = (t * (9 / 5)) + 2
