{-# LANGUAGE TypeApplications #-}

module Main (main) where

import DataFrame ((.>=), (|>))
import qualified DataFrame as D

main :: IO ()
main = do
    let highs = [(24 :: Double), 20, 22, 23, 25, 26, 26, 20.2, 20.8, 25.2]
    let lows = [(14 :: Double), 13, 13, 13, 14, 15, 15, 12.1, 12.2, 12.5]
    let df =
            D.fromNamedColumns
                [ ("Day", D.fromList (take (length highs) (cycle ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"])))
                , ("High Temperature (Celcius)", D.fromList highs)
                , ("Low Temperature (Celcius)", D.fromList lows)
                ]

    -- Typed column references
    let high = D.Col @Double "High Temperature (Celcius)"
    let low = D.Col @Double "Low Temperature (Celcius)"

    -- Some expressions.
    let hotDays =
            df
                |> D.filterWhere (high .>= 25)
                |> D.derive "high_fahrenheit" (toFahrenheit high)
                |> D.derive "low_fahrenheit" (toFahrenheit low)
                |> D.derive "average (fahrenheit)" ((high + low) / 2)
                |> D.select ["Day", "average (fahrenheit)"]
    putStrLn ""
    putStrLn (D.renderMarkdownTable Nothing hotDays)
    putStrLn $ show (hotDays |> D.sortBy (D.Asc "average (fahrenheit)"))
    putStrLn $ show (hotDays |> D.sortBy (D.Desc "average (fahrenheit)"))

toFahrenheit :: D.Expr Double -> D.Expr Double
toFahrenheit t = (t * (9 / 5)) + 2
