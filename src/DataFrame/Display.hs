module DataFrame.Display where

import Data.List
import XHaskell.Display
import DataFrame.Core
import DataFrame.Column
import DataFrame.PrettyPrint

instance Display DataFrame where
  display dataframe = DisplayData {
      mimeType = "text/markdown",
      content  = renderMarkdownTable (Just 10) dataframe
  }

newtype HtmlPlot = HtmlPlot String deriving (Show)

instance Display HtmlPlot where
  display (HtmlPlot val) = DisplayData {
      mimeType = "text/html",
      content  = val
  }

data PlotConfig = PlotConfig
    { plotType :: PlotType
    , plotTitle :: String
    , plotWidth :: Int
    , plotHeight :: Int
    , plotFile :: Maybe String
    }

data PlotType
    = Histogram
    | Scatter
    | Line
    | Bar
    | BoxPlot
    | Pie
    | StackedBar
    | Heatmap
    deriving (Eq, Show)

defaultPlotConfig :: PlotType -> PlotConfig
defaultPlotConfig ptype =
    PlotConfig
        { plotType = ptype
        , plotTitle = ""
        , plotWidth = 600
        , plotHeight = 400
        , plotFile = Nothing
        }

calculateHistogram :: [Int] -> [Int] -> Int -> [Int]
calculateHistogram values bins binWidth =
    let countBin b = length [v | v <- values, v >= b && v < b + binWidth]
     in map countBin bins

plotHistogram :: String -> Int -> DataFrame -> IO HtmlPlot
plotHistogram colName numBins = plotHistogramWith colName numBins (defaultPlotConfig Histogram)

plotHistogramWith :: String -> Int -> PlotConfig -> DataFrame -> IO HtmlPlot
plotHistogramWith colName numBins config df = do
    let chartId = "histogram"
    let (Just values) = fmap (map snd . fromColumn @Int) (lookup colName (columns df))
        (minVal, maxVal) = if null values then (0, 1) else (minimum values, maximum values)
        binWidth = (maxVal - minVal) `div` numBins
        bins = [minVal + i * binWidth | i <- [0 .. numBins - 1]]
        counts = calculateHistogram values bins binWidth

        labels =
            intercalate
                ","
                [ "\"" <> show b <> "\""
                | b <- bins
                ]
        dataPoints = intercalate "," [show c | c <- counts]

        chartTitle =
            if null (plotTitle config)
                then "Histogram of " <> colName
                else plotTitle config

        jsCode =
            concat
                [ "setTimeout(function() { new Chart(\""
                , chartId
                , "\", {\n"
                , "  type: \"bar\",\n"
                , "  data: {\n"
                , "    labels: ["
                , labels
                , "],\n"
                , "    datasets: [{\n"
                , "      label: \""
                , colName
                , "\",\n"
                , "      data: ["
                , dataPoints
                , "],\n"
                , "      backgroundColor: \"rgba(75, 192, 192, 0.6)\",\n"
                , "      borderColor: \"rgba(75, 192, 192, 1)\",\n"
                , "      borderWidth: 1\n"
                , "    }]\n"
                , "  },\n"
                , "  options: {\n"
                , "    title: { display: true, text: \""
                , chartTitle
                , "\" },\n"
                , "    scales: {\n"
                , "      yAxes: [{ ticks: { beginAtZero: true } }]\n"
                , "    }\n"
                , "  }\n"
                , "})}, 100);"
                ]

    return $
        HtmlPlot $
            wrapInHTML chartId jsCode (plotWidth config) (plotHeight config)

wrapInHTML :: String -> String -> Int -> Int -> String
wrapInHTML chartId content width height =
    concat
        [ "<canvas id=\""
        , chartId
        , "\" style=\"width:100%;max-width:"
        , show width
        , "px;height:"
        , show height
        , "px\"></canvas>\n"
        , "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.min.js\"></script>\n"
        , "<script>\n"
        , content
        , "\n</script>\n"
        ]
