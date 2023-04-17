#! /usr/bin/env stack
{- stack script
    --ghc-options "-Wall"
    --resolver lts-20.18
    --package bytestring
    --package cassava
    --package Chart
    --package Chart-diagrams
    --package text
    --package time
    --package vector
-}

{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad (mzero)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Csv
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector qualified as V
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

-- | Define the data type for each row in the CSV
data Release = Release
  { date :: UTCTime
  , version :: T.Text
  }
  deriving (Show)

-- | Parse a date from the CSV
parseDate :: Csv.Field -> Csv.Parser UTCTime
parseDate field =
  parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpack field)

-- | Parse a row from the CSV
instance Csv.FromRecord Release where
  parseRecord v
    | V.length v == 2 =
        Release
          <$> (v Csv..! 0 >>= parseDate)
          <*> (T.pack . unpack <$> v Csv..! 1)
    | otherwise = mzero

-- | Calculate the number of days between two dates
daysBetween :: UTCTime -> UTCTime -> Int
daysBetween d1 d2 =
  floor $ diffUTCTime d1 d2 / 86400

-- | Calculate difference between consecutive elements
stepSizes :: Num a => V.Vector a -> V.Vector a
stepSizes xs =
  V.zipWith (-) (V.tail xs) xs

-- | Use an improved style for the bar chart
createBars ::
  (PlotValue x, BarsPlotValue y) =>
  [(x, [y])] ->
  EC l (PlotBars x y)
createBars vals = liftEC $ do
  plot_bars_titles .= ["Days since last release"]
  plot_bars_values .= vals
  plot_bars_style .= BarsClustered
  plot_bars_spacing .= BarsFixGap 0.2 2
  plot_bars_item_styles .= [(solidFillStyle $ opaque teal, Nothing)]

{- | Load the CSV file, calculate the number of days since each release,
| and write the chart to an SVG file
-}
main :: IO ()
main = do
  csvData <- BL.readFile "release_data.csv"
  case Csv.decode Csv.HasHeader csvData of
    Left err -> putStrLn err
    Right (releases :: V.Vector Release) -> do
      now <- getCurrentTime

      let
        daysSinceLastRelease =
          releases
            & V.map (daysBetween now . date)
            & stepSizes

      toFile
        (FileOptions (800, 450) SVG loadSansSerifFonts)
        "days_since_last_sqlite_release.svg"
        $ do
          layout_title .= "Days Since Last SQLite Release"
          plot $
            plotBars
              <$> createBars
                ( releases
                    `V.zip` daysSinceLastRelease
                    & V.toList
                    <&> \(release :: Release, day) -> (release.date, [day])
                )
