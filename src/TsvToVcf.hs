{-# LANGUAGE OverloadedStrings #-}
module TsvToVcf
  ( gnomadsummarycoverageconvert ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import System.IO (withFile, IOMode(..), hPutStrLn)

data TsvRow = TsvRow
  { locus         :: !Text
  , mean          :: !Double
  , medianApprox  :: !Double
  , totalDP       :: !Int
  , over1         :: !Double
  , over5         :: !Double
  , over10        :: !Double
  , over15        :: !Double
  , over20        :: !Double
  , over25        :: !Double
  , over30        :: !Double
  , over50        :: !Double
  , over100       :: !Double
  } deriving (Show)

instance Csv.FromNamedRecord TsvRow where
  parseNamedRecord r = TsvRow <$> r Csv..: "locus"
                              <*> r Csv..: "mean"
                              <*> r Csv..: "median_approx"
                              <*> r Csv..: "total_DP"
                              <*> r Csv..: "over_1"
                              <*> r Csv..: "over_5"
                              <*> r Csv..: "over_10"
                              <*> r Csv..: "over_15"
                              <*> r Csv..: "over_20"
                              <*> r Csv..: "over_25"
                              <*> r Csv..: "over_30"
                              <*> r Csv..: "over_50"
                              <*> r Csv..: "over_100"

gnomadsummarycoverageconvert :: IO ()
gnomadsummarycoverageconvert = do
  let tsvFilePath = "gnomad.exomes.v4.0.coverage.summary.toplines.tsv"
  let vcfFilePath = "annotated_output.vcf"

  -- Read TSV file
  tsvData <- BL.readFile tsvFilePath
  case Csv.decodeByName tsvData of
    Left err -> putStrLn err
    Right (_, v) -> withFile vcfFilePath WriteMode $ \h -> do
      -- Write VCF header
      hPutStrLn h "##fileformat=VCFv4.2"
      hPutStrLn h "##INFO=<ID=mean,Number=1,Type=Float,Description=\"Mean\">"
      hPutStrLn h "##INFO=<ID=median_approx,Number=1,Type=Float,Description=\"Median Approximation\">"
      hPutStrLn h "##INFO=<ID=total_DP,Number=1,Type=Integer,Description=\"Total DP\">"
      hPutStrLn h "##INFO=<ID=over_1,Number=1,Type=Float,Description=\"Coverage Over 1\">"
      hPutStrLn h "##INFO=<ID=over_5,Number=1,Type=Float,Description=\"Coverage Over 5\">"
      hPutStrLn h "##INFO=<ID=over_10,Number=1,Type=Float,Description=\"Coverage Over 10\">"
      hPutStrLn h "##INFO=<ID=over_15,Number=1,Type=Float,Description=\"Coverage Over 15\">"
      hPutStrLn h "##INFO=<ID=over_20,Number=1,Type=Float,Description=\"Coverage Over 20\">"
      hPutStrLn h "##INFO=<ID=over_25,Number=1,Type=Float,Description=\"Coverage Over 25\">"
      hPutStrLn h "##INFO=<ID=over_30,Number=1,Type=Float,Description=\"Coverage Over 30\">"
      hPutStrLn h "##INFO=<ID=over_50,Number=1,Type=Float,Description=\"Coverage Over 50\">"
      hPutStrLn h "##INFO=<ID=over_100,Number=1,Type=Float,Description=\"Coverage Over 100\">"
      hPutStrLn h "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO"

      -- Process and write VCF data
      V.forM_ v $ \row -> do
        let (chrom, pos) = break (== ':') (unpack $ locus row)
        let info = "mean=" <> show (mean row)
                 <> ";median_approx=" <> show (medianApprox row)
                 <> ";total_DP=" <> show (totalDP row)
                 <> ";over_1=" <> show (over1 row)
                 <> ";over_5=" <> show (over5 row)
                 <> ";over_10=" <> show (over10 row)
                 <> ";over_15=" <> show (over15 row)
                 <> ";over_20=" <> show (over20 row)
                 <> ";over_25=" <> show (over25 row)
                 <> ";over_30=" <> show (over30 row)
                 <> ";over_50=" <> show (over50 row)
                 <> ";over_100=" <> show (over100 row)
        hPutStrLn h (chrom <> "\t" <> tail pos <> "\t.\tN\t.\t.\t.\t" <> info)
