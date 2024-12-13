{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import System.Environment (getArgs)
import Options.Applicative
import Data.Semigroup ((<>))
import GHC.Generics
import Text.Parsec (parse)

import Lib
import TsvToVcf
import RnaSeq ( importData, processTFPathways )

data Command = GnomadConvert
  | ParseCsv
  deriving (Show, Generic)

gnomadConvertParser :: Parser Command
gnomadConvertParser = pure GnomadConvert

parseCsvParser :: Parser Command
parseCsvParser = pure ParseCsv
  -- <$> argument str
  --   ( metavar "INPUT"
  --     <> help "Input CSV file" )

commandParser :: Parser Command
commandParser = subparser
  ( command "gnomadconvert" (info gnomadConvertParser (progDesc "For one very specific file, convert TSV to VCF"))
    <> command "parsecsv" (info parseCsvParser (progDesc "Parse CSV file")) )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "Run different tasks based on the command"
   <> header "sausage - a multi-purpose program" )

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:xs) -> case x of
      "import-data" -> importData (head xs :: String) ((head . tail) xs :: FilePath) ((head . tail . tail) xs :: FilePath)
      "stuff" -> do
        z <- getArgs
        processTFPathways z
      "parse" -> do
        command <- execParser opts
        case command of
          GnomadConvert -> gnomadsummarycoverageconvert
          ParseCsv -> do
            c <- getContents
            case parse csvFile "(stdin)" c of
              Left e -> do putStrLn "Error parsing input:"
                           print e
              Right r -> mapM_ print r
          _anyOtherFailure -> print "bad day"
      _ -> print "bad day"
    _ -> print "very bad day"
