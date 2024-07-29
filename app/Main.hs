{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))
import GHC.Generics

import Lib
import TsvToVcf

data Command = GnomadConvert
  | ParseCsv
  deriving (Show, Generic)

gnomadConvertParser :: Parser Command
gnomadConvertParser = GnomadConvert
  <$> argument str (metavar "INPUT" <> help "Input TSV file")

parseCsvParser :: Parser Command
parseCsvParser = ParseCsv
  <$> argument str
    ( metavar "INPUT"
      <> help "Input CSV file" )

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
  command <- execParser opts
  case command of
    GnomadConvert -> gnomadsummarycoverageconvert
    ParseCsv -> do
      c <- getContents
      case parse csvFile "(stdin)" c of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right r -> mapM_ print r
