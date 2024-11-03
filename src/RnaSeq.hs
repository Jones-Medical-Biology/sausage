module RnaSeq (importData) where

import Data.Csv (FromNamedRecord, decodeByName, (.:))

type Count = Int

importData :: FilePath -> ()
importData x = 

data TsvRow = NcbiFpkmRow
  { firstColumn :: !String
  , sampleCounts :: ![Count]
  } deriving Show

instance FromNamedRecord TsvRow where
  parseNamedRecord r = do
    first <- r .: "GeneID"

