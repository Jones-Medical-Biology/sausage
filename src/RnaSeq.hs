{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RnaSeq (importData) where

import Data.Text (Text)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Data.Csv ( FromNamedRecord(..)
                , ToNamedRecord(..)
                , DefaultOrdered(..)
                , decodeByName
                , decodeByNameWith
                , (.:)
                , decDelimiter
                , FromRecord(..)
                , ToRecord(..)
                , defaultDecodeOptions
                , decodeWith
                , HasHeader(..)
                , NamedRecord(..)
                , parseNamedRecord
                , Field(..)
                , parseField
                , Parser(..)
                , Header(..)
                , Record(..))
import Data.Char (ord)
import Debug.Trace (trace)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS8
import qualified Distribution.Simple.Utils as Utils

importData :: FilePath -> IO ()
importData x = do
  tsvData <- BL.readFile x
  let decodeOptions = defaultDecodeOptions {
        decDelimiter = fromIntegral (ord '\t') }
      decoded :: Either String (Header, V.Vector NcbiRow)
      decoded = decodeByNameWith decodeOptions tsvData
  case decoded of
    Left err -> putStrLn $ "Error parsing TSV: " ++ err
    Right (header, rows) -> do
      printHeader header
      -- putStrLn "fpkm of top 10 rows"
      -- print $ V.foldl1 (zipWith (+)) (V.map counts $ V.take 10 rows)
      -- putStrLn "sum of sample counts"
      -- print $ sumSampleCounts rows
      putStrLn "geneIds"
      print $ getTopGeneId rows
      print $ headerToValues header rows
      print $ sampleKeyToValues "GSM5011616" rows
      putStrLn "grab the row"
      print $ geneKeyToValues "100287102" rows
      putStrLn "gene ids"
      print $ getGeneIds rows

getTopGeneId :: V.Vector NcbiRow -> V.Vector String
getTopGeneId rows = V.map geneId $ V.take 1 rows

headerToValues :: Header -> V.Vector NcbiRow -> [V.Vector (Maybe BS8.ByteString)]
headerToValues header rows = map (\x -> V.map (HM.lookup x . values) rows) $ BS8.pack <$> fromHeader header

sampleKeyToValues :: BS8.ByteString -> V.Vector NcbiRow -> V.Vector (Maybe BS8.ByteString)
sampleKeyToValues key = V.map ((HM.lookup key) . values)

geneKeyToValues :: String -> V.Vector NcbiRow -> NcbiRow
geneKeyToValues key rows = V.head $ V.filter (\row -> geneId row == key) rows

getGeneIds :: V.Vector NcbiRow -> V.Vector String
getGeneIds rows = V.map geneId rows

data NcbiRow = NcbiFpkmRow
  { geneId :: !String
  , values :: HM.HashMap BS8.ByteString BS8.ByteString
  , source :: !String -- the source document probably as a hash
  }
  deriving (Generic, Show)

instance FromNamedRecord NcbiRow where
  parseNamedRecord :: NamedRecord -> Parser NcbiRow
  parseNamedRecord r = do
    let firstFieldKey = "GeneID"
        restHM = HM.delete firstFieldKey r
    geneIdVal <- r .: firstFieldKey
    return $ NcbiFpkmRow geneIdVal restHM ""

getIndex :: Eq a => [a] -> a -> Int -> Int
getIndex x y = until ((\x y i -> y == x !! i) x y) (0 +)

printHeader :: Header -> IO ()
printHeader header = do
  putStrLn $ (unwords . fromHeader) header

fromHeader :: Header -> [String]
fromHeader x = map BS8.unpack $ V.toList x

-- [ ] We need to know what the gene ids convert to
  
-- ! Goal is to set up monads so that we can apply a series of
-- ! transformations with bind.

data Count = Fpkm Float | Fpm Float | RawCount Int
  
-- Perhaps these should be defined in the terms of each other as can
-- be seen in (see below), but maybe just add a RawToFpkm type or
-- somesuch.
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html

-- !!!!!! We need to construct a convertFromTpm that can get
-- !!!!!! backwards, and it would be best to do this in such a way
-- !!!!!! where we know everything that we need to. The pullback
-- !!!!!! should be within the thing, so if the inputs include
-- !!!!!! geneLength and totalCounts, then first of all these should
-- !!!!!! be given type declations so that they are accessible to the
-- !!!!!! type checker, but they should also be retained if the
-- !!!!!! pullback should expect them
