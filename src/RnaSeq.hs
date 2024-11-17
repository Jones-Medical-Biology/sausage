{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RnaSeq (importData) where

import Data.Text (Text)
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
                , Header(..))
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS8

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
      putStrLn "fpkm of top 10 rows"
      print $ V.foldl1 (zipWith (+)) (V.map counts $ V.take 10 rows)
      putStrLn "sum of sample counts"
      print $ sumSampleCounts rows

sumSampleCounts :: V.Vector NcbiRow -> [Float]
sumSampleCounts rows = V.foldl1 (zipWith (+)) (V.map counts rows)

instance FromRecord NcbiRow where
  parseRecord v
    | V.length v >= 2 = do
        firstColValue <- parseField (v V.! 0)
        fpkmValues <- mapM parseField (V.toList (V.tail v))
        return $ NcbiFpkmRow firstColValue fpkmValues
    | otherwise = fail "Insufficient columns in record"

instance FromNamedRecord NcbiRow where
  parseNamedRecord :: NamedRecord -> Parser NcbiRow
  parseNamedRecord r = do
    let fieldNames = HM.keys r
        firstFieldName = head fieldNames
        sampleFieldNames = drop 1 fieldNames
    firstColValue :: String <- r .: firstFieldName
    fpkmValues <- mapM (r .:) sampleFieldNames
    return $ NcbiFpkmRow firstColValue fpkmValues

printHeader :: Header -> IO ()
printHeader header = do
  let headerFields = map BS8.unpack (V.toList header)
  putStrLn $ unwords headerFields
  
data NcbiRow = NcbiFpkmRow
  { geneId :: !String
  , counts :: ![Float]
  , source :: !String -- the source document probably as a hash
  }
  deriving (Generic, Show)

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
