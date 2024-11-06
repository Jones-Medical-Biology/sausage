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

type Count = Double

importData :: FilePath -> IO ()
importData x = do
  tsvData <- BL.readFile x
  let decodeOptions = defaultDecodeOptions {
        decDelimiter = fromIntegral (ord '\t') }
      -- decoded :: Either String (V.Vector TsvRow)
      -- decoded = decodeWith decodeOptions HasHeader tsvData
      decoded :: Either String (Header, V.Vector TsvRow)
      decoded = decodeByNameWith decodeOptions tsvData
  case decoded of
    Left err -> putStrLn $ "Error parsing TSV: " ++ err
    -- Right rows -> do
    Right (header, rows) -> do
      printHeader header
      V.forM_ rows print

instance FromRecord TsvRow where
  parseRecord v
    | V.length v >= 2 = do
        firstColValue <- parseField (v V.! 0)
        sampleCountsValues <- mapM parseField (V.toList (V.tail v))
        return $ NcbiFpkmRow firstColValue sampleCountsValues
    | otherwise = fail "Insufficient columns in record"

-- instance FromNamedRecord TsvRow
-- instance ToNamedRecord TsvRow
-- instance DefaultOrdered TsvRow


instance FromNamedRecord TsvRow where
  parseNamedRecord :: NamedRecord -> Parser TsvRow
  parseNamedRecord r = do
    let fieldNames = HM.keys r
        firstFieldName = head fieldNames
        sampleFieldNames = drop 1 fieldNames
    firstColValue :: String <- r .: firstFieldName
    sampleCountsValues <- mapM (r .:) sampleFieldNames
    return $ NcbiFpkmRow firstColValue sampleCountsValues

-- instance FromNamedRecord TsvRow where
--   parseNamedRecord :: NamedRecord -> Parser TsvRow
--   parseNamedRecord r = do
--     let recordMap = V.toList r
--         headers = map fst recordMap
--         values = map snd recordMap
--     firstColValue <- parseField (values !! 0)
--     sampleCountsValues <- mapM parseField (drop 1 values)
--     return $ NcbiFpkmRow (toString firstColValue) sampleCountsValues
--       where toString = map (toEnum . fromEnum)

printHeader :: Header -> IO ()
printHeader header = do
  let headerFields = map BS8.unpack (V.toList header)
  putStrLn $ unwords headerFields

myOptions = defaultDecodeOptions {
  decDelimiter = fromIntegral (ord '\t')
  }
 
data TsvRow = NcbiFpkmRow
  { geneId :: !String
  , sampleCounts :: ![Count]
  } deriving (Generic, Show)

data RawCount
data FPKM
data TPM
data GeneName

data Expression a where
  Raw :: Int -> Expression RawCount
  Fpkm :: Double -> Expression FPKM
  Tpm :: Double -> Expression TPM
  Gene :: String -> Expression GeneName
  
-- Perhaps these should be defined in the terms of each other as can
-- be seen in (see below), but maybe just add a RawToFpkm type or
-- somesuch.
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html

addExpr :: Expression a -> Expression a -> Expression a
addExpr (Raw  x)   (Raw  y)   = Raw  (x + y)
addExpr (Fpkm x)   (Fpkm y)   = Fpkm (x + y)
addExpr (Tpm  x)   (Tpm  y)   = Tpm  (x + y)

convertToTpm :: Expression RawCount -> Double -> Double -> Expression TPM
convertToTpm (Raw count) geneLength totalCounts =
    let rpk        = fromIntegral count / (geneLength / 1e3)
        perMillion = totalCounts / 1e6
        tpmValue   = rpk / perMillion
    in Tpm tpmValue

-- !!!!!! We need to construct a convertFromTpm that can get
-- !!!!!! backwards, and it would be best to do this in such a way
-- !!!!!! where we know everything that we need to. The pullback
-- !!!!!! should be within the thing, so if the inputs include
-- !!!!!! geneLength and totalCounts, then first of all these should
-- !!!!!! be given type declations so that they are accessible to the
-- !!!!!! type checker, but they should also be retained if the
-- !!!!!! pullback should expect them

showExpression :: Expression a -> String
showExpression expr = case expr of
    Raw count     -> "Raw Count: " ++ show count
    Fpkm value    -> "FPKM: " ++ show value
    Tpm value     -> "TPM: " ++ show value

sumExpressions :: [Expression a] -> Expression a
sumExpressions = foldl1 addExpr

-- !!! We want to derive functor for these, and so
