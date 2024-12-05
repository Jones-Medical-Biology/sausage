{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}

module RnaSeq (importData
              ,processTFPathways) where

import System.Environment ( getArgs )
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
import Text.Parsec ( alphaNum
                   , char
                   , string
                   , sepBy
                   , many
                   , parse
                   , oneOf )
import Text.Parsec.Prim ( Stream(..)
                        , ParsecT(..) )
import Text.Parsec.Combinator ( choice )

importData :: FilePath -> FilePath -> IO ()
importData file1 file2 = do
  tsvData <- BL.readFile file1
  metaData <- BL.readFile file2
  let decodeOptions = defaultDecodeOptions {
        decDelimiter = fromIntegral (ord '\t') }
      decodeData :: (Either String (Header, V.Vector NcbiRow))
      decodeData = decodeByNameWith decodeOptions tsvData
      decodeMeta :: (Either String (Header, V.Vector MetadataRow))
      decodeMeta = decodeByNameWith decodeOptions metaData
  case (decodeData, decodeMeta) of
    (Left err, _) -> putStrLn $ "Error parsing dataTSV: " ++ err
    (_, Left err) -> putStrLn $ "Error parsing metaTSV: " ++ err
    (Right (header, rows), Right (metaHeader, metaRows)) -> do
      printHeader header
      printHeader metaHeader
      let a = V.head metaRows
      let b = metaGeneId a
      let c = metaValues a
      let d = HM.filterWithKey (\key value -> any ((key ==) . BS8.pack) (fromHeader header) && BS8.isInfixOf "KIAA0319L" value) c
      let e = geneKeyToValues "79932" rows
      let f = geneKeyToValues "9856" rows
      print $ HM.keys $ values e
      print $ map snd $ HM.toList $ values e
      print $ map snd $ HM.toList $ values f
      -- print $ HM.head $ geneId $ V.take 1 rows
      -- putStrLn "fpkm of top 10 rows"
      -- print $ V.foldl1 (zipWith (+)) (V.map counts $ V.take 10 rows)
      -- putStrLn "sum of sample counts"
      -- print $ sumSampleCounts rows
      -- print $ headerToValues header rows
      -- print $ sampleKeyToValues "GSM5011616" rows
      -- putStrLn "grab the row"
      -- print $ geneKeyToValues "100287102" rows
      -- putStrLn "gene ids"
      -- print $ getGeneIds rows

getTopGeneId :: V.Vector NcbiRow -> V.Vector String
getTopGeneId rows = V.map geneId $ V.take 1 rows
headerToValues :: Header -> V.Vector NcbiRow -> [V.Vector (Maybe BS8.ByteString)]
headerToValues header rows = map (\x -> V.map (HM.lookup x . values) rows)
  $ BS8.pack <$> fromHeader header
sampleKeyToValues :: BS8.ByteString -> V.Vector NcbiRow -> V.Vector (Maybe BS8.ByteString)
sampleKeyToValues key = V.map ((HM.lookup key) . values)
geneKeyToValues :: String -> V.Vector NcbiRow -> NcbiRow
geneKeyToValues key rows = V.head $ V.filter (\row -> geneId row == key) rows
getGeneIds :: V.Vector NcbiRow -> V.Vector String
getGeneIds = V.map geneId

data NcbiRow = NcbiFpkmRow
  { geneId :: !String
  , values :: HM.HashMap BS8.ByteString BS8.ByteString
  , source :: !String -- the source document probably as a hash
  } deriving (Generic, Show)

data MetadataRow = MetadataRow
  { metaGeneId :: !String
  , metaValues :: HM.HashMap BS8.ByteString BS8.ByteString
  , metaSource :: !String -- the source document probably as a hash
  }
  deriving (Generic, Show)

instance FromNamedRecord NcbiRow where
  parseNamedRecord :: NamedRecord -> Parser NcbiRow
  parseNamedRecord r = do
    let firstFieldKey = "GeneID"
        restHM = HM.delete firstFieldKey r
    geneIdVal <- r .: firstFieldKey
    return $ NcbiFpkmRow geneIdVal restHM ""

instance FromNamedRecord MetadataRow where
  parseNamedRecord :: NamedRecord -> Parser MetadataRow
  parseNamedRecord r = do
    let firstFieldKey = "GeneID"
        restHM = HM.delete firstFieldKey r
    geneIdVal <- r .: firstFieldKey
    return $ MetadataRow geneIdVal restHM ""

getIndex :: Eq a => [a] -> a -> Int -> Int
getIndex x y = until ((\x y i -> y == x !! i) x y) (0 +)

printHeader :: Header -> IO ()
printHeader header = do
  putStrLn $ (unwords . fromHeader) header

fromHeader :: Header -> [String]
fromHeader x = map BS8.unpack $ V.toList x

strings :: Stream s m Char =>  String -> ParsecT s u m String
strings = choice . map (string . pure)

processTFPathways :: [String] -> IO ()
processTFPathways thing = do
  let z = thing
  if length z == 2
  then do
    -- mapM_ putStrLn z
    a <- readFile $ (head . tail) z
    case parse ((many (alphaNum <|> char ' ') `sepBy` (string ", " <|> strings ":,;")) `sepBy` char '\t') "input" a of
      Left err -> print err
      Right result -> print result
  else
    putStrLn "nothing"

-- [ ] We need some rules here about parsing

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
