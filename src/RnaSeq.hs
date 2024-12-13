{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DefaultSignatures #-}

module RnaSeq ( importData
              , processTFPathways) where

import System.Environment ( getArgs )
import Data.Text (Text)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Data.Csv ( FromNamedRecord(..)
                , ToNamedRecord(..)
                , namedRecord
                , DefaultOrdered(..)
                , decodeByName
                , decodeByNameWith
                , encodeByName
                , encodeDefaultOrderedByName
                , (.:)
                , (.=)
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
import Data.List (isInfixOf, nub)
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
import Data.Maybe (fromMaybe)
import Text.Parsec.Prim ( Stream(..)
                        , ParsecT(..) )
import Text.Parsec.Combinator ( choice )

importData :: String -> FilePath -> FilePath -> IO ()
importData genename file1 file2 = do
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
      -- printHeader header
      -- printHeader metaHeader
      -- print $ getTopGeneId rows
      let floated = V.map values $ rows
      let sums = sumSamples floated
      -- print $ V.map (HM.unionWith (/) sums) floated
      let entryMatch = V.filter (\x -> HM.lookup "Symbol" (metaValues x) == Just (BS8.pack genename)) metaRows
      let matchedGeneIds = V.map metaGeneId entryMatch
      let matchedEntry =  V.filter (\x -> any (geneId x ==) matchedGeneIds) rows
      let namedRecordsMatchedEntry = V.map toNamedRecord matchedEntry
      print $ encodeByName header $ V.toList namedRecordsMatchedEntry

floatIt :: HM.HashMap BS8.ByteString BS8.ByteString -> HM.HashMap BS8.ByteString Float
floatIt inputMap = HM.mapMaybe parseFloat inputMap
  where
    parseFloat :: BS8.ByteString -> Maybe Float
    parseFloat bs = case reads (BS8.unpack bs) of
                      [(x, "")] -> Just x
                      _         -> Nothing

sumSamples :: V.Vector (HM.HashMap BS8.ByteString Float) -> HM.HashMap BS8.ByteString Float
sumSamples = V.foldl' (HM.unionWith (+)) HM.empty

getTopGeneId :: V.Vector NcbiRow -> V.Vector String
getTopGeneId rows = V.map geneId $ V.take 1 rows

headerToValues :: Header -> V.Vector NcbiRow -> [V.Vector (Maybe Float)]
headerToValues header rows = map (\x -> V.map (HM.lookup x . values) rows)
  $ BS8.pack <$> fromHeader header

sampleKeyToValues :: BS8.ByteString -> V.Vector NcbiRow -> V.Vector (Maybe Float)
sampleKeyToValues key = V.map ((HM.lookup key) . values)

geneKeyToValues :: String -> V.Vector NcbiRow -> NcbiRow
geneKeyToValues key rows = V.head $ V.filter (\row -> geneId row == key) rows

getGeneIds :: V.Vector NcbiRow -> V.Vector String
getGeneIds = V.map geneId

data NcbiRow = NcbiFpkmRow
  { geneId :: !String
  , values :: HM.HashMap BS8.ByteString Float
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
        restHM = floatIt $ HM.delete firstFieldKey r
    geneIdVal <- r .: firstFieldKey
    return $ NcbiFpkmRow geneIdVal restHM ""

instance ToNamedRecord NcbiRow where
  toNamedRecord (NcbiFpkmRow geneIdVal restHM "") =
    namedRecord [ geneIdRelation ] <> namedRecord relation
    where
      weirdFun = (\(key, value) -> key .= value)
      relation = map weirdFun (HM.toList restHM)
      geneIdRelation = ("GeneID" .= geneIdVal)


instance FromNamedRecord MetadataRow where
  parseNamedRecord :: NamedRecord -> Parser MetadataRow
  parseNamedRecord r = do
    let firstFieldKey = "GeneID"
        restHM = HM.delete firstFieldKey r
        parseFloat :: BS8.ByteString -> Maybe Float
        parseFloat bs = case reads (BS8.unpack bs) of
                          [(x, "")] -> Just x
                          _         -> Nothing
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

data NucleicAcidBase = DnaBase | RnaBase
  deriving (Show, Eq)

data DnaBase = DA | DT | DC | DG
  deriving (Show, Eq, Generic)
  -- deriving Codonable

data RnaBase = A | U | C | G
  deriving (Show, Eq, Generic)
  -- deriving Codonable

class Complement a where
  complement :: a -> a

-- class Codonable a where
--   default codons :: [a] -> [(a,a,a)]
--   codons = codonsGeneric

-- codonsGeneric :: [a] -> [(a,a,a)]
-- codonsGeneric (a:b:c:xs) = (a,b,c) : codonsGeneric xs
-- codonsGeneric _ = []

-- class Readingframe a where
--   readingframe :: 

instance Complement DnaBase where
    complement :: DnaBase -> DnaBase
    complement DA = DT
    complement DT = DA
    complement DC = DG
    complement DG = DC

instance Complement RnaBase where
    complement :: RnaBase -> RnaBase
    complement A = U
    complement U = A
    complement C = G
    complement G = C

class Transcribe a b where
    transcribe :: a -> b

instance Transcribe DnaBase RnaBase where
    transcribe DA = A
    transcribe DT = U
    transcribe DC = C
    transcribe DG = G

type DnaSequence = [DnaBase]
type RnaSequence = [RnaBase]

newtype Quality = Quality { unQuality :: Int }
    deriving (Show, Eq)

data FastqBase = FastqBase 
    { baseValue :: NucleicAcidBase
    , quality :: Quality }
    deriving (Show, Eq)

-- [ ] We need some rules here about parsing

-- [ ] We need to know what the gene ids convert to
  
-- ! Goal is to set up monads so that we can apply a series of
-- ! transformations with bind.

data Count = Fpkm Float| Fpm Float | RawCount Int
  
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
