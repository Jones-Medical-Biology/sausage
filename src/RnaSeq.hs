{-# LANGUAGE GADTs #-}
{-# TemplateHaskell #-}

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
