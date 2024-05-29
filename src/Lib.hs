{-# LANGUAGE DataKinds #-} -- TemplateHaskell, GADTs,

module Lib
    ( someFunc
    , quicksort
    , umap
    , umap'
    ) where

import Data.Csv
import Text.ParserCombinators.Parsec

-- data GeneExpressionData
--   = GeneCode String
--   | RelativeExpression Float
--   | SampleId String

data GeneCode = GeneCode String
  deriving (Show, Eq)

data SampleId = SampleId String
  deriving (Show, Eq)

data RelativeExpression = Float
  deriving (Show, Eq)

data GeneExpression = GeneExpression
    { genecode :: GeneCode
    , relative_expression :: RelativeExpression }
  deriving (Show, Eq)

data GeneExpressionSample = GeneExpressionSample
    { sample_id :: SampleId
    , expression_set :: [GeneExpression] }
  deriving (Show, Eq)

data DnaBase = Adenine | Thymine | Cytosine | Guanine
  deriving (Show, Eq, Enum)

data FastaSequence = FastaSequence
  { sequence_header :: (Maybe GeneCode, Maybe SampleId)
  , sequence_data :: [DnaBase] }
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a) }

-- parse_fasta_head :: String -> Parser String
-- parse_fasta_head x = Parser f
--   where
--     f (y:ys) 
--       | y == x = Just (y, ys)
--       | otherwise = Nothing
--     f [] = Nothing

someFunc :: IO ()
someFunc = putStrLn "someFunc"

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted  = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

umap :: [[a]] -> [(a, a)]
umap [] = []
umap (x:xs) = [(head x, head $ tail x)] ++ umap xs

y, z :: Int
y = 5
z = 9

umap' :: [(Int, Int)]
umap' = umap myvar
  where
    myvar = [[x..(x+3)] | x <- [y..z]]

csvFile :: GenParser Char st [[String]]
csvFile = 
  do result <- many line
    eof
    return result

line :: GenParser Char st [String]
line = 
  do result <- cells
    eol
    return result

cells :: GenParser Char st [String]
cells = 
  do first <- cellContent
    next <- remainingCells
    return (first : next)


