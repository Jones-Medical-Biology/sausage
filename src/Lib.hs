{-# LANGUAGE DataKinds #-} -- TemplateHaskell, GADTs,
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    , quicksort
    , umap
    , umap'
    , csvFile
    ) where

import Data.Csv
import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT, parse)
import Numeric ( readHex
               , readSigned
               , readFloat
               )

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
  deriving (Read, Show, Eq, Enum, Ord)

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
-- I am proud of this function
composeParseT :: [ParsecT s u m Char] -> ParsecT s u m Char
composeParseT [] = error "empty list"
composeParseT [x] = x
composeParseT (x:xs) = x <|> (composeParseT xs)

-- https://book.realworldhaskell.org/read/using-parsec.html
csvFile :: GenParser Char st [[String]]
csvFile = do result <- many line
             eof
             return result

line :: GenParser Char st [String]
line = do result <- cells
          eol
          return result

cells :: GenParser Char st [String]
cells = do first <- cell
           next <- remainingCells
           return (first : next) 

cell = quotedCell <|> many (noneOf ",\n\r")
quotedCell = do char '"'
                content <- many quotedChar
                char '"' <?> "quote at end of cell"
                return content

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells) <|> (return [])

eol = try (string "\n\r") 
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

-- data JValue = JString String
--   | JNumber Double
--   | JBool Bool
--   | JNull
--   | JObject (JObj JValue)
--   | JArray (JAry JValue)
--   deriving (Eq, Ord, Show)

-- newtype JAry a = JAry {
--   fromJAry :: [a]
--   } deriving (Eq, Ord, Show)

-- newtype JObj a = JObj {
--   fromJObj :: [(String, a)]
--   } deriving (Eq, Ord, Show) 

-- p_text :: CharParser () JValue
-- p_text = spaces *> text
--          <?> "JSON text"
--          where text = JObject <$> p_object
--                       <|> JArray <$> p_array

-- p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
-- p_series left parser right = 
--   between (char left <* spaces) (char right)
--     $ (parser <* spaces) `sepBy` (char ',' <* spaces)


-- p_array :: CharParser () (JAry JValue)
-- p_array = JAry <$> p_series '[' p_value ']'

-- p_object :: CharParser () (JObj JValue)
-- p_object = JObj <$> p_series '{' p_field '}'
--     where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value

-- p_value :: CharParser () JValue
-- p_value = value <* spaces
--   where value = JString <$> p_string
--             <|> JNumber <$> p_number
--             <|> JObject <$> p_object
--             <|> JArray <$> p_array
--             <|> JBool <$> p_bool
--             <|> JNull <$ string "null"
--             <?> "JSON value"

-- p_bool :: CharParser () Bool
-- p_bool = True <$ string "true"
--      <|> False <$ string "false"

-- p_value_choice = value <* spaces
--   where value = choice [ JString <$> p_string
--                        , JNumber <$> p_number
--                        , JObject <$> p_object
--                        , JArray <$> p_array
--                        , JBool <$> p_bool
--                        , JNull <$ string "null"
--                        ]
--                 <?> "JSON value"

-- p_number :: CharParser () Double
-- p_number = do s <- getInput
--               case readSigned readFloat s of
--                 [(n, s')] -> n <$ setInput s'
--                 _         -> empty

-- p_string :: CharParser () String
-- p_string = between (char '\"') (char '\"') (many jchar)
--     where jchar = char '\\' *> (p_escape <|> p_unicode)
--               <|> satisfy (`notElem` "\"\\")

-- p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
--     where decode c r = r <$ char c

-- p_unicode :: CharParser () Char
-- p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
--     where decode x = toEnum code
--               where ((code,_):_) = readHex x
