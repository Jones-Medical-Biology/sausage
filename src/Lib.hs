module Lib
    ( someFunc
    , quicksort
    , umap
    , umap'
    ) where

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
