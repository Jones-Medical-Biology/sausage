module Main (main) where

import Lib

main :: IO ()
main = do c <- getContents
          case parse csvFile "(stdin)" c of
               Left e -> do putStrLn "Error parsing input:"
                            print e
               Right r -> mapM_ print r
