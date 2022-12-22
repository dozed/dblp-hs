module Main (main) where

import Control.Monad (forM_)
  
import Lib

main :: IO ()
main = do
  coAuthorInfo <- fetchCoAuthorInfo $ UrlPt "m/McCallum:Andrew"
  
  case coAuthorInfo of
    Left e -> fail $ show e
    Right info -> do
      putStrLn "Person:"
      print (person info)
      putStrLn ""
      putStrLn "CoAuthors:"
      forM_ (coAuthors info) $ \ca -> 
        putStrLn $ "- " <> show ca
