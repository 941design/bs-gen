{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (Snap, Method(GET), route, method, writeBS, liftSnap)
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, unpack) -- readFile, split, lines
import Data.Maybe (fromJust)
import Data.List (transpose, intercalate)
import System.Random
import Text.ParserCombinators.Parsec


-- | Path to file containing the bullshit
bs_file = "./dist/resources/bs.csv"


-- | The main function
main :: IO ()
main = do
  putStrLn "resource path: /"
  httpServe (setPort 8181 defaultConfig) site


-- | Main entry point, defining all routes
site :: Snap ()
site = route [ ("", method GET $ bsHandler) ]


-- | Handler for producing the bullshit response
bsHandler :: Snap ()
bsHandler = do
  phrase <- liftIO $ readFile bs_file >>= \raw ->
    case readCSV raw of
      Left err -> undefined
      Right table -> randomBS . map (filter (not . null)) $ transpose table
  writeBS . pack $ phrase


-- | Picks random bullshit from buzzword table
randomBS :: [[String]] -> IO String
randomBS xs = do
  buzzwords <- mapM randomX xs
  return (intercalate " " buzzwords)


-- | Reads csv to table
readCSV :: String -> Either ParseError [[String]]
readCSV s = parse lines "NAME IGNORED" s
  where
    lines = sepBy cells newline
    cells = sepBy (many . noneOf $ ",\n") (char ',')


-- | Picks random value from list
randomX :: [a] -> IO a
randomX xs = do
  idx <- getStdRandom (randomR (start, end))
  return (xs!!idx)
  where start = 0
        end = (length xs) - 1
