{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (Snap, Method(GET), route, method, writeBS, liftSnap)
import Snap.Http.Server (quickHttpServe)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, unpack) -- readFile, split, lines
import Data.Maybe (fromJust)
import Data.List (transpose, intercalate)
import Data.List.Split (splitOn)
import System.Random


-- | Path to file containing the bullshit
bs_file = "./dist/resources/bs.csv"


-- | The main function
main :: IO ()
main = do
  putStrLn "resource path: /generate-bs"
  quickHttpServe site


-- | Main entry point, defining all routes
site :: Snap ()
site = route [ ("generate-bs", method GET $ bsHandler) ]


-- | Handler for producing the bullshit response
bsHandler :: Snap ()
bsHandler = do
  content <- liftIO . readFile $ bs_file
  let table = map filterFromJust $ transpose . csvToTable $ content
  phrase <- liftIO . randomBS $ table
  writeBS . pack $ phrase


-- | Picks random bullshit from buzzword table
randomBS :: [[String]] -> IO String
randomBS xs = do
  buzzwords <- mapM randomX xs
  return (intercalate " " buzzwords)


-- | Reads csv to table
csvToTable :: String -> [[Maybe String]]
csvToTable = map (map maybeString . (splitOn ",")) . lines
    where maybeString "" = Nothing
          maybeString s = Just s


-- | Filters values / drops Nothings
filterFromJust :: [Maybe a] -> [a]
filterFromJust = map fromJust . filter onlyValues
  where onlyValues = maybe False (const True)


-- | Picks random value from list
randomX :: [a] -> IO a
randomX xs = do
  idx <- getStdRandom (randomR (start, end))
  return (xs!!idx)
  where start = 0
        end = (length xs) - 1
