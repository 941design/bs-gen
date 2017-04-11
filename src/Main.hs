{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (Snap, Method(GET), route, method, writeBS)
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import Test.QuickCheck.Gen (generate)
import Testimonial (testimonial)


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
  phrase <- liftIO $ generate testimonial
  writeBS . pack $ phrase
