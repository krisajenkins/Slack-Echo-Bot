{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text
import           Lib

apiKey :: Text
apiKey = "???"

main :: IO ()
main = runBot apiKey
