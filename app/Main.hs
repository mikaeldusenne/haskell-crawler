module Main where


import Scrapper

-- import PP101
import Hafez


main :: IO ()
main = do
  cfg <- mkcfg
  mainLoop cfg fs
