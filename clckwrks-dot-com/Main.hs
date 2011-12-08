{-# LANGUAGE RecordWildCards #-}
module Main where

import Clckwrks
import ClckwrksServer

data SiteURL = C ClckURL

main :: IO ()
main = 
  let c = defaultClckwrksConfig  { clckURL = C }
  in simpleClckwrks c
