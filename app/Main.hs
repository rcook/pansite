{-|
Module      : Main
Description : Main entrypoint for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Main (main) where

import           CommandLine
import           Scan

main :: IO ()
main = parseOptions >>= \(Options config) -> doScan config
