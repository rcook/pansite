{-|
Module      : Main
Description : Main entrypoint for Pansite doctests
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Main (main) where

import           System.FilePath.Glob
import           Test.DocTest

defaultIncludeDirs :: [FilePath]
defaultIncludeDirs = []

doctestWithIncludeDirs :: [FilePath] -> IO ()
doctestWithIncludeDirs fs = doctest (map ("-I" ++) defaultIncludeDirs ++ fs)

sourceDirs :: [FilePath]
sourceDirs = ["app", "src"]

main :: IO ()
main = mconcat (map (glob . (++ "/**/*.hs")) sourceDirs) >>= doctestWithIncludeDirs
