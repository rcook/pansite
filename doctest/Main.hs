{-|
Module      : Main
Description : Main entrypoint for Pansite doctests
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE CPP #-}

module Main (main) where

import           System.FilePath.Glob
import           Test.DocTest

compilerOpts :: [String]
compilerOpts =
#if defined(OS_LINUX)
    [ "-DOS_LINUX"
#elif defined(OS_MACOS)
    [ "-DOS_MACOS"
#elif defined(OS_WINDOWS)
    [ "-DOS_WINDOWS"
#else
#error Unsupported platform
#endif
    ]

defaultIncludeDirs :: [FilePath]
defaultIncludeDirs = []

doctestWithIncludeDirs :: [FilePath] -> IO ()
doctestWithIncludeDirs fs = doctest (map ("-I" ++) defaultIncludeDirs ++ fs ++ compilerOpts)

sourceDirs :: [FilePath]
sourceDirs = ["app", "src"]

main :: IO ()
main = mconcat (map (glob . (++ "/**/*.hs")) sourceDirs) >>= doctestWithIncludeDirs
