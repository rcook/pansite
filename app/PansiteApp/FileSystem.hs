{-|
Module      : PansiteApp.FileSystem
Description : File system functions
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE CPP #-}

module PansiteApp.FileSystem (stemPaths) where

import           Development.Shake.FilePath
import           PansiteApp.Util

#if defined(OS_MACOS) || defined(OS_LINUX)
-- TODO: Enforce the wildcard token via the types!
-- | Find stem of path and rule
--
-- Examples:
--
-- >>> stemPaths "%" "C:/aaa/bbb/stem.txt" "C:/aaa/bbb/%.txt"
-- Right "stem"
#elif defined(OS_WINDOWS)
-- TODO: Enforce the wildcard token via the types!
-- | Find stem of path and rule
--
-- Examples:
--
-- >>> stemPaths "%" "C:\\aaa\\bbb\\stem.txt" "C:/aaa/bbb/%.txt"
-- Right "stem"
-- >>> stemPaths "%" "C:/aaa/bbb/stem.txt" "C:\\aaa\\bbb\\%.txt"
-- Right "stem"
#else
#error Unsupported platform
#endif
stemPaths :: String -> FilePath -> String -> Either String String
stemPaths expectedToken path rule =
    let path' = toNative path
        rule' = toNative rule
        (stem, token) = stems path' rule'
    in if token == expectedToken
        then Right stem
        else Left $ "Unexpected wildcard token: " ++ token
