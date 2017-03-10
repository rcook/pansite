{-|
Module      : Pansite.PathPattern
Description : GNU Make-style path patterns
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE CPP #-}

module Pansite.PathPattern
    ( PathPattern
    , pathPattern
    , pathPatternStem
    , substituteStem
    , (%%>>)
    ) where

import           Data.String.Utils
import           Development.Shake
import           Development.Shake.FilePath
import           Pansite.Util

newtype PathPattern = PathPattern String deriving Show

makeToken :: String
makeToken = "%"

shakeToken :: String
shakeToken = "*"

pathPattern :: String -> Either String PathPattern
pathPattern s
    | let n = countOccurrences s makeToken in n == 0 || n == 1 = Right $ PathPattern s
    | otherwise = Left $ "Invalid path pattern " ++ s

substituteStem :: String -> PathPattern -> FilePath
substituteStem stem (PathPattern s) = replace makeToken stem s

(%%>>) :: PathPattern -> (FilePath -> Action ()) -> Rules ()
(%%>>) (PathPattern s) b = replace makeToken shakeToken s %> b
infix 1 %%>>

#if defined(OS_MACOS) || defined(OS_LINUX)
-- | Stem of pattern and path
--
-- Examples:
--
-- >>> Right p = pathPattern "C:/aaa/bbb/%.txt"
-- >>> pathPatternStem p "C:/aaa/bbb/stem.txt"
-- "stem"
#elif defined(OS_WINDOWS)
-- TODO: Enforce the wildcard token via the types!
-- | Find stem of path and rule
--
-- Examples:
--
-- >>> Right p0 = pathPattern "C:/aaa/bbb/%.txt"
-- >>> pathPatternStem p0 "C:\\aaa\\bbb\\stem.txt"
-- "stem"
-- >>> Right p1 = pathPattern "C:\\aaa\\bbb\\%.txt"
-- >>> pathPatternStem p1 "C:/aaa/bbb/stem.txt"
-- "stem"
#else
#error Unsupported platform
#endif
pathPatternStem :: PathPattern -> FilePath -> String
pathPatternStem (PathPattern s) path = let (stem, _) = stems (toNative path) (toNative s) in stem
