{-|
Module      : Pansite.Config.Types
Description : Helper functions for Pansite application
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module PansiteApp.Util
    ( countOccurrences
    , readFileUtf8
    , readFileWithEncoding
    , skipDirectory
    , stems
    , writeFileUtf8
    , writeFileWithEncoding
    ) where

import           Data.List.Split
import           System.FilePath
import           System.IO

-- | Number of occurrences of a substring in a string
--
-- Examples:
--
-- >>> countOccurrences "" "abc"
-- 0
-- >>> countOccurrences "" ""
-- 0
-- >>> countOccurrences "abcdefghi" "xyz"
-- 0
-- >>> countOccurrences "abc" "abc"
-- 1
-- >>> countOccurrences "abcabc" "abc"
-- 2
-- >>> countOccurrences "abcdefabcghiabcjkl" "abc"
-- 3
countOccurrences :: String -> String -> Int
countOccurrences str substr = length (splitOn substr str) - 1

-- | Compute common prefix of two lists
--
-- Examples:
--
-- >>> commonPrefix "abcdefghi" "abcdefghi"
-- "abcdefghi"
-- >>> commonPrefix "abcdefghi" "abcfoo"
-- "abc"
-- >>> commonPrefix "abc" "xyz"
-- ""
commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x : xs) (y : ys)
    | x == y = x : commonPrefix xs ys
    | otherwise = []

-- | Compute common suffix of two lists
--
-- Examples:
--
-- >>> commonSuffix "abcdefghi" "abcdefghi"
-- "abcdefghi"
-- >>> commonSuffix "abcdefghi" "fooghi"
-- "ghi"
-- >>> commonSuffix "abc" "xyz"
-- ""
commonSuffix :: (Eq a) => [a] -> [a] -> [a]
commonSuffix xs ys = reverse (commonPrefix (reverse xs) (reverse ys))

-- | Slice of list
--
-- Examples:
--
-- >>> slice 4 10 "helloworldgoodbye"
-- "oworld"
slice :: Int -> Int -> [a] -> [a]
slice p0 p1 xs = take (p1 - p0) (drop p0 xs)

-- | Stems of lists
--
-- Examples:
--
-- >>> stems "abcmiddledef" "abcfoodef"
-- ("middle","foo")
-- >>> stems "abc" "xyz"
-- ("abc","xyz")
stems :: (Eq a) => [a] -> [a] -> ([a], [a])
stems xs ys =
    let prefix = commonPrefix xs ys
        prefixCount = length prefix
        suffix = commonSuffix xs ys
        suffixCount = length suffix
        xsCount = length xs
        ysCount = length ys
    in (slice prefixCount (xsCount - suffixCount) xs, slice prefixCount (ysCount - suffixCount) ys)

readFileWithEncoding :: TextEncoding -> FilePath -> IO String
readFileWithEncoding encoding path = do
    h <- openFile path ReadMode
    hSetEncoding h encoding
    hGetContents h

readFileUtf8 :: FilePath -> IO String
readFileUtf8 = readFileWithEncoding utf8

writeFileWithEncoding :: TextEncoding -> FilePath -> String -> IO ()
writeFileWithEncoding encoding path content =
    withFile path WriteMode $ \h -> do
        hSetEncoding h encoding
        hPutStr h content

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 = writeFileWithEncoding utf8

skipDirectory :: FilePath -> FilePath
skipDirectory p = let d = takeDirectory p in drop (length d + 1) p
