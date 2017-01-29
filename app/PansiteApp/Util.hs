module PansiteApp.Util
    ( readFileUtf8
    , readFileWithEncoding
    , writeFileUtf8
    , writeFileWithEncoding
    ) where

import           System.IO

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
