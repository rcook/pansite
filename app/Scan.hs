{-|
Module      : Scan
Description : Scanning for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Scan (doScan) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Data.Yaml
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Pansite
import           System.Directory
import           System.IO

data ConfigInfo = ConfigInfo FilePath UTCTime Config deriving Show

readConfigInfo :: FilePath -> IO ConfigInfo
readConfigInfo path = do
    t <- getModificationTime path
    s <- C8.readFile path
    let Just config = decode s -- TODO: Irrefutable pattern
    return $ ConfigInfo path t config

readChar :: IO Char
readChar = bracketHandle stdin $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    c <- hGetChar stdin
    return c

showConfigInfoLoop :: ConfigInfo -> IO ()
showConfigInfoLoop configInfo@(ConfigInfo path t _) = do
    print configInfo
    putStr "(Q)uit or (S)can: "
    hFlush stdout
    c <- toUpper <$> readChar
    putStrLn ""
    if c == 'S'
        then do
            putStrLn "(Scan)"
            t' <- getModificationTime path
            case t' `compare` t of
                EQ -> putStrLn "(Unchanged)" >> showConfigInfoLoop configInfo
                _ -> putStrLn "(Changed)" >> readConfigInfo path >>= showConfigInfoLoop
        else (putStrLn "(Quit)")

--doScan :: IO ()
--doScan = canonicalizePath "routes.yaml" >>= readConfigInfo >>= showConfigInfoLoop

doScan :: IO ()
doScan = canonicalizePath "routes.yaml" >>= readConfigInfo >>= blah

blah :: ConfigInfo -> IO ()
blah (ConfigInfo _ _ (Config routes _)) = do
    let m = Map.fromList (map (\(Route paths sourcePath) -> (map Text.pack paths, Text.pack sourcePath)) routes)
        port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port (app m)

app :: Map [Text] Text -> Application
app m req f =
    case Map.lookup (pathInfo req) m of
        Just _ -> f $ responseLBS status200 [(hContentType, "text/plain")] "Found route"
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"
