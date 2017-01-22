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

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time
import           Data.Yaml
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Pansite
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

data SiteInfo = SiteInfo FilePath FilePath

data ConfigInfo = ConfigInfo FilePath UTCTime Config deriving Show

mkSiteInfo :: FilePath -> FilePath -> IO SiteInfo
mkSiteInfo siteDir outputDir = do
    canonicalSiteDir <- canonicalizePath siteDir
    canonicalOutputDir <- canonicalizePath outputDir
    return $ SiteInfo canonicalSiteDir canonicalOutputDir

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

siteDir :: FilePath
siteDir = "_site"

outputDir :: FilePath
outputDir = "_output"

routesYamlFileName :: FilePath
routesYamlFileName = "routes.yaml"

rebuildRouteSourcePath :: SiteInfo -> FilePath -> IO FilePath
rebuildRouteSourcePath (SiteInfo siteDir cacheDir) sourcePath = do
    let outputPath = cacheDir </> sourcePath
    callProcess "make"
        [ "OUTPUT_DIR=" ++ cacheDir
        , "-C"
        , siteDir
        , outputPath
        ]
    return outputPath

readRouteSourcePath :: FilePath -> IO String
readRouteSourcePath sourcePath = do
    siteInfo <- mkSiteInfo siteDir outputDir
    outputPath <- rebuildRouteSourcePath siteInfo sourcePath
    putStrLn $ "Try to read " ++ outputPath
    readFile outputPath

--doScan :: IO ()
--doScan = canonicalizePath "_site/routes.yaml" >>= readConfigInfo >>= showConfigInfoLoop

doScan :: IO ()
doScan = withStdoutLogger $ \logger -> do
    routesYamlPath <- canonicalizePath $ siteDir </> routesYamlFileName
    configInfo <- readConfigInfo routesYamlPath
    blah logger configInfo

blah :: ApacheLogger -> ConfigInfo -> IO ()
blah logger (ConfigInfo _ _ (Config routes _)) = do
    let m = Map.fromList (map (\(Route paths sourcePath) -> (map Text.pack paths, Text.pack sourcePath)) routes)
        port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port (app logger m)

app :: ApacheLogger -> Map [Text] Text -> Application
app logger m req f =
    case Map.lookup (pathInfo req) m of
        Just sourcePath -> do
            liftIO $ logger req status200 (Just 0)

            -- TODO: Fix all text re-encoding etc.
            content <- Text.pack <$> readRouteSourcePath (Text.unpack sourcePath)

            f $ responseLBS status200 [(hContentType, "text/plain")] (BL.fromStrict $ Text.encodeUtf8 content)
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"

{-
render :: [String] -> ConfigInfo -> IO ()
render paths (ConfigInfo _ _ (Config routes _)) = do
    let m = M.fromList (map (\(Route paths sourcePath) -> (paths, sourcePath)) routes)
    case M.lookup paths m of
        Just sourcePath -> putStrLn $ "sourcePath=" ++ sourcePath
        Nothing -> error "Route not supported"
    putStrLn "Done"
-}

{-
doScan :: IO ()
doScan = do
    let g = mkGraph
                [ Edge "5" "2"
                , Edge "5" "0"
                , Edge "4" "0"
                , Edge "4" "1"
                , Edge "2" "3"
                , Edge "3" "1"
                ]
    print g
    print $ topoSort g
-}
