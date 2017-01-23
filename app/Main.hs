{-|
Module      : Main
Description : Main entrypoint for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Build
import           CommandLine
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
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
import           SiteConfig
import           System.Directory
import           System.FilePath
import           System.Process

data ConfigInfo = ConfigInfo FilePath UTCTime Config deriving Show

readConfigInfo :: FilePath -> IO ConfigInfo
readConfigInfo path = do
    t <- getModificationTime path
    s <- C8.readFile path
    let Just config = decode s -- TODO: Irrefutable pattern
    return $ ConfigInfo path t config

rebuildRouteSourcePath :: SiteConfig -> FilePath -> IO FilePath
rebuildRouteSourcePath siteConfig sourcePath = do
    let outputPath = (outputDir siteConfig) </> sourcePath
    callProcess "make"
        [ "OUTPUT_DIR=" ++ (outputDir siteConfig)
        , "-C"
        , siteDir siteConfig
        , outputPath
        ]
    return outputPath

readRouteSourcePath :: SiteConfig -> FilePath -> IO String
readRouteSourcePath siteConfig sourcePath = do
    outputPath <- rebuildRouteSourcePath siteConfig sourcePath
    putStrLn $ "Try to read " ++ outputPath
    readFile outputPath

doScan :: ServerConfig -> IO ()
doScan serverConfig = withStdoutLogger $ \logger -> do
    siteConfig <- mkSiteConfig "_site" "_output"
    configInfo <- readConfigInfo (routesYamlPath siteConfig)
    doIt serverConfig siteConfig logger configInfo

doIt :: ServerConfig -> SiteConfig -> ApacheLogger -> ConfigInfo -> IO ()
doIt (ServerConfig port) siteConfig logger (ConfigInfo _ _ (Config routes _)) = do
    let m = Map.fromList (map (\(Route paths sourcePath) -> (map Text.pack paths, Text.pack sourcePath)) routes)
    putStrLn $ "Listening on port " ++ show port
    run port (app siteConfig logger m)

app :: SiteConfig -> ApacheLogger -> Map [Text] Text -> Application
app siteConfig logger m req f =
    case Map.lookup (pathInfo req) m of
        Just sourcePath -> do
            liftIO $ logger req status200 (Just 0)

            -- TODO: Fix all text re-encoding etc.
            content <- Text.pack <$> readRouteSourcePath siteConfig (Text.unpack sourcePath)

            f $ responseLBS status200 [(hContentType, "text/html")] (BL.fromStrict $ Text.encodeUtf8 content)
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"

--main :: IO ()
--main = parseOptions >>=
--    \(Options serverConfig) -> doScan serverConfig


main :: IO ()
main = do
    let siteDir = "_site"
    ConfigInfo _ _ config <- readConfigInfo (siteDir </> "routes.yaml")
    build config siteDir "_output"
