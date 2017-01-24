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
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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

runApp :: ServerConfig -> SiteConfig -> ApacheLogger -> AppConfigInfo -> IO ()
runApp (ServerConfig port) siteConfig logger (AppConfigInfo _ _ appConfig@(AppConfig routes _)) = do
    let m = Map.fromList (map (\(Route paths sourcePath) -> (map Text.pack paths, Text.pack sourcePath)) routes)
    putStrLn $ "Listening on port " ++ show port
    run port (app appConfig siteConfig logger m)

app :: AppConfig -> SiteConfig -> ApacheLogger -> Map [Text] Text -> Application
app appConfig siteConfig logger m req f =
    case Map.lookup (pathInfo req) m of
        Just sourcePath -> do
            liftIO $ logger req status200 (Just 0)

            -- TODO: Eliminate this re-encoding
            let sourcePath' = Text.unpack sourcePath

            targetOutputPath <- buildTarget appConfig siteConfig sourcePath'
            putStrLn $ "Read from " ++ targetOutputPath

            -- TODO: Eliminate this re-encoding
            content <- Text.pack <$> readFile targetOutputPath

            f $ responseLBS status200 [(hContentType, "text/html")] (BL.fromStrict $ Text.encodeUtf8 content)
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"

buildTarget :: AppConfig -> SiteConfig -> FilePath -> IO FilePath
buildTarget appConfig siteConfig target = do
    -- TODO: Remove some of the redundancy from here
    -- TODO: Do not require that we pass outputDir' </> target as the target to build
    let siteDir' = siteDir siteConfig
        outputDir' = outputDir siteConfig
        targetPath = outputDir' </> target
    build appConfig targetPath siteDir' outputDir'
    return targetPath

main :: IO ()
main = parseOptions >>= \(Options serverConfig) -> withStdoutLogger $ \logger -> do
    -- TODO: Move _site and _output into command-line options parser
    siteConfig <- mkSiteConfig "_site" "_output"
    appConfigInfo <- readAppConfigInfo (routesYamlPath siteConfig)
    runApp serverConfig siteConfig logger appConfigInfo
