{-|
Module      : PansiteApp.App
Description : Main entrypoint for Pansite application
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module PansiteApp.App (appMain) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import           Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Pansite
import           PansiteApp.Build
import           PansiteApp.CommandLine
import           PansiteApp.ConfigInfo
import           PansiteApp.Util
import           System.FilePath

runApp :: ApacheLogger -> ServerConfig -> ConfigInfo -> IO ()
runApp logger (ServerConfig port) configInfo = do
    putStrLn $ "Listening on port " ++ show port
    configInfoRef <- newIORef configInfo
    run port (app logger configInfoRef)

app :: ApacheLogger -> IORef ConfigInfo -> Application
app logger configInfoRef req f = do
    -- TODO: Think about concurrent accesses...
    -- TODO: This is a mess..
    oldConfigInfo <- liftIO $ readIORef configInfoRef
    mbConfigInfo <- liftIO $ updateConfigInfo oldConfigInfo
    configInfo <- case mbConfigInfo of
                        Nothing -> return oldConfigInfo
                        Just configInfo' -> do
                            liftIO $ atomicWriteIORef configInfoRef configInfo'
                            return configInfo'

    let (ConfigInfo timestamp _ _ _ _ app@(App routes _)) = configInfo

    -- TODO: Let's not rebuild this on every request
    let m = Map.fromList (map (\(Route ps targetPath) -> (map Text.pack ps, targetPath)) routes)

    case Map.lookup (pathInfo req) m of
        Just targetPath -> do
            liftIO $ logger req status200 (Just 0)

            build configInfo targetPath

            putStrLn $ "Read from " ++ targetPath

            -- TODO: Eliminate this re-encoding
            content <- Text.pack <$> readFileUtf8 targetPath

            -- TODO: Ugh. Let's make this less hacky. It works for now though.
            let contentType = case (takeExtension targetPath) of
                                ".css" -> "text/css; charset=utf-8"
                                ".html" -> "text/html; charset=utf-8"
                                _ -> "text/plain; charset=utf-8"

            f $ responseLBS status200 [(hContentType, contentType)] (BL.fromStrict $ Text.encodeUtf8 content)
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"

appMain :: IO ()
appMain = parseOptions >>=
        \(Options serverConfig appDir outputDir shakeDir) -> withStdoutLogger $ \logger -> do
        configInfo <- readConfigInfo appDir outputDir shakeDir
        runApp logger serverConfig configInfo
