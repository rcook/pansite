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
import qualified Data.HashMap.Strict as HashMap
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
import           System.Directory
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

    let (ConfigInfo timestamp _ app@(App routes _)) = configInfo

    -- TODO: Let's not rebuild this on every request
    let m = HashMap.fromList (map (\(Route ps targetPath) -> (map Text.pack ps, targetPath)) routes)

    let requestPath = pathInfo req
    putStrLn $ "requestPath=" ++ show requestPath

    case HashMap.lookup requestPath m of
        Just targetPath -> do
            liftIO $ logger req status200 (Just 0)

            build configInfo targetPath

            putStrLn $ "Read from " ++ targetPath

            -- TODO: Ugh. Let's make this less hacky. It works for now though.
            let (rawDataAction, contentType) = case (takeExtension targetPath) of
                                                ".css" -> (makeUtf8Response targetPath, "text/css; charset=utf-8")
                                                ".docx" -> (makeRawResponse targetPath, "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
                                                ".html" -> (makeUtf8Response targetPath, "text/html; charset=utf-8")
                                                _ -> (makeUtf8Response targetPath, "text/plain; charset=utf-8")

            rawData <- rawDataAction
            f $ responseLBS status200 [(hContentType, contentType)] rawData
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"

makeUtf8Response :: FilePath -> IO BL.ByteString
makeUtf8Response targetPath = do
    -- TODO: Eliminate multiple encoding redundancy!
    content <- Text.pack <$> readFileUtf8 targetPath
    return $ BL.fromStrict (Text.encodeUtf8 content)

makeRawResponse :: FilePath -> IO BL.ByteString
makeRawResponse = BL.readFile

mkAppPaths :: FilePath -> FilePath -> IO AppPaths
mkAppPaths appYamlPath outputDir = do
    appYamlPath' <- canonicalizePath appYamlPath
    let appDir = takeDirectory appYamlPath'
    outputDir' <- canonicalizePath $ appDir </> outputDir
    let cacheDir = outputDir' </> "cache"
        shakeDir = outputDir' </> "shake"
    return $ AppPaths appYamlPath' appDir cacheDir shakeDir

appMain :: IO ()
appMain = parseOptions >>=
    \(Options serverConfig appYamlPath outputDir) -> withStdoutLogger $ \logger -> do
        appPaths <- mkAppPaths appYamlPath outputDir
        configInfo <- readConfigInfo appPaths
        runApp logger serverConfig configInfo
