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
import           ConfigInfo
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           PandocBuildTool
import           Pansite
import           System.Directory
import           System.FilePath
import           System.Process
import           ToolSettings
import           Util

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

    let (ConfigInfo timestamp _ _ _ appConfig@(AppConfig routes _)) = configInfo

    -- TODO: Let's not rebuild this on every request
    let m = Map.fromList (map (\(Route paths sourcePath) -> (map Text.pack paths, Text.pack sourcePath)) routes)

    case Map.lookup (pathInfo req) m of
        Just target -> do
            liftIO $ logger req status200 (Just 0)

            -- TODO: Eliminate this re-encoding
            let target' = Text.unpack target

            -- TODO: Come up with some mechanism to pass multiple build tools
            -- Currently we're passing pandocRender even if the build tool is "copy" etc.
            build pandocRender configInfo target'

            let targetOutputPath = (outputDir configInfo) </> target'
            putStrLn $ "Read from " ++ targetOutputPath

            -- TODO: Eliminate this re-encoding
            content <- Text.pack <$> readFileUtf8 targetOutputPath

            -- TODO: Ugh. Let's make this less hacky. It works for now though.
            let contentType = case (takeExtension targetOutputPath) of
                                ".css" -> "text/css; charset=utf-8"
                                ".html" -> "text/html; charset=utf-8"
                                _ -> "text/plain; charset=utf-8"

            f $ responseLBS status200 [(hContentType, contentType)] (BL.fromStrict $ Text.encodeUtf8 content)
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"

main :: IO ()
main = demoToolSettings >>
    parseOptions >>=
        \(Options serverConfig appDir outputDir) -> withStdoutLogger $ \logger -> do
        configInfo <- readConfigInfo appDir outputDir
        runApp logger serverConfig configInfo
