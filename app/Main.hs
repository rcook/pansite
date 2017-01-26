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
import           Util

runApp :: ApacheLogger -> ServerConfig -> ConfigInfo -> IO ()
runApp logger (ServerConfig port) configInfo@(ConfigInfo _ _ appConfig@(AppConfig routes _)) = do
    let m = Map.fromList (map (\(Route paths sourcePath) -> (map Text.pack paths, Text.pack sourcePath)) routes)
    putStrLn $ "Listening on port " ++ show port
    run port (app logger configInfo m)

app :: ApacheLogger -> ConfigInfo -> Map [Text] Text -> Application
app logger configInfo m req f =
    case Map.lookup (pathInfo req) m of
        Just target -> do
            liftIO $ logger req status200 (Just 0)

            -- TODO: Eliminate this re-encoding
            let target' = Text.unpack target

            -- TODO: Come up with some mechanism to pass multiple build tools
            build pandocRender configInfo target'

            let targetOutputPath = (outputDir configInfo) </> target'
            putStrLn $ "Read from " ++ targetOutputPath

            -- TODO: Eliminate this re-encoding
            content <- Text.pack <$> readFileUtf8 targetOutputPath

            f $ responseLBS status200 [(hContentType, "text/html; charset=utf-8")] (BL.fromStrict $ Text.encodeUtf8 content)
        Nothing -> f $ responseLBS status200 [(hContentType, "text/plain")] "No such route"

main :: IO ()
main = parseOptions >>= \(Options serverConfig appDir outputDir) -> withStdoutLogger $ \logger -> do
    configInfo <- readConfigInfo appDir outputDir
    runApp logger serverConfig configInfo
