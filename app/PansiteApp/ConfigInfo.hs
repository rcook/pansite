{-|
Module      : ConfigInfo
Description : Configuration info for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE RecordWildCards #-}

module PansiteApp.ConfigInfo
    ( ConfigInfo (..)
    , readConfigInfo
    , updateConfigInfo
    ) where

import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict as HashMap
import           Data.Time
import           Data.Yaml
import           Pansite
import           PansiteApp.CopyTool.Instances
import           PansiteApp.CopyTool.Parse
import           PansiteApp.CopyTool.Render
import           PansiteApp.PandocTool.Parse
import           PansiteApp.PandocTool.Render
import           System.Directory
import           System.FilePath

data ConfigInfo = ConfigInfo
    { timestamp :: UTCTime
    , appYamlPath :: FilePath
    , appDir :: FilePath
    , outputDir :: FilePath
    , appConfig :: AppConfig
    }

tools :: [Tool]
tools =
    [ Tool "pandoc" pandocSettingsParser pandocRenderer
    , Tool "copy" copySettingsParser copyRenderer
    ]

emptyConfigInfo :: UTCTime -> FilePath -> FilePath -> FilePath -> ConfigInfo
emptyConfigInfo timestamp appYamlPath appDir outputDir =
    ConfigInfo timestamp appYamlPath appDir outputDir (AppConfig [] [] HashMap.empty)

readConfigInfo :: FilePath -> FilePath -> IO ConfigInfo
readConfigInfo appDir outputDir = do
    appDir' <- canonicalizePath appDir
    outputDir' <- canonicalizePath outputDir

    -- TODO: Use UTCTime field to determine if shakeVersion should be incremented
    let appYamlPath = appDir' </> "app.yaml"
    currentTime <- getCurrentTime

    appYamlExists <- doesFileExist appYamlPath
    if appYamlExists
        then do
            putStrLn $ "Getting timestamp for configuration file " ++ appYamlPath
            t <- getModificationTime appYamlPath
            yaml <- C8.readFile appYamlPath
            case decodeEither' yaml of
                Left e -> do
                    putStrLn $ "Parse exception: " ++ show e
                    return $ emptyConfigInfo currentTime appYamlPath appDir' outputDir'
                Right value -> do
                    case parse (appConfigParser tools) value of
                        Error message -> do
                            putStrLn $ "Could not parse configuration file at " ++ appYamlPath ++ ": " ++ message
                            return $ emptyConfigInfo currentTime appYamlPath appDir' outputDir'
                        Success appConfig -> return $ ConfigInfo t appYamlPath appDir' outputDir' appConfig
        else do
            putStrLn $ "Configuration file does not exist at " ++ appYamlPath
            return $ emptyConfigInfo currentTime appYamlPath appDir' outputDir'

updateConfigInfo :: ConfigInfo -> IO (Maybe ConfigInfo)
updateConfigInfo ConfigInfo{..} = do
    t <- getModificationTime appYamlPath
    if (t > timestamp)
        then do
            configInfo' <- readConfigInfo appDir outputDir
            return $ Just configInfo'
        else return Nothing