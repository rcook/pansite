{-|
Module      : PansiteApp.ConfigInfo
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

import           Data.Time
import           Pansite
import           PansiteApp.CopyTool
import           PansiteApp.PandocTool
import           PansiteApp.Util
import           System.Directory
import           System.FilePath

data ConfigInfo = ConfigInfo
    { timestamp :: UTCTime
    , appYamlPath :: FilePath
    , appDir :: FilePath
    , outputDir :: FilePath
    , shakeDir :: FilePath
    , ciApp :: App
    }

outputDirMeta :: FilePath
outputDirMeta = "$(@D)"

resolveFilePath :: FilePath -> FilePath -> FilePathResolver
resolveFilePath appDir outputDir path
    | takeDirectory path == outputDirMeta = outputDir </> skipDirectory path
    | otherwise = appDir </> path

emptyConfigInfo :: UTCTime -> FilePath -> FilePath -> FilePath -> FilePath -> ConfigInfo
emptyConfigInfo timestamp appYamlPath appDir outputDir shakeDir =
    ConfigInfo timestamp appYamlPath appDir outputDir shakeDir (App [] [])

readConfigInfo :: FilePath -> FilePath -> FilePath -> IO ConfigInfo
readConfigInfo appDir outputDir shakeDir = do
    appDir' <- canonicalizePath appDir
    outputDir' <- canonicalizePath outputDir
    shakeDir' <- canonicalizePath shakeDir
    let ctx = ParserContext (resolveFilePath appDir' outputDir')

    -- TODO: Use UTCTime field to determine if shakeVersion should be incremented
    let appYamlPath = appDir' </> "app.yaml"
    currentTime <- getCurrentTime

    appYamlExists <- doesFileExist appYamlPath
    if appYamlExists
        then do
            putStrLn $ "Getting timestamp for configuration file " ++ appYamlPath
            t <- getModificationTime appYamlPath
            mbApp <- readApp ctx [copyToolSpec, pandocToolSpec] appYamlPath
            case mbApp of
                Left message -> do
                    putStrLn $ "Could not parse configuration file at " ++ appYamlPath ++ ": " ++ message
                    return $ emptyConfigInfo currentTime appYamlPath appDir' outputDir' shakeDir'
                Right app -> return $ ConfigInfo t appYamlPath appDir' outputDir' shakeDir' app
        else do
            putStrLn $ "Configuration file does not exist at " ++ appYamlPath
            return $ emptyConfigInfo currentTime appYamlPath appDir' outputDir' shakeDir'

updateConfigInfo :: ConfigInfo -> IO (Maybe ConfigInfo)
updateConfigInfo ConfigInfo{..} = do
    t <- getModificationTime appYamlPath
    if (t > timestamp)
        then do
            configInfo' <- readConfigInfo appDir outputDir shakeDir
            return $ Just configInfo'
        else return Nothing
