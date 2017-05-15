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
    ( AppPaths (..)
    , ConfigInfo (..)
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

data AppPaths = AppPaths
    { apAppYamlPath :: FilePath
    , apAppDir :: FilePath
    , apCacheDir :: FilePath
    , apShakeDir :: FilePath
    }

data ConfigInfo = ConfigInfo
    { ciAppPaths :: AppPaths
    , ciTimestamp :: UTCTime
    , ciApp :: App
    }

outputDirMeta :: FilePath
outputDirMeta = "$(@D)"

resolveFilePath :: AppPaths -> FilePathResolver
resolveFilePath AppPaths{..} path
    | takeDirectory path == outputDirMeta = apCacheDir </> skipDirectory path
    | otherwise = apAppDir </> path

emptyConfigInfo :: AppPaths -> UTCTime -> ConfigInfo
emptyConfigInfo appPaths timestamp = ConfigInfo appPaths timestamp(App [] [])

readConfigInfo :: AppPaths -> IO ConfigInfo
readConfigInfo appPaths@AppPaths{..} = do
    let ctx = UpdateContext (resolveFilePath appPaths)

    -- TODO: Use UTCTime field to determine if shakeVersion should be incremented
    currentTime <- getCurrentTime

    appYamlExists <- doesFileExist apAppYamlPath
    if appYamlExists
        then do
            putStrLn $ "Getting timestamp for configuration file " ++ apAppYamlPath
            t <- getModificationTime apAppYamlPath
            mbApp <- readApp ctx [copyTool, pandocTool] apAppYamlPath
            case mbApp of
                Left message -> do
                    putStrLn $ "Could not parse configuration file at " ++ apAppYamlPath ++ ": " ++ message
                    return $ emptyConfigInfo appPaths currentTime
                Right app -> return $ ConfigInfo appPaths t app
        else do
            putStrLn $ "Configuration file does not exist at " ++ apAppYamlPath
            return $ emptyConfigInfo appPaths currentTime

updateConfigInfo :: ConfigInfo -> IO (Maybe ConfigInfo)
updateConfigInfo ConfigInfo{..} = do
    t <- getModificationTime (apAppYamlPath ciAppPaths)
    if (t > ciTimestamp)
        then do
            configInfo' <- readConfigInfo ciAppPaths
            return $ Just configInfo'
        else return Nothing
