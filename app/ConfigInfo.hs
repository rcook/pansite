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

module ConfigInfo
    ( ConfigInfo (..)
    , readConfigInfo
    , updateConfigInfo
    ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Time
import           Data.Yaml
import           Pansite
import           System.Directory
import           System.FilePath

data ConfigInfo = ConfigInfo
    { timestamp :: UTCTime
    , appYamlPath :: FilePath
    , appDir :: FilePath
    , outputDir :: FilePath
    , appConfig :: AppConfig
    } deriving Show

emptyConfigInfo :: UTCTime -> FilePath -> FilePath -> FilePath -> ConfigInfo
emptyConfigInfo timestamp appYamlPath appDir outputDir = ConfigInfo timestamp appYamlPath appDir outputDir (AppConfig [] [])

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
            s <- C8.readFile appYamlPath
            case decode s of
                Just c -> return $ ConfigInfo t appYamlPath appDir' outputDir' c
                Nothing -> do
                    putStrLn $ "Could not parse configuration file at " ++ appYamlPath
                    return $ emptyConfigInfo currentTime appYamlPath appDir' outputDir'
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
