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
    , routesYamlPath :: FilePath
    , appDir :: FilePath
    , outputDir :: FilePath
    , appConfig :: AppConfig
    } deriving Show

emptyConfigInfo :: UTCTime -> FilePath -> FilePath -> FilePath -> ConfigInfo
emptyConfigInfo timestamp routesYamlPath appDir outputDir = ConfigInfo timestamp routesYamlPath appDir outputDir (AppConfig [] [])

readConfigInfo :: FilePath -> FilePath -> IO ConfigInfo
readConfigInfo appDir outputDir = do
    appDir' <- canonicalizePath appDir
    outputDir' <- canonicalizePath outputDir

    -- TODO: Use UTCTime field to determine if shakeVersion should be incremented
    let routesYamlPath = appDir' </> "routes.yaml"
    currentTime <- getCurrentTime

    routesYamlExists <- doesFileExist routesYamlPath
    if routesYamlExists
        then do
            putStrLn $ "Getting timestamp for configuration file " ++ routesYamlPath
            t <- getModificationTime routesYamlPath
            s <- C8.readFile routesYamlPath
            case decode s of
                Just c -> return $ ConfigInfo t routesYamlPath appDir' outputDir' c
                Nothing -> do
                    putStrLn $ "Could not parse configuration file at " ++ routesYamlPath
                    return $ emptyConfigInfo currentTime routesYamlPath appDir' outputDir'
        else do
            putStrLn $ "Configuration file does not exist at " ++ routesYamlPath
            return $ emptyConfigInfo currentTime routesYamlPath appDir' outputDir'

updateConfigInfo :: ConfigInfo -> IO (Maybe ConfigInfo)
updateConfigInfo ConfigInfo{..} = do
    t <- getModificationTime routesYamlPath
    if (t > timestamp)
        then do
            configInfo' <- readConfigInfo appDir outputDir
            return $ Just configInfo'
        else return Nothing
