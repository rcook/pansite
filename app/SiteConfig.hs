{-|
Module      : SiteConfig
Description : Site configuration for Pansite app
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module SiteConfig
    ( ConfigInfo (..)
    , appDir
    , outputDir
    , readConfigInfo
    ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Time
import           Data.Yaml
import           Pansite
import           System.Directory
import           System.FilePath

data ConfigInfo = ConfigInfo FilePath FilePath AppConfig deriving Show

appDir :: ConfigInfo -> FilePath
appDir (ConfigInfo p _ _) = p

outputDir :: ConfigInfo -> FilePath
outputDir (ConfigInfo _ p _) = p

readConfigInfo :: FilePath -> FilePath -> IO ConfigInfo
readConfigInfo appDir outputDir = do
    appDir' <- canonicalizePath appDir
    outputDir' <- canonicalizePath outputDir

    -- TODO: Use UTCTime field to determine if shakeVersion should be incremented
    --t <- getModificationTime path

    s <- C8.readFile (appDir' </> "routes.yaml")
    let Just c = decode s -- TODO: Irrefutable pattern
    return $ ConfigInfo appDir' outputDir' c
