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
    ( AppConfigInfo (..)
    , SiteConfig
    , mkSiteConfig
    , outputDir
    , readAppConfigInfo
    , routesYamlPath
    , siteDir
    ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Time
import           Data.Yaml
import           Pansite
import           System.Directory
import           System.FilePath

-- TODO: Use UTCTime field to determine if shakeVersion should be incremented
data AppConfigInfo = AppConfigInfo FilePath UTCTime AppConfig deriving Show

data SiteConfig = SiteConfig FilePath FilePath FilePath

mkSiteConfig :: FilePath -> FilePath -> IO SiteConfig
mkSiteConfig siteDir outputDir = do
    canonicalSiteDir <- canonicalizePath siteDir
    let routesYamlPath = canonicalSiteDir </> "routes.yaml"
    canonicalOutputDir <- canonicalizePath outputDir
    return $ SiteConfig canonicalSiteDir routesYamlPath canonicalOutputDir

siteDir :: SiteConfig -> FilePath
siteDir (SiteConfig p _ _) = p

routesYamlPath :: SiteConfig -> FilePath
routesYamlPath (SiteConfig _ p _) = p

outputDir :: SiteConfig -> FilePath
outputDir (SiteConfig _ _ p) = p

readAppConfigInfo :: FilePath -> IO AppConfigInfo
readAppConfigInfo path = do
    t <- getModificationTime path
    s <- C8.readFile path
    let Just c = decode s -- TODO: Irrefutable pattern
    return $ AppConfigInfo path t c
