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
    ( SiteConfig
    , mkSiteConfig
    , outputDir
    , routesYamlPath
    , siteDir
    ) where

import           System.Directory
import           System.FilePath

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
