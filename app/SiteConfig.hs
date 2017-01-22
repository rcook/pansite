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
    , siteDir
    ) where

import           System.Directory

data SiteConfig = SiteConfig FilePath FilePath

mkSiteConfig :: FilePath -> FilePath -> IO SiteConfig
mkSiteConfig siteDir outputDir = do
    canonicalSiteDir <- canonicalizePath siteDir
    canonicalOutputDir <- canonicalizePath outputDir
    return $ SiteConfig canonicalSiteDir canonicalOutputDir

siteDir :: SiteConfig -> FilePath
siteDir (SiteConfig siteDir _) = siteDir

outputDir :: SiteConfig -> FilePath
outputDir (SiteConfig _ outputDir) = outputDir
