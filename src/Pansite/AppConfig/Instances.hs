{-|
Module      : Pansite.Config.Instances
Description : Application configuration type class instances for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Pansite.AppConfig.Instances () where

import           Control.Applicative
import           Data.List
import           Data.List.Split
import           Data.Text (Text (..))
import           Data.Yaml
import           Pansite.AppConfig.Types

instance FromJSON AppConfig where
    parseJSON (Object v) = AppConfig
        <$> v .: routesKey
        <*> v .:? targetsKey .!= []
    parseJSON _ = empty

instance ToJSON AppConfig where
    toJSON (AppConfig routes targets) = object
        [ routesKey .= routes
        , targetsKey .= targets
        ]

instance FromJSON BuildTool where
    parseJSON "copy" = pure Copy
    parseJSON "pandoc" = pure Pandoc
    parseJSON _ = empty

instance ToJSON BuildTool where
    toJSON Copy = "copy"
    toJSON Pandoc = "pandoc"

instance FromJSON Route where
    parseJSON (Object v) = Route
        <$> parseRoutePath <$> (v .: pathKey)
        <*> v .: targetKey
    parseJSON _ = empty

instance ToJSON Route where
    toJSON (Route paths target) = object
        [ pathKey .= toRoutePath paths
        , targetKey .= target
        ]

instance FromJSON Target where
    parseJSON (Object v) = Target
        <$> v .: pathKey
        <*> v .: buildToolKey
        <*> v .:? dependenciesKey .!= []
    parseJSON _ = empty

instance ToJSON Target where
    toJSON (Target path buildTool dependencies) = object
        [ pathKey .= path
        , buildToolKey .= buildTool
        , dependenciesKey .= dependencies
        ]

buildToolKey :: Text
buildToolKey = "build-tool"

dependenciesKey :: Text
dependenciesKey = "dependencies"

pathKey :: Text
pathKey = "path"

routesKey :: Text
routesKey = "routes"

targetKey :: Text
targetKey = "target"

targetsKey :: Text
targetsKey = "targets"

parseRoutePath :: String -> [String]
parseRoutePath = splitOn "/"

toRoutePath :: [String] -> String
toRoutePath = intercalate "/"
