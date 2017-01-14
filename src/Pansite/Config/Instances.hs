{-|
Module      : Pansite.Config.Instances
Description : Configuration type class instances for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Pansite.Config.Instances () where

import           Control.Applicative
import           Data.List
import           Data.List.Split
import           Data.Text (Text (..))
import           Data.Yaml
import           Pansite.Config.Types

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .: routesKey
        <*> v .: inputsKey
    parseJSON _ = empty

instance ToJSON Config where
    toJSON (Config routes inputs) = object
        [ routesKey .= routes
        , inputsKey .= inputs
        ]

instance FromJSON Route where
    parseJSON (Object v) = Route
        <$> parseRoutePath <$> (v .: pathKey)
        <*> v .: sourcePathKey
    parseJSON _ = empty

instance ToJSON Route where
    toJSON (Route paths sourcePath) = object
        [ pathKey .= toRoutePath paths
        , sourcePathKey .= sourcePath
        ]

instance FromJSON Input where
    parseJSON (Object v) = Input
        <$> v .: sourcePathKey
        <*> v .:? dependenciesKey .!= []
    parseJSON _ = empty

instance ToJSON Input where
    toJSON (Input sourcePath dependencies) = object
        [ sourcePathKey .= sourcePath
        , dependenciesKey .= dependencies
        ]

routesKey :: Text
routesKey = "routes"

inputsKey :: Text
inputsKey = "inputs"

pathKey :: Text
pathKey = "path"

sourcePathKey :: Text
sourcePathKey = "sourcePath"

dependenciesKey :: Text
dependenciesKey = "dependencies"

parseRoutePath :: String -> [String]
parseRoutePath = splitOn "/"

toRoutePath :: [String] -> String
toRoutePath = intercalate "/"
