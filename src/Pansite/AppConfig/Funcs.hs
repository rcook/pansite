{-|
Module      : Pansite.Config.Funcs
Description : Application configuration functions for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module Pansite.AppConfig.Funcs
    ( appConfigParser
    ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default
import           Data.List
import           Data.List.Split
import           Data.Traversable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Pansite.AppConfig.Types
import           Pansite.Tool

dependenciesKey :: Text
dependenciesKey = "dependencies"

inputsKey :: Text
inputsKey = "inputs"

pathKey :: Text
pathKey = "path"

routesKey :: Text
routesKey = "routes"

targetKey :: Text
targetKey = "target"

targetsKey :: Text
targetsKey = "targets"

toolKey :: Text
toolKey = "tool"

toolSettingsKey :: Text
toolSettingsKey = "tool-settings"

parseRoutePath :: String -> [String]
parseRoutePath = splitOn "/"

toRoutePath :: [String] -> String
toRoutePath = intercalate "/"

appConfigParser :: FilePathResolver -> [Tool] -> Value -> Parser AppConfig
appConfigParser resolveFilePath tools = withObject "appConfig" $ \o -> do
    let toolNames = map (\(Tool name _ _) -> name) tools
    routesNode <- o .: routesKey
    routes <- arrayParser "routes" routeParser routesNode
    targetsNode <- o .: targetsKey
    targets <- arrayParser "targets" (targetParser toolNames) targetsNode
    toolSettingsNode <- o .: toolSettingsKey
    toolSettings <- toolSettingsParser toolSettingsNode
    toolRunners <- case toolRunnersWithSettings resolveFilePath tools toolSettings of
                        Error message -> fail message
                        Success toolRunners -> return toolRunners
    return $ AppConfig routes targets toolRunners

toolSettingsParser :: Value -> Parser [(String, Value)]
toolSettingsParser = withObject "tool-settings" $ \o ->
    for (HashMap.toList o) $ \(name, value) -> return (Text.unpack name, value)

toolRunnersWithSettings :: FilePathResolver -> [Tool] -> [(String, Value)] -> Result (HashMap String ToolRunner)
toolRunnersWithSettings resolveFilePath tools nameValues =
    let nameValueMap = HashMap.fromList nameValues
    in case traverse (toolRunnerWithSettings resolveFilePath nameValueMap) tools of
        Error message -> Error message
        Success s -> Success $ HashMap.fromList s

toolRunnerWithSettings :: FilePathResolver -> HashMap String Value -> Tool -> Result (String, ToolRunner)
toolRunnerWithSettings resolveFilePath nameValueMap (Tool name parser runner) =
    case HashMap.lookup name nameValueMap of
        Nothing -> Success (name, def)
        Just value -> case parse (parser resolveFilePath) value of
            Error message -> Error message
            Success s -> Success (name, runner s)

arrayParser :: String -> (Value -> Parser a) -> Value -> Parser [a]
arrayParser expected f = withArray expected $ \arr -> mapM f (Vector.toList arr)

routeParser :: Value -> Parser Route
routeParser = withObject "route" $ \o -> do
    path <- parseRoutePath <$> (o .: pathKey)
    target <- o .: targetKey
    return $ Route path target

targetParser :: [ToolName] -> Value -> Parser Target
targetParser toolNames = withObject "target" $ \o -> do
    path <- o .: pathKey
    toolName <- o .: toolKey
    unless
        (toolName `elem` toolNames)
        (fail $ "Unsupported build tool \"" ++ toolName ++ "\"")
    inputs <- o .:? inputsKey .!= []
    dependencies <- o .:? dependenciesKey .!= []
    return $ Target path toolName inputs dependencies
