module Pansite.AppConfig.Funcs () where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default
import           Data.Traversable
import           Data.Yaml
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import           Data.Yaml
import           Pansite.AppConfig.Keys
import           Pansite.Tool

data AppConfig = AppConfig String String ToolRunnerMap

appConfigParser :: [Tool] -> Value -> Parser AppConfig
appConfigParser tools = withObject "appConfig" $ \o -> do
    firstName <- o .: firstNameKey
    lastName <- o .: lastNameKey
    toolSettingsNode <- o .: buildToolSettingsKey
    toolSettings <- toolSettingsParser toolSettingsNode
    case toolRunnersWithSettings tools toolSettings of
        Error message -> fail message
        Success toolRunners -> return $ AppConfig firstName lastName toolRunners

toolSettingsParser :: Value -> Parser [(String, Value)]
toolSettingsParser = withObject "build-tool-settings" $ \o ->
    for (HashMap.toList o) $ \(name, value) -> return (Text.unpack name, value)

toolRunnersWithSettings :: [Tool] -> [(String, Value)] -> Result (HashMap String ToolRunner)
toolRunnersWithSettings tools nameValues =
    let nameValueMap = HashMap.fromList nameValues
    in case traverse (toolRunnerWithSettings nameValueMap) tools of
        Error message -> Error message
        Success s -> Success $ HashMap.fromList s

toolRunnerWithSettings :: HashMap String Value -> Tool -> Result (String, ToolRunner)
toolRunnerWithSettings nameValueMap (Tool name parser runner) =
    case HashMap.lookup name nameValueMap of
        Nothing -> Success (name, def)
        Just value -> case parse parser value of
            Error message -> Error message
            Success s -> Success (name, runner s)
