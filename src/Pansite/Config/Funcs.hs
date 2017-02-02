{-|
Module      : Pansite.Config.Types
Description : Functions for Pansite app configuration
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Pansite.Config.Funcs
    ( readApp
    , toolConfigRunner
    ) where

import           Control.Monad
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as C8
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import qualified Data.Vector as Vector
import           Data.Yaml
import           Pansite.Config.Types

type ToolConfigMap = HashMap String ToolConfig

defaultToolConfig :: ToolSpec -> ToolConfig
defaultToolConfig (ToolSpec key u r) = ToolConfig u r def

toolConfigUpdater :: ParserContext -> ToolConfig -> Value -> Parser ToolConfig
toolConfigUpdater ctx (ToolConfig u r a) value = do
    result <- u ctx a value
    return $ ToolConfig u r result

toolConfigRunner :: ToolContext -> ToolConfig -> IO ()
toolConfigRunner ctx (ToolConfig _ r a) = r ctx a

arrayParser :: Object -> Text -> (Value -> Parser a) -> Parser [a]
arrayParser o key parser = helper (Text.unpack key) parser =<< (o .: key)
    where helper expected f = withArray expected $ \arr -> mapM f (Vector.toList arr)

-- TODO: Create a unit test for this!
parseRoutePath :: String -> [String]
parseRoutePath path =
    let fragments = splitOn "/" path
        fragmentCount = length fragments
    in if (fragmentCount == 1 && fragments !! 0 == "")
          then []
          else fragments

appParser :: ParserContext -> [ToolSpec] -> Value -> Parser App
appParser ctx toolSpecs = withObject "App" $ \o -> do
    let toolConfigMapOrig = HashMap.fromList (map (\t@(ToolSpec k _ _) -> (k, defaultToolConfig t)) toolSpecs)
    toolConfigPairs <- toolConfigsParser =<< o .: "tool-settings"
    toolConfigMap <- updateToolConfigs ctx toolConfigMapOrig toolConfigPairs
    routes <- arrayParser o "routes" (routeParser ctx)
    targets <- arrayParser o "targets" (targetParser ctx toolConfigMap)
    return $ App routes targets

toolConfigsParser :: Value -> Parser [(String, Value)]
toolConfigsParser = withObject "tool-settings" $ \o ->
    for (HashMap.toList o) $ \(name, value) -> return (Text.unpack name, value)

updateToolConfigs :: ParserContext -> ToolConfigMap -> [(String, Value)] -> Parser ToolConfigMap
updateToolConfigs ctx = foldM (\m (key, value) -> case HashMap.lookup key m of
                                    Nothing -> fail $ "Unsupported tool " ++ key
                                    Just toolConfigOrig -> do
                                        toolConfig <- toolConfigUpdater ctx toolConfigOrig value
                                        return $ HashMap.insert key toolConfig m)

routeParser :: ParserContext -> Value -> Parser Route
routeParser (ParserContext resolveFilePath) =
    withObject "route" $ \o -> Route
        <$> parseRoutePath <$> o .: "path"
        <*> (resolveFilePath <$> o .: "target")

targetParser :: ParserContext -> ToolConfigMap -> Value -> Parser Target
targetParser ctx@(ParserContext resolveFilePath) toolConfigMap =
    withObject "target" $ \o -> do
        path <- resolveFilePath <$> o .: "path"
        key <- o .: "tool"
        toolConfigOrig <- case HashMap.lookup key toolConfigMap of
                        Nothing -> fail $ "Unsupported tool " ++ key
                        Just p -> return p
        toolConfig <- toolConfigUpdater ctx toolConfigOrig =<< o .:? "tool-settings" .!= emptyObject
        inputPaths <- ((map resolveFilePath) <$> o .: "inputs")
        dependencyPaths <-  ((map resolveFilePath) <$> o .: "dependencies")
        return $ Target path toolConfig inputPaths dependencyPaths

readApp :: ParserContext -> [ToolSpec] -> FilePath -> IO (Either String App)
readApp ctx toolSpecs appYamlPath = do
    yaml <- C8.readFile appYamlPath
    let value = case decodeEither' yaml of
                    Left e -> error $ "Exception: " ++ show e -- TODO: Fix error handling
                    Right v -> v
        app@(App routes targets) = case parse (appParser ctx toolSpecs) value of
                                    Error message -> error $ "Message: " ++ message -- TODO: Fix error handling
                                    Success v -> v

    forM_ routes $ \(Route path target) ->
        putStrLn $ "Route: " ++ show path ++ " -> " ++ target

    forM_ targets $ \(Target path toolConfig inputPaths dependencyPaths) -> do
        putStrLn $ "Target: " ++ path ++ ", " ++ show inputPaths ++ ", " ++ show dependencyPaths

    return $ Right app
