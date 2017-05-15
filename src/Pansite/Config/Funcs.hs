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
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import qualified Data.Vector as Vector
import           Data.Yaml
import           Pansite.Config.Types
import           Pansite.Config.Util
import           Pansite.PathPattern

type ToolConfigMap = HashMap String ToolConfig

toolConfigUpdater :: ParserContext -> ToolConfig -> Value -> Parser ToolConfig
toolConfigUpdater ctx (ToolConfig _ u _) value = u ctx value

toolConfigRunner :: ToolContext -> ToolConfig -> IO ()
toolConfigRunner ctx (ToolConfig _ _ r) = r ctx

arrayParser :: Object -> Text -> (Value -> Parser a) -> Parser [a]
arrayParser o key parser = helper (Text.unpack key) parser =<< o .: key
    where helper expected f = withArray expected $ \arr -> mapM f (Vector.toList arr)

appParser :: ParserContext -> [ToolConfig] -> Value -> Parser App
appParser ctx toolSpecs = withObject "App" $ \o -> do
    let toolConfigMapOrig = HashMap.fromList (map (\t@(ToolConfig k _ _) -> (k, t)) toolSpecs)
    toolConfigPairs <- toolConfigsParser =<< o .:? "tool-settings" .!= emptyObject
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
        <$> splitRoutePath <$> o .: "path"
        <*> (resolveFilePath <$> o .: "target")

pathPatternParser :: FilePathResolver -> String -> Parser PathPattern
pathPatternParser resolveFilePath s = case pathPattern (resolveFilePath s) of
    Left message -> fail message
    Right p -> return p

targetParser :: ParserContext -> ToolConfigMap -> Value -> Parser Target
targetParser ctx@(ParserContext resolveFilePath) toolConfigMap =
    withObject "target" $ \o -> do
        let pathPatternParser' = pathPatternParser resolveFilePath

        path <- pathPatternParser' =<< o .: "path"

        key <- o .: "tool"
        toolConfigOrig <- case HashMap.lookup key toolConfigMap of
                        Nothing -> fail $ "Unsupported tool " ++ key
                        Just p -> return p
        toolConfig <- toolConfigUpdater ctx toolConfigOrig =<< o .:? "tool-settings" .!= emptyObject

        inputPaths <- mapM pathPatternParser' =<< o .: "inputs"

        dependencyPaths <- mapM pathPatternParser' =<< o .: "dependencies"

        return $ Target path toolConfig inputPaths dependencyPaths

parseExceptionMessage :: FilePath -> ParseException -> String
parseExceptionMessage appYamlPath (InvalidYaml (Just (YamlException problem))) =
    "Invalid YAML: " ++ problem ++ "\n" ++
    "Location: " ++ appYamlPath
parseExceptionMessage appYamlPath (InvalidYaml (Just (YamlParseException problem ctx (YamlMark _ line column)))) =
    "Invalid YAML: " ++ problem ++ " " ++ ctx ++ "\n" ++
    "Location: " ++ appYamlPath ++ ":" ++ show line ++ ":" ++ show column
parseExceptionMessage appYamlPath (InvalidYaml _) = "Invalid YAML in " ++ appYamlPath
parseExceptionMessage _ e = error $ "Unhandled exception: " ++ show e

resultErrorMessage :: FilePath -> String -> String
resultErrorMessage appYamlPath problem =
    "Invalid configuration: " ++ problem ++ "\n" ++
    "Location: " ++ appYamlPath

readApp :: ParserContext -> [ToolConfig] -> FilePath -> IO (Either String App)
readApp ctx toolSpecs appYamlPath = do
    yaml <- C8.readFile appYamlPath
    case decodeEither' yaml of
        Left e -> do
            putStrLn $ "WARNING: " ++ parseExceptionMessage appYamlPath e
            return $ Left (show e)
        Right value -> do
            case parse (appParser ctx toolSpecs) value of
                Error problem -> do
                    putStrLn $ "WARNING: " ++ resultErrorMessage appYamlPath problem
                    return $ Left problem
                Success app@(App routes targets) -> do
                    forM_ routes $ \(Route path target) ->
                        putStrLn $ "Route: " ++ show path ++ " -> " ++ target
                    forM_ targets $ \(Target path _ inputPaths dependencyPaths) -> do
                        putStrLn $ "Target: " ++ show path ++ ", " ++ show inputPaths ++ ", " ++ show dependencyPaths
                    return $ Right app
