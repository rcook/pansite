{-|
Module      : Pansite.Config.Types
Description : Functions for Pansite app configuration
Copyright   : (C) Richard Cook, 2017-2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Pansite.Config.Funcs
    ( readApp
    , runTool
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

type ToolMap = HashMap String Tool

updateTool :: UpdateContext -> Tool -> Value -> Parser Tool
updateTool ctx (Tool _ u _) value = u ctx value

updateTools :: UpdateContext -> ToolMap -> [(String, Value)] -> Parser ToolMap
updateTools ctx = foldM (\m (key, value) -> case HashMap.lookup key m of
                                    Nothing -> fail $ "Unsupported tool " ++ key
                                    Just toolConfigOrig -> do
                                        toolConfig <- updateTool ctx toolConfigOrig value
                                        return $ HashMap.insert key toolConfig m)

runTool :: RunContext -> Tool -> IO ()
runTool ctx (Tool _ _ r) = r ctx

arrayParser :: Object -> Text -> (Value -> Parser a) -> Parser [a]
arrayParser o key parser = helper (Text.unpack key) parser =<< o .: key
    where helper expected f = withArray expected $ \arr -> mapM f (Vector.toList arr)

appParser :: UpdateContext -> [Tool] -> Value -> Parser App
appParser ctx tools = withObject "App" $ \o -> do
    let toolMapOrig = HashMap.fromList (map (\t@(Tool k _ _) -> (k, t)) tools)
    toolSettings <- toolSettingsParser =<< o .:? "tool-settings" .!= emptyObject
    toolMapNew <- updateTools ctx toolMapOrig toolSettings
    routes <- arrayParser o "routes" (routeParser ctx)
    targets <- arrayParser o "targets" (targetParser ctx toolMapNew)
    return $ App routes targets

toolSettingsParser :: Value -> Parser [(String, Value)]
toolSettingsParser = withObject "tool-settings" $ \o ->
    for (HashMap.toList o) $ \(name, value) -> return (Text.unpack name, value)

routeParser :: UpdateContext -> Value -> Parser Route
routeParser (UpdateContext resolveFilePath) =
    withObject "route" $ \o -> Route
        <$> splitRoutePath <$> o .: "path"
        <*> (resolveFilePath <$> o .: "target")

pathPatternParser :: FilePathResolver -> String -> Parser PathPattern
pathPatternParser resolveFilePath s = case pathPattern (resolveFilePath s) of
    Left message -> fail message
    Right p -> return p

targetParser :: UpdateContext -> ToolMap -> Value -> Parser Target
targetParser ctx@(UpdateContext resolveFilePath) toolMap =
    withObject "target" $ \o -> do
        let pathPatternParser' = pathPatternParser resolveFilePath

        path <- pathPatternParser' =<< o .: "path"

        key <- o .: "tool"
        toolConfigOrig <- case HashMap.lookup key toolMap of
                        Nothing -> fail $ "Unsupported tool " ++ key
                        Just p -> return p
        toolConfig <- updateTool ctx toolConfigOrig =<< o .:? "tool-settings" .!= emptyObject

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

readApp :: UpdateContext -> [Tool] -> FilePath -> IO (Either String App)
readApp ctx tools appYamlPath = do
    yaml <- C8.readFile appYamlPath
    case decodeEither' yaml of
        Left e -> do
            putStrLn $ "WARNING: " ++ parseExceptionMessage appYamlPath e
            return $ Left (show e)
        Right value -> do
            case parse (appParser ctx tools) value of
                Error problem -> do
                    putStrLn $ "WARNING: " ++ resultErrorMessage appYamlPath problem
                    return $ Left problem
                Success app@(App routes targets) -> do
                    forM_ routes $ \(Route path target) ->
                        putStrLn $ "Route: " ++ show path ++ " -> " ++ target
                    forM_ targets $ \(Target path _ inputPaths dependencyPaths) -> do
                        putStrLn $ "Target: " ++ show path ++ ", " ++ show inputPaths ++ ", " ++ show dependencyPaths
                    return $ Right app
