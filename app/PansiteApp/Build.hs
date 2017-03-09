{-|
Module      : PansiteApp.Build
Description : Shake build function
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE RecordWildCards #-}

module PansiteApp.Build (build) where

import           Control.Exception
import           Control.Monad
import           Data.String.Utils
import           Development.Shake
import           Pansite
import           PansiteApp.ConfigInfo
import           PansiteApp.Util

data WildcardError = WildcardError FilePath deriving Show
instance Exception WildcardError

replaceZeroOrOneMarker :: String -> String -> String -> Either String String
replaceZeroOrOneMarker old new str
    | let n = countOccurrences str old in n == 0 || n == 1 = Right $ replace old new str
    | otherwise = Left str

replaceZeroOrOneMarkerMulti :: String -> String -> [String] -> Either String [String]
replaceZeroOrOneMarkerMulti old new = mapM (replaceZeroOrOneMarker old new)

gnuMakeWildcardToken :: String
gnuMakeWildcardToken = "%"

shakeWildcardToken :: String
shakeWildcardToken = "*"

build :: ConfigInfo -> FilePath -> IO ()
build (ConfigInfo AppPaths{..} _ (App _ targets)) targetPath =
    shake shakeOptions { shakeFiles = apShakeDir } $ do
        liftIO $ putStrLn ("want: " ++ targetPath)
        want [targetPath]

        forM_ targets $ \(Target pathRule toolConfig inputPathRules dependencyPathRules) -> do
            shakePathPattern <- liftIO $ do
                putStrLn $ "rule: " ++ pathRule

                case replaceZeroOrOneMarker gnuMakeWildcardToken shakeWildcardToken pathRule of
                    Right p -> return p
                    Left message -> throwIO $ WildcardError message

            shakePathPattern %> \outputPath -> do
                (inputPaths, dependencyPaths) <- liftIO $ do
                    let (stem, "%") = stems outputPath pathRule

                    inputPaths <- case replaceZeroOrOneMarkerMulti gnuMakeWildcardToken stem inputPathRules of
                        Left message -> throwIO $ WildcardError message
                        Right paths -> return paths

                    dependencyPaths <- case replaceZeroOrOneMarkerMulti gnuMakeWildcardToken stem dependencyPathRules of
                        Left message -> throwIO $ WildcardError message
                        Right paths -> return paths

                    putStrLn $ "need: " ++ show inputPaths
                    putStrLn $ "need: " ++ show dependencyPaths

                    return (inputPaths, dependencyPaths)

                need inputPaths
                need dependencyPaths

                let ctx = ToolContext outputPath inputPaths dependencyPaths
                liftIO $ toolConfigRunner ctx toolConfig
