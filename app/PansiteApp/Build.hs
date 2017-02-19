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

import           Control.Monad
import           Data.String.Utils
import           Development.Shake
import           Pansite
import           PansiteApp.ConfigInfo
import           PansiteApp.Util

-- TODO: Patterns must only contain zero or one "%" characters
-- This should be enforced!
shakePattern :: FilePath -> FilePath
shakePattern = replace "%" "*"

-- TODO: Patterns must only contain zero or one "%" characters
-- This should be enforced!
expandWildcardPattern :: String -> String -> FilePath
expandWildcardPattern = replace "%"

build :: ConfigInfo -> FilePath -> IO ()
build (ConfigInfo AppPaths{..} _ (App _ targets)) targetPath =
    shake shakeOptions { shakeFiles = apShakeDir } $ do
        liftIO $ putStrLn ("want: " ++ targetPath)
        want [targetPath]

        forM_ targets $ \(Target path toolConfig inputPaths dependencyPaths) -> do
            liftIO $ putStrLn ("rule: " ++ path)
            shakePattern path %> \outputPath -> do
                let (stem, "%") = stems outputPath path -- TODO: Is this kind of irrefutable pattern OK? It's an assert of sorts.
                    replaceWithStem = expandWildcardPattern stem
                    inputPaths' = map replaceWithStem inputPaths
                    dependencyPaths' = map replaceWithStem dependencyPaths

                liftIO $ do
                    putStrLn ("need: " ++ show inputPaths')
                    putStrLn ("need: " ++ show dependencyPaths')
                need inputPaths'
                need dependencyPaths'

                let ctx = ToolContext outputPath inputPaths' dependencyPaths'
                liftIO $ toolConfigRunner ctx toolConfig
