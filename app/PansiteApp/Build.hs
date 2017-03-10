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
import           Development.Shake
import           Pansite
import           PansiteApp.ConfigInfo

build :: ConfigInfo -> FilePath -> IO ()
build (ConfigInfo AppPaths{..} _ (App _ targets)) targetPath =
    shake shakeOptions { shakeFiles = apShakeDir } $ do
        liftIO $ putStrLn ("want: " ++ targetPath)
        want [targetPath]

        forM_ targets $ \(Target pathMakePattern toolConfig inputPathPatterns dependencyPathPatterns) -> do
            liftIO $ do
                putStrLn $ "pathMakePattern: " ++ show pathMakePattern
                putStrLn $ "inputPathPatterns: " ++ show inputPathPatterns
                putStrLn $ "dependencyPathPatterns: " ++ show dependencyPathPatterns

            pathMakePattern %%>> \outputPath -> do
                let stem = pathPatternStem pathMakePattern outputPath
                    inputPaths = map (substituteStem stem) inputPathPatterns
                    dependencyPaths = map (substituteStem stem) dependencyPathPatterns

                liftIO $ do
                    putStrLn $ "need: " ++ show inputPaths
                    putStrLn $ "need: " ++ show dependencyPaths
                need inputPaths
                need dependencyPaths

                let ctx = ToolContext outputPath inputPaths dependencyPaths
                liftIO $ toolConfigRunner ctx toolConfig
