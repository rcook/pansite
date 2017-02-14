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

        forM_ targets $ \(Target path toolConfig inputPaths dependencyPaths) -> do
            liftIO $ putStrLn ("rule: " ++ path)
            path %> \outputPath -> do
                liftIO $ putStrLn ("need: " ++ show inputPaths)
                need inputPaths
                liftIO $ putStrLn ("need: " ++ show dependencyPaths)
                need dependencyPaths
                let ctx = ToolContext outputPath inputPaths dependencyPaths
                liftIO $ toolConfigRunner ctx toolConfig
