{-# LANGUAGE RecordWildCards #-}

module PansiteApp.Build (build) where

import           Control.Monad
import qualified Data.HashMap.Strict as HashMap
import           Development.Shake
import           Pansite
import           PansiteApp.ConfigInfo

build :: ToolRunnerMap -> ConfigInfo -> FilePath -> IO ()
build toolRunners configInfo@(ConfigInfo _ _ _ _ shakeDir (AppConfig _ targets _)) target =
    shake shakeOptions { shakeFiles = shakeDir } $ do
        want [makeTargetPath configInfo target]

        forM_ targets $ \Target{..} -> do
            let Just toolRunner = HashMap.lookup targetTool toolRunners
            makeTargetPath configInfo targetPath %> \outputPath -> do
                let inputPaths = (makeTargetPath configInfo) <$> targetInputs
                    dependencyPaths = (makeTargetPath configInfo) <$> targetDependencies
                need inputPaths
                need dependencyPaths
                liftIO $ toolRunner (ToolContext outputPath inputPaths dependencyPaths)
