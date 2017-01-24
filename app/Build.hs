module Build (build) where

import           SiteConfig
import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
import           Pansite

runBuildTool :: BuildTool -> FilePath -> [FilePath] -> Action ()
runBuildTool Pandoc outputPath inputPaths = cmd "pandoc -o" [outputPath] inputPaths

build :: ConfigInfo -> FilePath -> IO ()
build (ConfigInfo appDir outputDir (AppConfig _ targets)) target = shake shakeOptions { shakeFiles = outputDir } $ do
    want [outputDir </> target]

    forM_ targets $ \(Target path buildTool dependencies) -> do
        outputDir </> path %> \outputPath -> do
            let dependencyPaths = (appDir </>) <$> dependencies
            need dependencyPaths
            runBuildTool buildTool outputPath dependencyPaths
