module Build (build) where

import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
import           Pansite

runBuildTool :: BuildTool -> FilePath -> [FilePath] -> Action ()
runBuildTool Pandoc outputPath inputPaths = cmd "pandoc -o" [outputPath] inputPaths

build :: Config -> FilePath -> FilePath -> FilePath -> IO ()
build (Config _ targets) target siteDir outputDir = shake shakeOptions { shakeFiles = outputDir } $ do
    want [target]

    forM_ targets $ \(Target path buildTool dependencies) -> do
        outputDir </> path %> \outputPath -> do
            let dependencyPaths = (siteDir </>) <$> dependencies
            need dependencyPaths
            runBuildTool buildTool outputPath dependencyPaths
