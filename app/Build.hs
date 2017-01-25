module Build (build) where

import           ConfigInfo
import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
import           Pansite

type Renderer = String -> String

-- TODO: Dependency paths won't work in the long term
-- We need to distinguish between the primary input(s) and the dependencies
-- In the case of Pandoc we should only pass the former on the command line
-- Right now, we'll only handle a single input hence the unsafe use of "head"
-- which we should remove in the future
runBuildTool :: BuildTool -> Renderer -> FilePath -> [FilePath] -> Action ()
runBuildTool Pandoc pandocRenderer outputPath inputPaths = liftIO $ do
    input <- readFile (head inputPaths) -- TODO: Unsafe, let's not do this
    let output = pandocRenderer input
    writeFile outputPath output

-- TODO: Pass some kind of map of renderers to support more than one build tool
build :: Renderer -> ConfigInfo -> FilePath -> IO ()
build pandocRenderer (ConfigInfo appDir outputDir (AppConfig _ targets)) target = shake shakeOptions { shakeFiles = outputDir } $ do
    want [outputDir </> target]

    forM_ targets $ \(Target path buildTool dependencies) -> do
        outputDir </> path %> \outputPath -> do
            let dependencyPaths = (appDir </>) <$> dependencies
            need dependencyPaths
            runBuildTool buildTool pandocRenderer outputPath dependencyPaths
