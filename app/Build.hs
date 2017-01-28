module Build
    ( build
    ) where

import           ConfigInfo
import           Control.Monad
import           Data.Default
import qualified Data.HashMap.Strict as HashMap
import           Development.Shake
import           Development.Shake.FilePath
import           Pansite
import           Util

-- TODO: Dependency paths won't work in the long term
-- We need to distinguish between the primary input(s) and the dependencies
-- In the case of Pandoc we should only pass the former on the command line
-- Right now, we'll only handle a single input hence the unsafe use of "head"
-- which we should remove in the future
runTool :: ToolRunner -> FilePath -> FilePath -> [FilePath] -> IO ()
runTool toolRunner appDir outputPath inputPaths = do
    let inputPath = head inputPaths -- TODO: Unsafe, let's not do this
    toolRunner appDir inputPath outputPath

-- TODO: Pass some kind of map of renderers to support more than one build tool
build :: ToolRunnerMap -> ConfigInfo -> FilePath -> IO ()
build toolRunners (ConfigInfo _ _ appDir outputDir (AppConfig _ targets _)) target = shake shakeOptions { shakeFiles = outputDir } $ do
    want [outputDir </> target]

    forM_ targets $ \(Target path toolName dependencies) -> do
        let Just toolRunner = HashMap.lookup toolName toolRunners
        outputDir </> path %> \outputPath -> do
            let dependencyPaths = (appDir </>) <$> dependencies
            need dependencyPaths
            liftIO $ runTool toolRunner appDir outputPath dependencyPaths
