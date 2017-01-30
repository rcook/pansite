module PansiteApp.Build
    ( build
    ) where

import           Control.Monad
import qualified Data.HashMap.Strict as HashMap
import           Development.Shake
import           Pansite
import           PansiteApp.ConfigInfo

-- TODO: Dependency paths won't work in the long term
-- We need to distinguish between the primary input(s) and the dependencies
-- In the case of Pandoc we should only pass the former on the command line
-- Right now, we'll only handle a single input hence the unsafe use of "head"
-- which we should remove in the future
runTool :: ToolRunner -> ConfigInfo -> FilePath -> [FilePath] -> IO ()
runTool toolRunner configInfo outputPath inputPaths = do
    let inputPath = head inputPaths -- TODO: Unsafe, let's not do this
    toolRunner (ToolContext (makeAppPath configInfo) (makeTargetPath configInfo) inputPath outputPath)

-- TODO: Pass some kind of map of renderers to support more than one build tool
build :: ToolRunnerMap -> ConfigInfo -> FilePath -> IO ()
build toolRunners configInfo@(ConfigInfo _ _ appDir outputDir shakeDir (AppConfig _ targets _)) target =
    shake shakeOptions { shakeFiles = shakeDir } $ do
        want [makeTargetPath configInfo target]

        forM_ targets $ \(Target path toolName dependencies) -> do
            let Just toolRunner = HashMap.lookup toolName toolRunners
            makeTargetPath configInfo path %> \outputPath -> do
                let dependencyPaths = (makeAppPath configInfo) <$> dependencies
                need dependencyPaths
                liftIO $ runTool toolRunner configInfo outputPath dependencyPaths
