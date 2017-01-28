module PandocBuildTool.Render
    ( pandocRenderer
    ) where

import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML
import           PandocBuildTool.Types
import           Pansite
import           System.FilePath
import           Util

pandocRenderer ::  PandocSettings2 -> ToolRunner
pandocRenderer (PandocSettings2 mbTemplatePath vars) appDir inputPath outputPath = do
    input <- readFileUtf8 inputPath
    mbTemplate <- case mbTemplatePath of
                    Nothing -> return Nothing
                    Just templatePath -> Just <$> readFileUtf8 (appDir </> templatePath)
    let Right doc = readMarkdown def input -- TODO: Irrefutable pattern
        writerOpts = def
            { writerTemplate = mbTemplate
            , writerVariables = vars
            , writerNumberSections = True -- TODO: Derive this from YAML
            }
    let html = toEntities (renderHtml (writeHtml writerOpts doc))
    writeFileUtf8 outputPath html
