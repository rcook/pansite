module PansiteApp.PandocTool.Render (pandocRenderer) where

import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML
import           Pansite
import           PansiteApp.PandocTool.Types
import           PansiteApp.Util

pandocRenderer ::  PandocSettings -> ToolRunner
pandocRenderer (PandocSettings mbTemplatePath vars) (ToolContext makeAppPath _ inputPath outputPath) = do
    input <- readFileUtf8 inputPath
    mbTemplate <- case mbTemplatePath of
                    Nothing -> return Nothing
                    Just templatePath -> Just <$> readFileUtf8 (makeAppPath templatePath)
    let Right doc = readMarkdown def input -- TODO: Irrefutable pattern
        writerOpts = def
            { writerTemplate = mbTemplate
            , writerVariables = vars
            , writerNumberSections = True -- TODO: Derive this from YAML
            }
    let html = toEntities (renderHtml (writeHtml writerOpts doc))
    writeFileUtf8 outputPath html
