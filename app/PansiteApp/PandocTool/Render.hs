{-# LANGUAGE RecordWildCards #-}

module PansiteApp.PandocTool.Render (pandocRenderer) where

import           Control.Monad
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML
import           Pansite
import           PansiteApp.PandocTool.Types
import           PansiteApp.Util

pandocRenderer ::  PandocSettings -> ToolRunner
pandocRenderer (PandocSettings mbTemplatePath vars numberSections) ToolContext{..} = do
    when -- TODO: Should support multiple inputs
        (length toolContextInputPaths /= 1)
        (error "Pandoc tool currently supports exactly one input")
    input <- readFileUtf8 (head toolContextInputPaths) -- TODO: Should support multiple inputs
    mbTemplate <- case mbTemplatePath of
                    Nothing -> return Nothing
                    Just templatePath -> Just <$> readFileUtf8 templatePath
    let Right doc = readMarkdown def input -- TODO: Irrefutable pattern
        writerOpts = def
            { writerTemplate = mbTemplate
            , writerVariables = vars
            , writerNumberSections = numberSections
            }
    let html = toEntities (renderHtml (writeHtml writerOpts doc))
    writeFileUtf8 toolContextOutputPath html
