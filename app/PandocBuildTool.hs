module PandocBuildTool (pandocRender) where

import           Build
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML
import           Paths_pansite

pandocRender :: RenderOpts -> String -> String
pandocRender (RenderOpts template) s =
    let Right doc = readMarkdown def s -- TODO: Irrefutable pattern
        writerOpts = def
            { writerStandalone = True
            , writerTemplate = template
            }
    in toEntities (renderHtml (writeHtml writerOpts doc))
