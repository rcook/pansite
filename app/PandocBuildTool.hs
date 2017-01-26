module PandocBuildTool (pandocRender) where

import           Build
import           Text.Blaze.Html.Renderer.String
import           Text.Pandoc
import           Text.Pandoc.XML
import           Paths_pansite

-- TODO: Eventually extract this as a configuration setting in routes.yaml
cssUrls :: [FilePath]
cssUrls = ["css/buttondown.css"]

pandocRender :: RenderOpts -> String -> String
pandocRender (RenderOpts template) s =
    let Right doc = readMarkdown def s -- TODO: Irrefutable pattern
        writerOpts = def
            { writerStandalone = True
            , writerTemplate = template
            , writerVariables = map (\x -> ("css", x)) cssUrls
            }
    in toEntities (renderHtml (writeHtml writerOpts doc))
