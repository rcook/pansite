module PansiteApp.CopyTool.Render (copyRenderer) where

import           Pansite
import           PansiteApp.CopyTool.Types
import           System.Directory

copyRenderer :: CopySettings -> ToolRunner
copyRenderer _ (ToolContext _ inputPath outputPath) = copyFile inputPath outputPath
