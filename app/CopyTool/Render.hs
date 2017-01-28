module CopyTool.Render (copyRenderer) where

import           CopyTool.Types
import           Pansite
import           System.Directory

copyRenderer :: CopySettings -> ToolRunner
copyRenderer _ _ inputPath outputPath = copyFile inputPath outputPath
