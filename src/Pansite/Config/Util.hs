{-|
Module      : Pansite.Config.Util
Description : Helper functions for Pansite app configuration
Copyright   : (C) Richard Cook, 2017-2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.Config.Util
    ( splitRoutePath
    ) where

import           Data.List.Split

-- | Split route path into fragments
--
-- Examples:
--
-- >>> splitRoutePath "aaa/bbb"
-- ["aaa","bbb"]
-- >>> splitRoutePath "aaa"
-- ["aaa"]
-- >>> splitRoutePath ""
-- []
splitRoutePath :: String -> [String]
splitRoutePath path = case splitOn "/" path of
    [""] -> []
    fragments -> fragments
