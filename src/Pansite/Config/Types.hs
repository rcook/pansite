{-|
Module      : Pansite.Config.Types
Description : Configuration types for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.Config.Types
    ( Config (..)
    , Input (..)
    , Route (..)
    ) where

data Config = Config [Route] [Input] deriving Show
data Route = Route [String] FilePath deriving Show
data Input = Input FilePath [FilePath] deriving Show
