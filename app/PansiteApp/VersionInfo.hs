{-|
Module      : Pansite.VersionInfo
Description : Version information for Pansite application
Copyright   : (C) Richard Cook, 2017-2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE TemplateHaskell #-}

module PansiteApp.VersionInfo (fullVersionString) where

import           Data.Version
import           Distribution.VcsRevision.Git
import           Language.Haskell.TH.Syntax
import           Paths_pansite

gitVersionString :: String
gitVersionString = $(do
    v <- qRunIO getRevision
    lift $ case v of
        Nothing -> []
        Just (commit, True)  -> commit ++ " (locally modified)"
        Just (commit, False) -> commit)

fullVersionString :: String
fullVersionString = case gitVersionString of
    [] -> showVersion version
    v -> showVersion version ++ "." ++ v
