{-|
Module      : Pansite.IO.Types
Description : I/O types for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.IO.Types
    ( HandleState (..)
    ) where

import           System.IO

data HandleState = HandleState BufferMode Bool
