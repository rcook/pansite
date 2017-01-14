{-|
Module      : Pansite.TestServer
Description : Types for Pansite test server
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.TestServer.Types
    ( Port
    , RequestHandler
    , RequestHandlerResult (..)
    , ServerConfig (..)
    , ServerInfo (..)
    ) where

import           Control.Concurrent.STM
import           Data.ByteString.Lazy
import           Data.Text
import           Pansite.Event

type Port = Int

type RequestHandler = [Text] -> STM RequestHandlerResult

data RequestHandlerResult =
    Valid ByteString |
    Done ByteString |
    BadState |
    BadRequest
    deriving Show

data ServerConfig = ServerConfig Port deriving Show

data ServerInfo = ServerInfo TMEvent (TVar Int)
