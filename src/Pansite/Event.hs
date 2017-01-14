{-|
Module      : Pansite.Event
Description : Event functions for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.Event
    ( MEvent
    , TMEvent
    , isSetMEvent
    , isSetTMEvent
    , newMEvent
    , newTMEventIO
    , setMEvent
    , setTMEvent
    , waitMEvent
    , waitTMEvent
    ) where

import Control.Concurrent
            ( MVar
            , isEmptyMVar
            , newEmptyMVar
            , putMVar
            , takeMVar
            )
import Control.Concurrent.STM
            ( STM
            )
import Control.Concurrent.STM.TMVar
            ( TMVar
            , isEmptyTMVar
            , newEmptyTMVarIO
            , putTMVar
            , takeTMVar
            )

type MEvent = MVar ()

isSetMEvent :: MEvent -> IO Bool
isSetMEvent = isEmptyMVar

newMEvent :: IO MEvent
newMEvent = newEmptyMVar

setMEvent :: MEvent -> IO ()
setMEvent = (flip putMVar) ()

waitMEvent :: MEvent -> IO ()
waitMEvent = takeMVar

type TMEvent = TMVar ()

isSetTMEvent :: TMEvent -> STM Bool
isSetTMEvent = isEmptyTMVar

newTMEventIO :: IO TMEvent
newTMEventIO = newEmptyTMVarIO

setTMEvent :: TMEvent -> STM ()
setTMEvent = (flip putTMVar) ()

waitTMEvent :: TMEvent -> STM ()
waitTMEvent = takeTMVar
