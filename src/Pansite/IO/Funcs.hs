{-|
Module      : Pansite.IO.Funcs
Description : I/O functions for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.IO.Funcs
    ( bracketHandle
    , hGetState
    , hSetState
    ) where

import           Control.Exception
import           System.IO
import           Pansite.IO.Types

-- |Get state of handle
hGetState :: Handle -> IO HandleState
hGetState h = do
    bufferMode <- hGetBuffering h
    isEcho <- hGetEcho h
    return $ HandleState bufferMode isEcho

-- |Set state of handle
hSetState :: Handle -> HandleState -> IO ()
hSetState h (HandleState mode isEcho) = do
    hSetEcho h isEcho
    hSetBuffering h mode

-- |Save and restore state of handle before and after running action
bracketHandle :: Handle -> (IO a -> IO a)
bracketHandle h action = bracket (hGetState h) (hSetState h) (const action)
