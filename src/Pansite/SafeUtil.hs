{-|
Module      : Pansite.SafeUtil
Description : Safe alternatives to Prelude functions for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.SafeUtil
    ( atIndex
    , isIndexInRange
    , removeAtIndex
    , updateAtIndex
    ) where

-- |True if index is in range, False otherwise
isIndexInRange :: [a] -> Int -> Bool
isIndexInRange xs idx = idx >= 0 && idx < length xs

-- |Element of list at given index if index in range
atIndex :: [a] -> Int -> Maybe a
atIndex xs idx
    | isIndexInRange xs idx = Just $ xs !! idx
    | otherwise = Nothing
infixl 9 `atIndex`

-- |List with given element removed if index in range
removeAtIndex :: [a] -> Int -> Maybe [a]
removeAtIndex xs idx
    | isIndexInRange xs idx = let (before, e : after) = splitAt idx xs in Just $ before ++ after
    | otherwise = Nothing

-- |List with result of applying given function to specified element if index in range
updateAtIndex :: [a] -> Int -> (a -> a) -> Maybe [a]
updateAtIndex xs idx f
    | isIndexInRange xs idx = let (before, e : after) = splitAt idx xs in Just $ before ++ f e : after
    | otherwise = Nothing
