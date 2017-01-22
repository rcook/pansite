{-|
Module      : Pansite.Graph
Description : DAG for Pansite
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Pansite.Graph
    ( Edge (..)
    , mkGraph
    , topoSort
    ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

data Edge a = Edge a a
data Graph a = Graph (Map a Int) deriving Show

mkGraph :: Ord a => [Edge a] -> Graph a
mkGraph es =
    Graph $ foldr (\(Edge x0 x1) m -> insertNew x1 (insertNew x0 m)) Map.empty es
    where
        insertNew x m
            | x `Map.member` m = m
            | otherwise = Map.insert x (Map.size m) m

topoSort (Graph m) =
    let states = Map.fromList (map (\idx -> (idx, False)) [0 .. Map.size m - 1])
    in foldr (\idx states -> topoSortHelper idx states) states (Map.keys states)

topoSortHelper :: Int -> Map Int Bool -> Map Int Bool
topoSortHelper idx = Map.insert idx True
