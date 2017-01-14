{-|
Module      : SafeUtilSpec
Description : Tests for Pansite.SafeUtil
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module SafeUtilSpec (spec) where

import           Pansite
import           Test.Hspec

spec :: Spec
spec = do
    describe "Pansite.SafeUtil" $ do
        describe "isIndexInRange" $ do
            context "when index is out of range" $
                it "should evaluate to False" $ do
                    isIndexInRange [] (-1) `shouldBe` False
                    isIndexInRange [] 0 `shouldBe` False
                    isIndexInRange [] 1 `shouldBe` False
                    isIndexInRange [1] (-1) `shouldBe` False
                    isIndexInRange [1] 1 `shouldBe` False
            context "when index is in range" $
                it "should evaluate to True" $ do
                    isIndexInRange [1] 0 `shouldBe` True

        describe "atIndex" $ do
            context "when index is out of range" $
                it "should evaluate to Nothing" $ do
                    ([] :: [Int]) `atIndex` (-1) `shouldBe` Nothing
                    ([] :: [Int]) `atIndex` 0 `shouldBe` Nothing
                    ([] :: [Int]) `atIndex` 1 `shouldBe` Nothing
                    [1] `atIndex` (-1) `shouldBe` Nothing
                    [1] `atIndex` 1 `shouldBe` Nothing
            context "when index is in range" $
                it "should evaluate to Just a value" $ do
                    [1] `atIndex` 0 `shouldBe` Just 1
