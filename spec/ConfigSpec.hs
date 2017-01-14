{-|
Module      : ConfigSpec
Description : Tests for Pansite.Config
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import           Data.Yaml
import           Pansite
import           Test.Hspec

spec :: Spec
spec = do
    describe "Pansite.Config" $
        describe "Config" $ do
            context "when config is empty" $
                it "should match expected" $
                    encode (Config [] []) `shouldBe`
                        "routes: []\n\
                        \inputs: []\n"

            context "when config has routes" $
                it "should match expected" $
                    encode (Config [Route ["a", "b"] "p", Route ["c", "d"] "q"] []) `shouldBe`
                        "routes:\n\
                        \- path: a/b\n\
                        \  sourcePath: p\n\
                        \- path: c/d\n\
                        \  sourcePath: q\n\
                        \inputs: []\n"

            context "when config has routes and inputs" $
                it "should match expected" $
                    encode (Config [Route ["a", "b"] "p", Route ["c", "d"] "q"] [Input "m" ["n", "o"], Input "p" ["q", "r"]]) `shouldBe`
                        "routes:\n\
                        \- path: a/b\n\
                        \  sourcePath: p\n\
                        \- path: c/d\n\
                        \  sourcePath: q\n\
                        \inputs:\n\
                        \- dependencies:\n\
                        \  - 'n'\n\
                        \  - o\n\
                        \  sourcePath: m\n\
                        \- dependencies:\n\
                        \  - q\n\
                        \  - r\n\
                        \  sourcePath: p\n"
