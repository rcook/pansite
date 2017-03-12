module PansiteSpec.Config.UtilSpec (spec) where

import           Data.List
import           Pansite
import           Test.Hspec
import           Test.QuickCheck

genRoutePath :: Gen String
genRoutePath = intercalate "/" <$> listOf (listOf $ elements ['a'..'z'])

unsplitRoutePathProp :: Property
unsplitRoutePathProp = forAll genRoutePath $ \p -> intercalate "/" (splitRoutePath p) == p

spec :: Spec
spec = do
    describe "splitRoutePath" $
        it "satisfies unsplitRoutePathProp" $ property unsplitRoutePathProp
