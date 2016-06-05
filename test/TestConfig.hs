module TestConfig (spec) where

import Antiblog.Config
import Skulk.Outcome hiding (describe)
import Test.Hspec
import Test.QuickCheck
import CommonTestUtils()

spec :: Spec
spec = do
    describe "Remote" $ do
        it "serialize is inverse to digest" $ property $ \a ->
            digest (systemName a) (serialize a) `shouldBe` (OK (a :: Remote))
    describe "Local" $ do
        it "serialize is inverse to digest" $ property $ \a ->
            digest (systemName a) (serialize a) `shouldBe` (OK (a :: Local))
        
