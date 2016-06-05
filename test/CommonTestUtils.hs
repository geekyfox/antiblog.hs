{-# LANGUAGE CPP #-}

module CommonTestUtils where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Antiblog.Config
import Data.String
import Data.Text
import Test.QuickCheck

instance Arbitrary BaseURL where
    arbitrary = fromString <$> arbitrary

instance Arbitrary SiteTitle where
    arbitrary = fromString <$> arbitrary

instance Arbitrary SystemName where
    arbitrary = fromString <$> arbitrary

instance Arbitrary Text where
    arbitrary = fromString <$> arbitrary

instance Arbitrary Remote where
    arbitrary = Remote <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Local where
    arbitrary = Local <$> arbitrary <*> arbitrary <*> arbitrary
            <*> arbitrary <*> arbitrary <*> arbitrary
            <*> arbitrary <*> arbitrary <*> arbitrary
            <*> arbitrary <*> arbitrary <*> arbitrary
            <*> arbitrary <*> arbitrary <*> arbitrary
