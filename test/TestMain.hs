{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Hspec
import qualified TestConfig

main :: IO ()
main = hspec $ do
#if __GLASGOW_HASKELL__ < 708
    return ()
#else
    describe "Config" TestConfig.spec
#endif
