{-# LANGUAGE OverloadedStrings #-}
module DateSpec where

import Test.Hspec
import Date


spec :: SpecWith ()
spec = do
    describe "a date" $ do
        it "can be created from a year, month and day" $ do
            let d = theDay 2022 05 26
            show d `shouldBe` "2022-05-26"

        it "can be read from a string" $ do
            let d = read "2022-05-26" :: Date
            show d `shouldBe` "2022-05-26"

