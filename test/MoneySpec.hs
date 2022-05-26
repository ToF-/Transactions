{-# LANGUAGE OverloadedStrings #-}
module MoneySpec where

import Test.Hspec
import Money

spec :: SpecWith ()
spec = do
    describe "money" $ do
        it "can be created from a double amount" $ do
            let m = money 48.7
            show m `shouldBe` "48.70"

        it "can be read from a string" $ do
            let m = read "-48.07" :: Money
            let n = read "17" :: Money
            show m `shouldBe` "-48.07"
            show n `shouldBe` "17.00"

        it "can be added with another money" $ do
            let m = money 48.07
            let n = money 20.05
            show (m+n) `shouldBe` "68.12"
