{-# LANGUAGE OverloadedStrings #-}
module LabelSpec where

import Test.Hspec
import Label


spec :: SpecWith ()
spec = do
    describe "a label" $ do
        it "can be created from a string" $ do
            let l = label "a label"
            show l `shouldBe` "a label"
