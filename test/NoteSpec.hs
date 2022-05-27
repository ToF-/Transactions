{-# LANGUAGE OverloadedStrings #-}
module NoteSpec where

import Test.Hspec
import Note


spec :: SpecWith ()
spec = do
    describe "a note" $ do
        it "can be created from a string" $ do
            let l = note "a note"
            show l `shouldBe` "a note"

        it "can be read from a string" $ do
            let l = read "a note" :: Note
            show l `shouldBe` "a note"
