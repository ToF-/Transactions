{-# LANGUAGE OverloadedStrings #-}
module TransactionSpec where

import Test.Hspec
import Transaction
import Date
import Label
import Note
import Money


spec:: SpecWith ()
spec = do
    describe "test harness" $ do
        it "should have tests" $ do
            2+2 `shouldBe` 4

    describe "A transaction" $ do
        it "has a date, label, note, credit and debit" $ do
            let t = transaction (theDay 2022 05 26) (label "new transaction") (Just (note "from test")) (Just (money 48.07)) Nothing
            transaction_date t   `shouldBe` (theDay 2022 05 26)
            transaction_label t  `shouldBe` (label "new transaction")
            transaction_note t   `shouldBe` Just (note "from test")
            transaction_credit t `shouldBe` Just (money 48.07)
            transaction_debit t  `shouldBe` Nothing


