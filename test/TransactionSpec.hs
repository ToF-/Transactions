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

        it "can be read from a CSV stream" $ do
            let s = "date,label,note,debit,credit\n2022-05-26,\"a transaction label\",,48.07,\n2022-06-27,\"another transaction\",\"with a note\",,17.00\n"
            let ts = fromCSV s
            ts `shouldBe` Right [Transaction (theDay 2022 05 26) (label "a transaction label") Nothing Nothing (Just (money 48.07)) 
                                ,Transaction (theDay 2022 06 27) (label "another transaction") (Just (note "with a note")) (Just (money 17.00)) Nothing]




