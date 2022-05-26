{-# LANGUAGE OverloadedStrings #-}
module TransactionSpec where

import Test.Hspec
-- import qualified Data.Text.Lazy.Encoding as TE


spec:: SpecWith ()
spec = do
    describe "test harness" $ do
        it "should have tests" $ do
            2+2 `shouldBe` 4

--     describe "A transaction" $ do
--         it "has a date, label, note, credit and debit" $ do
--             let t = transaction (theDay 2022 05 26) (label "new transaction") (note "from test") (money 48.07) (money 0.00)
--             transaction_date t `shouldBe` (theDay 2022 05 26)
--             transaction_label t `shouldBe` (label "new transaction")
--             transaction_note t `shouldBe` (note "from test")
--             transaction_credit `shouldBe` (money 48.07)
--             transaction_debit  `shouldBe` (money 0.00)
-- 

