{-# LANGUAGE OverloadedStrings #-}
module Transaction where

import Date
import Label
import Note
import Money

import Data.ByteString.Lazy
import Data.Csv
import Data.Vector

data Transaction = Transaction {
    transaction_date   :: Date,
    transaction_label  :: Label,
    transaction_note   :: Maybe Note,
    transaction_credit :: Maybe Money,
    transaction_debit  :: Maybe Money }
    deriving (Eq,Show)

instance FromNamedRecord Transaction where
    parseNamedRecord m =
        Transaction 
          <$> m .: "date"
          <*> m .: "label"
          <*> (m .: "note" :: Parser (Maybe Note))
          <*> (m .: "credit" :: Parser (Maybe Money))
          <*> (m .: "debit"  :: Parser (Maybe Money))


transaction :: Date -> Label -> Maybe Note -> Maybe Money -> Maybe Money -> Transaction
transaction = Transaction 

fromCSV :: ByteString -> Either String [Transaction]
fromCSV bs = (toList . snd) <$> decodeByName bs 
