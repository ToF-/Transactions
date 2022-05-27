{-# LANGUAGE OverloadedStrings #-}
module Transaction where

import Date
import Label
import Note
import Money

import Data.ByteString.Lazy
import Data.Csv
import Data.Vector
import Control.Applicative

data Transaction = Transaction {
    transaction_date   :: Date,
    transaction_label  :: Label,
    transaction_note   :: Maybe Note,
    transaction_credit :: Maybe Money,
    transaction_debit  :: Maybe Money }
    deriving (Eq,Show)

instance FromNamedRecord Transaction where
    parseNamedRecord rec =
        Transaction 
          <$> rec .: "date"
          <*> rec .: "label"
          <*> (rec .: "note" :: Parser (Maybe Note))
          <*> ((Just <$> Data.Csv.lookup rec "credit") <|> pure Nothing)
          <*> ((Just <$> Data.Csv.lookup rec "debit") <|> pure Nothing)


transaction :: Date -> Label -> Maybe Note -> Maybe Money -> Maybe Money -> Transaction
transaction = Transaction 

fromCSV :: ByteString -> Either String [Transaction]
fromCSV bs = (toList . snd) <$> decodeByName bs 
