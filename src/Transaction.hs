{-# LANGUAGE OverloadedStrings #-}
module Transaction where

import Date
import Label
import Note
import Money

import Data.ByteString.Lazy
import Data.Csv
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Maybe as M
import Control.Applicative

data Transaction = Transaction {
    transaction_date   :: Date,
    transaction_label  :: Label,
    transaction_note   :: Maybe Note,
    transaction_debit  :: Maybe Money,
    transaction_credit :: Maybe Money }
    deriving (Eq,Show)

instance FromNamedRecord Transaction where
    parseNamedRecord rec =
        Transaction 
          <$> rec .: "date"
          <*> rec .: "label"
          <*> ((Just <$> Data.Csv.lookup rec "note") <|> pure Nothing)
          <*> ((Just <$> Data.Csv.lookup rec "debit") <|> pure Nothing)
          <*> ((Just <$> Data.Csv.lookup rec "credit") <|> pure Nothing)


transaction :: Date -> Label -> Maybe Note -> Maybe Money -> Maybe Money -> Transaction
transaction = Transaction 

fromCSV :: ByteString -> Either String [Transaction]
fromCSV bs = (V.toList . snd) <$> decodeByName bs 

summarize :: [Transaction] -> (Money, Money)
summarize ts = (dt,ct) 
    where
        ct = Prelude.foldl (+) (Money 0) (M.catMaybes $ L.map transaction_credit ts)
        dt = Prelude.foldl (+) (Money 0) (M.catMaybes $ L.map transaction_debit ts)
