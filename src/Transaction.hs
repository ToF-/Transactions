{-# LANGUAGE OverloadedStrings #-}
module Transaction where

import Date
import Label
import Note
import Money

import Data.ByteString.Lazy
import Data.Csv
import Text.Printf
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Maybe as M
import Control.Applicative
import Data.Ord

data Transaction = Transaction {
    transaction_date   :: Date,
    transaction_label  :: Label,
    transaction_note   :: Maybe Note,
    transaction_debit  :: Maybe Money,
    transaction_credit :: Maybe Money }
    deriving Eq

instance Show Transaction where
    show t = printf "%08s|%-60s|%-40s|%10s|%10s"
                (show (transaction_date t))
                (show (transaction_label t))
                (M.maybe "" show (transaction_note t))
                (M.maybe "" show (transaction_debit t))
                (M.maybe "" show (transaction_credit t))

instance FromNamedRecord Transaction where
    parseNamedRecord rec =
        Transaction 
          <$> rec .: "date"
          <*> rec .: "label"
          <*> ((Just <$> Data.Csv.lookup rec "note") <|> pure Nothing)
          <*> ((Just <$> Data.Csv.lookup rec "debit") <|> pure Nothing)
          <*> ((Just <$> Data.Csv.lookup rec "credit") <|> pure Nothing)

instance Ord Transaction where
    compare ta tb = case comparing transaction_date ta tb of
                      EQ -> comparing transaction_label ta tb
                      other -> other

transaction :: Date -> Label -> Maybe Note -> Maybe Money -> Maybe Money -> Transaction
transaction = Transaction 

fromCSV :: ByteString -> Either String [Transaction]
fromCSV bs = (V.toList . snd) <$> decodeByName bs 

summarize :: [Transaction] -> (Money, Money)
summarize ts = (dt,ct) 
    where
        ct = Prelude.foldl (+) (Money 0) (M.catMaybes $ L.map transaction_credit ts)
        dt = Prelude.foldl (+) (Money 0) (M.catMaybes $ L.map transaction_debit ts)
