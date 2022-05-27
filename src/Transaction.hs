module Transaction where

import Date
import Label
import Note
import Money

data Transaction = Transaction {
    transaction_date   :: Date,
    transaction_label  :: Label,
    transaction_note   :: Maybe Note,
    transaction_credit :: Maybe Money,
    transaction_debit  :: Maybe Money }

transaction :: Date -> Label -> Maybe Note -> Maybe Money -> Maybe Money -> Transaction
transaction = Transaction 
