module Transaction where

import Date
import Label
import Note
import Money

data Transaction = Transaction {
    transaction_date   :: Date,
    transaction_label  :: Label,
    transaction_note   :: Note,
    transaction_credit :: Money,
    transaction_debit  :: Money }

transaction :: Date -> Label -> Note -> Money -> Money -> Transaction
transaction = Transaction 
