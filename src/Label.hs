module Label
    where

import Data.Text

data Label = Label Text

label :: String -> Label
label = Label . pack

instance Show Label where
    show (Label t) = unpack t
