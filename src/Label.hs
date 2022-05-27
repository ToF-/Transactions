module Label
    where

import Data.Text

data Label = Label Text
    deriving Eq

label :: String -> Label
label = Label . pack

instance Show Label where
    show (Label t) = unpack t

instance Read Label where
    readsPrec _ = \s -> [(label s,"")] 
