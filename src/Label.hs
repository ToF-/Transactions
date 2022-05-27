module Label
    where

import Data.Text
import Data.Text.Encoding ( decodeUtf8 )
import Data.Csv

data Label = Label Text
    deriving Eq

label :: String -> Label
label = Label . pack

instance Show Label where
    show (Label t) = unpack t

instance Read Label where
    readsPrec _ = \s -> [(label s,"")] 

instance FromField Label where
    parseField = pure . Label . strip . decodeUtf8
