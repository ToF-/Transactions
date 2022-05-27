module Note
    where

import Data.Text
import Data.Text.Encoding ( decodeUtf8 )
import Data.Csv

data Note = Note Text
    deriving Eq

note :: String -> Note
note = Note . pack

instance Show Note where
    show (Note t) = unpack t

instance Read Note where
    readsPrec _ = \s -> [(note s,"")]

instance FromField Note where
    parseField = pure . Note . strip . decodeUtf8
