module Note
    where

import Data.Text

data Note = Note Text
    deriving Eq

note :: String -> Note
note = Note . pack

instance Show Note where
    show (Note t) = unpack t

instance Read Note where
    readsPrec _ = \s -> [(note s,"")]
