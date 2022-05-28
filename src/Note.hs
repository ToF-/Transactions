module Note
    where

import Data.Text
import Data.Text.Encoding ( decodeUtf8 )
import Data.Csv
import Control.Applicative

data Note = Note Text
    deriving Eq

note :: String -> Note
note = Note . pack

instance Show Note where
    show (Note t) = unpack t

instance Read Note where
    readsPrec _ = \s -> [(note s,"")]

instance FromField Note where
    parseField f = case unpack (strip (decodeUtf8 f)) of
                     "" -> Control.Applicative.empty
                     s -> pure (Note (pack s))
