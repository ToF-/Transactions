module Label
    where

import Data.Text
import Data.Text.Encoding ( decodeUtf8 )
import Data.Csv
import Control.Applicative

data Label = Label Text
    deriving (Eq,Ord)

label :: String -> Label
label = Label . pack

instance Show Label where
    show (Label t) = unpack t

instance Read Label where
    readsPrec _ = \s -> [(label s,"")] 

instance FromField Label where
    parseField f = case unpack (strip (decodeUtf8 f)) of
                       "" -> Control.Applicative.empty
                       s -> pure (Label (pack s))
