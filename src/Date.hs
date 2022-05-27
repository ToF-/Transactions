module Date
    where

import Data.Time
import Data.Csv
import Data.ByteString    ( ByteString )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Text          ( unpack )

data Date = Date Day
    deriving Eq

instance Show Date where
    show (Date d) = show d

instance Read Date where
    readsPrec _ = \s -> case parse s :: Maybe Day of
                          Nothing -> []
                          Just d  -> [(Date d,"")]

instance FromField Date where
    parseField = parseDate

parseDate :: ByteString -> Parser Date
parseDate = fmap Date . parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack . decodeUtf8

theDay :: Integer -> Int -> Int -> Date
theDay y m d = Date (fromGregorian y m d)

parse = parseTimeM True defaultTimeLocale "%Y-%m-%d"
