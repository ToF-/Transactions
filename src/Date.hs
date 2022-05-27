module Date
    where

import Data.Time

data Date = Date Day
    deriving Eq

instance Show Date where
    show (Date d) = show d

instance Read Date where
    readsPrec _ = \s -> case parse s :: Maybe Day of
                          Nothing -> []
                          Just d  -> [(Date d,"")]

theDay :: Integer -> Int -> Int -> Date
theDay y m d = Date (fromGregorian y m d)

parse = parseTimeM True defaultTimeLocale "%Y-%m-%d"
