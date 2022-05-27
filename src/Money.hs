module Money 
    where

import Data.Fixed
import Data.Text

data Money = Money { value :: Centi }
    deriving Eq

money :: Double -> Money
money = Money . MkFixed . round . (* 100)

instance Show Money where
    show m = show (value m)

instance Read Money where
    readsPrec _ = \s -> case uncons (pack s) of
                       Just ('-', rest) -> parseNegativeMoney (unpack rest)
                       _                -> parsePositiveMoney s


instance Num Money where
    negate (Money v) = (Money (negate v))
    (Money v) + (Money w) = Money (v+w)

parseNegativeMoney :: String -> [(Money, String)]
parseNegativeMoney s = (Prelude.map (\(m,r) -> (negate m, r))) $ parsePositiveMoney s

parsePositiveMoney :: String -> [(Money, String)]
parsePositiveMoney s = Prelude.map (\(d,r) -> (money d, r)) $ reads s
