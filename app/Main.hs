module Main where

import Transaction
import Data.ByteString.Lazy as BS
import Data.Csv
import Data.Vector

main :: IO ()
main = do
    bs <- BS.readFile "test/example.csv"
    let result = decodeByName bs :: Either String (Header,Vector Transaction)
    print result
