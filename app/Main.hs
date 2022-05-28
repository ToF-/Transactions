module Main where

import Transaction
import System.Environment
import Data.ByteString.Lazy as BS
import Data.Csv
import Data.Vector

main :: IO ()
main = do
    args <- getArgs
    case Prelude.length args of
      0 -> Prelude.putStrLn "no file given"
      _ -> do
        bs <- BS.readFile (args!!0)
        let result = fromCSV bs 
        case result of
          Right ts -> do
                Prelude.putStrLn $ unlines $ Prelude.map show ts
                let (debit, credit) = summarize ts
                Prelude.putStrLn $ "debit: " <> show debit <> "\tcredit: " <> show credit
          Left msg -> Prelude.putStrLn msg
