#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad (join)
import System.Environment
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, mkTextEncoding)

-- | 行単位の処理
csv2mdtable' :: String -> String
csv2mdtable' s =
    "|" ++ map repl s ++ "|"
    where
        repl ',' = '|'
        repl x = x

-- | CSVからmarkdownの表に変換
csv2mdtable :: [String] -> [String]
csv2mdtable s =
    let x:xs = map csv2mdtable' s in
    let columns = length [y | y <- x, y == '|'] in
    let deli = concat (replicate columns "|----") ++ "|" in
    x:deli:xs

-- ファイル読み込み処理
readFile' :: String -> String -> IO String
readFile' cp path = join $ hGetContents <$> openFile path ReadMode <* (flip hSetEncoding <$> mkTextEncoding cp)

-- | CSVファイルからmarkdown形式のテーブルを出力する
main :: IO ()
main = getArgs >>= (readFile' "UTF-8" . head) >>= putStr . unlines . csv2mdtable . lines
