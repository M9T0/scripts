#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import System.Environment
import System.IO (IOMode (..), hGetContents, hPutStr, hFlush, hSetEncoding, openFile, hClose, mkTextEncoding)

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
readFile' cp path = do
    h <- openFile path ReadMode
    enc <- mkTextEncoding cp
    hSetEncoding h enc
    hGetContents h

-- | CSVファイルからmarkdown形式のテーブルを出力する
main :: IO ()
main = do
    [path] <- getArgs
    content <- readFile' "UTF-8" path
    putStr $ unlines $ csv2mdtable $ lines content
