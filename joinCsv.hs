#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import System.IO
import System.Environment

-- | 各行を連結
join :: [String] -> [String] -> [String]
join (x:xs) (y:ys) =
    concat [x,",",y]:join xs ys
join (_:_) [] =
    [""]
join [] (_:_) =
    [""]
join [] [] =
    [""]

-- | 文字コード指定読み込み
readFile' :: String -> String -> IO String
readFile' cp path = do
    h <- openFile path ReadMode
    enc <- mkTextEncoding cp
    hSetEncoding h enc
    hGetContents h

-- | CSVファイルをJOINする
main :: IO ()
main = do
    [left, right] <- getArgs
    leftContent <- readFile' "UTF-8" left
    rightContent <- readFile' "UTF-8" right
    putStr $ unlines $ join (lines leftContent) (lines rightContent)
