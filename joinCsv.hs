#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad
import System.IO
import System.Environment

-- | 各行を連結
joinCsv :: [String] -> [String] -> [String]
joinCsv (x:xs) (y:ys) =
    concat [x,",",y]:joinCsv xs ys
joinCsv (_:_) [] =
    [""]
joinCsv [] (_:_) =
    [""]
joinCsv [] [] =
    [""]

-- | 文字コード指定読み込み
readFile' :: String -> String -> IO String
readFile' cp path = join $ hGetContents <$> openFile path ReadMode <* (flip hSetEncoding <$> mkTextEncoding cp)

-- | CSVファイルをJOINする
main :: IO ()
main = do
    [left, right] <- getArgs
    leftContent <- readFile' "UTF-8" left
    rightContent <- readFile' "UTF-8" right
    putStr $ unlines $ joinCsv (lines leftContent) (lines rightContent)
