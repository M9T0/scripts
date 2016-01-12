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

readFile' :: String -> String -> IO String
readFile' cp path = do
    h <- openFile path ReadMode
    enc <- mkTextEncoding cp
    hSetEncoding h enc
    hGetContents h

writeFile' :: String -> String -> String -> IO ()
writeFile' cp path content = do
    h <- openFile path WriteMode
    enc <- mkTextEncoding cp
    hSetEncoding h enc
    hPutStr h content
    hFlush h
    hClose h

main :: IO ()
main = do
    [path, out] <- getArgs
    content <- readFile' "UTF-8" path
    writeFile' "UTF-8" out (unlines (csv2mdtable $ lines content))
