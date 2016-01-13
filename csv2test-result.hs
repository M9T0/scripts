#!/usr/bin/env stack
-- stack runghc --package parsec

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import System.Environment
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, mkTextEncoding)
import Text.Parsec
import Text.Parsec.Error

-- ! 結果
data TestResult = OK | NG | Yet | Pending
    deriving (Enum, Eq, Ord, Read)
instance Show TestResult where
    show OK = "OK"
    show NG = "NG"
    show Yet = "未実施"
    show Pending = "保留"
readResult :: String -> TestResult
readResult "OK" = OK
readResult "NG" = NG
readResult "保留" = Pending
readResult "未実施" = Yet
readResult _ = Yet

-- | 行
data Row = Row {
    subject :: String,
    result :: TestResult,
    remark :: String
}
outputRow :: Row -> String
outputRow row =
    "|" ++ s ++ "|" ++ r ++ "|"  ++ m ++ "|"
    where
        s = subject row
        r = show $ result row
        m = remark row
createRow :: [String] -> Row
--createRow xs = Row { subject = head xs, result = read (xs !! 1), remark = xs !! 2 }
createRow xs = Row { subject = head xs, result = readResult (xs !! 1), remark = xs !! 2 }

-- | テーブル
data Node = Table { name :: String, rows :: [Row] } | Sheet { name :: String, nodes :: [Node] }
outputTable :: Node -> [String]
outputTable table = [t, h, d] ++ c ++ ["\n"]
    where
        t = "## " ++ name table ++ "\n"
        h = "|項目|結果|備考|"
        d = "|----|----|----|"
        c = map outputRow (rows table)
createTable :: String -> Node
createTable n = Table { name = n, rows = [] }
addRow :: Node -> [Row] -> Node
addRow table rs = Table { name = name table, rows = rows table ++ rs}

-- | 全体
outputSheet :: Node -> [String]
outputSheet sheet = h:concatMap outputTable c
    where
        t = name sheet
        h = "# " ++ t ++ "\n"
        c = nodes sheet
addTable :: Node -> Node -> Node
addTable t s = Sheet { name = name s, nodes = n }
    where
        n = if name t `elem` [name c | c <- nodes s] then
                addRow (head [c | c <- nodes s, name c == name t]) (rows t):[c | c <- nodes s, name c /= name t]
            else
                t:nodes s
addRows :: String -> [Row] -> Node -> Node
addRows n rs = addTable t
    where
        t = Table { name = n, rows = rs }

-- | Sheet生成
createSheet :: String -> [[String]] -> Node
createSheet s src = rs
    where
        empty = Sheet { name = s, nodes = [] }
        ts = map createTable [x | [x,_] <- src]
        ss = foldr addTable empty ts
        rs = foldr (\p -> addRows (fst p) [snd p]) ss [(x,createRow xs) | x:xs <- src]


-- | CSVファイル構造定義
csvStruct = endBy line eol
line = sepBy cell $ char ','
cell = many $ noneOf ",\n"
eol = char '\n'
parseCsv :: String -> Either ParseError [[String]]
parseCsv = parse csvStruct "* ParseError *"

-- | ファイル読み込み処理
readFile' :: String -> String -> IO String
readFile' cp path = do
    h <- openFile path ReadMode
    enc <- mkTextEncoding cp
    hSetEncoding h enc
    hGetContents h

-- | メイン処理
load :: Either ParseError [[String]] -> [String]
load = either (map messageString . errorMessages) (outputSheet . createSheet "test")

-- | テスト結果CSVから表示用markdownを生成
main :: IO ()
main = do
    path <- getArgs
    src <- readFile' "UTF-8" (head path)
    mapM_ putStrLn (load $ parseCsv src)
