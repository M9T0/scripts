#!/usr/bin/env stack
-- stack runghc --package parsec

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import System.Environment
import System.FilePath
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, mkTextEncoding, hPutStr, withFile)
import Text.Parsec
import Text.Parsec.Error

-- ! 結果
data TestResult = OK | NG | Yet | Pending
    deriving (Enum, Eq, Ord)
instance Show TestResult where
    show OK = "<span class=\"label label-success\">OK</span>"
    show NG = "<span class=\"label label-danger\">NG</span>"
    show Yet = "<span class=\"label label-default\">未実施</span>"
    show Pending = "<span class=\"label label-default\">保留</span>"
instance Read TestResult where
    readsPrec _ = readResult
-- | 結果文字列読み込み
readResult :: String -> [(TestResult, String)]
readResult "OK" = [(OK, "" )]
readResult "NG" = [(NG, "")]
readResult "保留" = [(Pending, "")]
readResult "未実施" = [(Yet, "" )]
readResult _ = [(Yet, "")]

-- | 行
data Row = Row { subject :: String, content :: String, result :: TestResult, remark :: String }
instance Show Row where
    show row = "|" ++ s ++ "|" ++ c ++ "|" ++ r ++ "|"  ++ m ++ "|"
        where
            s = subject row
            c = content row
            r = show $ result row
            m = remark row
-- | 行格納ノード
data Node = Table { name :: String, rows :: [Row] } | Sheet { name :: String, nodes :: [Node] }
instance Show Node where
    show (Table n r) = unlines ([t, h, d] ++ c ++ ["\n"])
        where
            t = "## " ++ n ++ "\n"
            h = "|項目|内容|結果|備考|"
            d = "|----|----|----|----|"
            c = map show r
    show (Sheet n c) = h ++ concatMap show c
        where
            h = "# " ++ n ++ "\n"

-- | 行処理
createRow :: [String] -> Row
createRow xs = Row { subject = head xs, content = xs !! 1, result = read (xs !! 2), remark = xs !! 3 }

-- | テーブル処理
createTable :: String -> Node
createTable n = Table { name = n, rows = [] }

-- | 行追加
addRow :: Node -> [Row] -> Node
addRow table rs = Table { name = name table, rows = rs ++ rows table}

-- | テーブル追加
addTable :: Node -> Node -> Node
addTable t s = Sheet { name = name s, nodes = n }
    where
        n = if name t `elem` [name c | c <- nodes s] then
                addRow (head [c | c <- nodes s, name c == name t]) (rows t):[c | c <- nodes s, name c /= name t]
            else
                t:nodes s

-- | 行追加
addRows :: String -> [Row] -> Node -> Node
addRows n rs = addTable t
    where
        t = Table { name = n, rows = rs }

-- | Sheet生成
createSheet :: String -> [[String]] -> Node
createSheet s src = rs
    where
        empty = Sheet { name = s, nodes = [] }
        woHeader = drop 1 src
        ts = map createTable [x | [x,_] <- woHeader]
        ss = foldr addTable empty ts
        rs = foldr (\p -> addRows (fst p) [snd p]) ss [(x,createRow xs) | x:xs <- woHeader]

-- | CSVファイル構造定義
csvStruct = endBy line eol
line = sepBy cell $ char ','
cell = many $ noneOf ",\n"
eol = char '\n'
parseCsv :: String -> Either ParseError [[String]]
parseCsv = parse csvStruct "* ParseError *"

-- | ファイル読み込み処理
readFile' :: FilePath -> String -> IO String
readFile' path cp = do
    h <- openFile path ReadMode
    enc <- mkTextEncoding cp
    hSetEncoding h enc
    hGetContents h

writeFile' :: FilePath -> String -> String -> IO ()
writeFile' path str cp =
    withFile path WriteMode $ \h -> mkTextEncoding cp >>= hSetEncoding h >> hPutStr h str

-- | メイン処理
load :: String -> Either ParseError [[String]] -> [String]
load s = either (map messageString . errorMessages) (lines . show . createSheet s)

-- | テスト結果CSVから表示用markdownを生成
main :: IO ()
main = do
    path:_ <- getArgs
    src <- readFile' path "CP932"
    writeFile' (dropExtension path ++ ".md") (unlines (load (dropExtension path) (parseCsv src))) "UTF-8"
