#!/usr/bin/env stack
-- stack runghc --package shelly

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import qualified Data.Text as T
import System.Environment
import qualified System.FilePath as F
import System.IO
default (T.Text)

main' args = shelly $ do
    cmd "pandoc" "--table-of-contents" "--output=stdout" "--to=html" "--from=markdown" "--highlight-style=tango"
     "--css=http://jasonm23.github.com/markdown-css-themes/markdown7.css"
     "--css=https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" filename
    where
        filename = head args

main = main' =<< getArgs
