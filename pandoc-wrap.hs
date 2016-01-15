#!/usr/bin/env stack
-- stack runghc --package shelly

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import qualified Data.Text as T
import System.Environment (getArgs)
import System.FilePath (dropExtension)
default (T.Text)

main' args = shelly $ do
    cmd "pandoc" "--table-of-contents" (T.pack ("--output=" ++ outname)) "--to=html" "--from=markdown" "--highlight-style=tango"
     "--css=http://jasonm23.github.com/markdown-css-themes/markdown7.css"
     "--css=https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" "-s" filename
    where
        filename = head args
        outname = dropExtension filename ++ ".html"

main :: IO T.Text
main = main' =<< getArgs
