module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs --expects *.bmp file
    bmpToNegative $ head args
