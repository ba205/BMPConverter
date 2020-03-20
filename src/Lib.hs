module Lib
    ( bmpToNegative
    ) where

import System.IO
import Codec.BMP
import Data.ByteString.Char8 as B hiding (takeWhile)
import Data.Char

toNegativeChar :: Char -> Char
toNegativeChar c
  | isLatin1 c = (chr . (\x -> 255 - x) . ord) c
  | otherwise = c

bmpToNegative :: FilePath -> IO ()
bmpToNegative filename = do
  bmp <- B.readFile ("bmp/" ++ filename)
  let bmpNegative = B.map toNegativeChar (B.drop 50 bmp)
  let output = takeWhile (\x -> x /= '.') filename
  B.writeFile ("bmp/ " ++ output ++ "Negative.BMP") (B.append (B.take 50 bmp) bmpNegative)
