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
    Right bmp  <- readBMP ("bmp/" ++ filename)
    let rgba   =  unpackBMPToRGBA32 bmp
    let (width, height) = bmpDimensions bmp
    let rgbaNegative = B.map toNegativeChar rgba
    let newBMP = packRGBA32ToBMP24 width height rgbaNegative
    let output = takeWhile (\x -> x /= '.') filename
    writeBMP ("bmp/ " ++ output ++ "Negative.BMP") newBMP
