module Utils
    (strToInt,
     intToStr) where

import Data.List (genericIndex)
import Data.Char (digitToInt,intToDigit)
import Numeric (readInt,showIntAtBase)

strToInt :: (Integral a) => a -> String -> a
strToInt base string = fst $ head $ readInt base (`elem` (genericIndex allowedDigits base)) digitToInt string

intToStr :: (Integral a) => a -> a -> String
intToStr base value = showIntAtBase base intToDigit value ""

allowedDigits :: [String]
allowedDigits = ["", -- base 0
                 "0", -- base 1
                 "01", -- base 2
                 "012", -- base 3
                 "0123", -- base 4
                 "01234", -- base 5
                 "012345", -- base 6
                 "0123456", -- base 7
                 "01234567", -- base 8
                 "012345678", -- base 9
                 "0123456789", -- base 10
                 "0123456789A", -- base 11
                 "0123456789AB", -- base 12
                 "0123456789ABC", -- base 13
                 "0123456789ABCD", -- base 14
                 "0123456789ABCDE", -- base 15
                 "0123456789ABCDEF"] -- base 16
