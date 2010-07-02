module Utils
    (toLefts,
     toRights,
     maybeToEither,
     gatherEithers,
     digitToBase2,
     intToBinaryString,
     intToDecString,
     intToHexString) where     

import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.Char (intToDigit)
    
import Numeric (showIntAtBase)

toLefts :: (a -> c) -> Either a b -> Either c b
toLefts f (Left x) = Left $ f x
toLefts f (Right x) = Right x

toRights :: (b -> c) -> Either a b -> Either a c
toRights f (Left x) = Left x
toRights f (Right x) = Right $ f x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither a Nothing = Left a

gatherEithers :: [Either String a] -> Either String [a]
gatherEithers eithers =
    case partitionEithers eithers of
      ([],results) -> Right results
      (errorMessages,_) -> Left (intercalate "\n" errorMessages)

digitToBase2 :: String -> Char -> Either String String
digitToBase2 "b" '0' = Right "0"
digitToBase2 "b" '1' = Right "1"
digitToBase2 "b" digit = Left $ "Invalid binary digit " ++ show digit ++ " in literal!"
digitToBase2 "o" '0' = Right "000"
digitToBase2 "o" '1' = Right "001"
digitToBase2 "o" '2' = Right "010"
digitToBase2 "o" '3' = Right "011"
digitToBase2 "o" '4' = Right "100"
digitToBase2 "o" '5' = Right "101"
digitToBase2 "o" '6' = Right "110"
digitToBase2 "o" '7' = Right "111"
digitToBase2 "o" digit = Left $ "Invalid octal digit " ++ show digit ++ " in literal!"
digitToBase2 "h" '0' = Right "0000"
digitToBase2 "h" '1' = Right "0001"
digitToBase2 "h" '2' = Right "0010"
digitToBase2 "h" '3' = Right "0011"
digitToBase2 "h" '4' = Right "0100"
digitToBase2 "h" '5' = Right "0101"
digitToBase2 "h" '6' = Right "0110"
digitToBase2 "h" '7' = Right "0111"
digitToBase2 "h" '8' = Right "1000"
digitToBase2 "h" '9' = Right "1001"
digitToBase2 "h" 'A' = Right "1010"
digitToBase2 "h" 'B' = Right "1011"
digitToBase2 "h" 'C' = Right "1100"
digitToBase2 "h" 'D' = Right "1101"
digitToBase2 "h" 'E' = Right "1110"
digitToBase2 "h" 'F' = Right "1111"
digitToBase2 "h" 'a' = Right "1010"
digitToBase2 "h" 'b' = Right "1011"
digitToBase2 "h" 'c' = Right "1100"
digitToBase2 "h" 'd' = Right "1101"
digitToBase2 "h" 'e' = Right "1110"
digitToBase2 "h" 'f' = Right "1111"
digitToBase2 "h" digit = Left $ "Invalid hex digit " ++ show digit ++ " in literal!"

intToBinaryString :: Int -> Int -> String
intToBinaryString = intToBaseString 2

intToDecString :: Int -> Int -> String
intToDecString = intToBaseString 10

intToHexString :: Int -> Int -> String
intToHexString = intToBaseString 16

intToBaseString :: Int -> Int -> Int -> String
intToBaseString base size a
    | base <= 16 =
        let baseString = showIntAtBase base intToDigit a ""
        in if length baseString <= size
           then (replicate (size - (length baseString)) '0') ++ baseString
           else error $ "FATAL: intToBinaryString " ++ show size ++ " " ++ show a ++ " can't produce proper string!"
    | otherwise =
        error $ "FATAL: intToBaseString " ++ show base ++ " " ++ show size ++ " " ++ show a ++ " can't handle that base!"
