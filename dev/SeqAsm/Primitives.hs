module Primitives
    (primiBang,
     primiAdd,
     primiSub,
     primiMul,
     primiDiv,
     primiMod,
     primiCat) where

import Data.List (genericLength,genericDrop,genericReplicate)
import Utils (strToInt,intToStr)

getArg0n :: [String] -> Integer
getArg0n = strToInt 2 . head

getArg1n :: [String] -> Integer
getArg1n = strToInt 2 . head . tail

getArg0s :: [String] -> String
getArg0s = head

getArg1s :: [String] -> String
getArg1s = head . tail

primiArithmetic :: (Integer -> Integer -> Integer) -> [String] -> String
primiArithmetic op arguments =
    let arg0 = getArg0n arguments
        arg1 = getArg1n arguments
    in intToStr 2 (arg0 `op` arg1)
                      
primiBang :: [String] -> String
primiBang arguments =
    let arg0 = getArg0n arguments
        arg1 = getArg1s arguments
        diff = genericLength arg1 - arg0
    in if diff >= 0
       then genericDrop diff arg1
       else genericReplicate (abs diff) '0' ++ arg1

primiAdd :: [String] -> String
primiAdd = primiArithmetic (+)

primiSub :: [String] -> String
primiSub = primiArithmetic (-)

primiMul :: [String] -> String
primiMul = primiArithmetic (*)

primiDiv :: [String] -> String
primiDiv = primiArithmetic (div)

primiMod :: [String] -> String
primiMod = primiArithmetic (mod)

primiCat :: [String] -> String
primiCat arguments =
    let arg0 = getArg0s arguments
        arg1 = getArg1s arguments
    in arg0 ++ arg1
