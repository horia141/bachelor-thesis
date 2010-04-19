module Primitives
    (primitives) where

import Defines(SeqSourceInfo(..),SeqDefine(..),SeqModule(..),SeqOpFixity(..),SeqOpAssoc(..))
import Data.List (genericLength,genericDrop,genericReplicate)
import Utils (strToInt,intToStr)

primitives :: SeqModule
primitives = Module {
               moduleName = "primitives",
               moduleExports = ["!","*","/","%","+","-","~"],
               moduleImports = [],
               moduleDefines = [PrimOper "!" ["op0","op1"] (primBang)    (OpInfix,OpRight,0),
                                PrimOper "*" ["op0","op1"] (primBin (*)) (OpInfix,OpLeft, 1),
                                PrimOper "/" ["op0","op1"] (primBin div) (OpInfix,OpLeft, 1),
                                PrimOper "%" ["op0","op1"] (primBin mod) (OpInfix,OpLeft, 1),
                                PrimOper "+" ["op0","op1"] (primBin (+)) (OpInfix,OpLeft, 2),
                                PrimOper "-" ["op0","op1"] (primBin (-)) (OpInfix,OpLeft, 2),
                                PrimOper "~" ["op0","op1"] (primCat)     (OpInfix,OpLeft, 3)],
               moduleInstructions = [],
               moduleISource = SourceStub}

getArg0n :: [String] -> Integer
getArg0n = strToInt 2 . head

getArg1n :: [String] -> Integer
getArg1n = strToInt 2 . head . tail

getArg0s :: [String] -> String
getArg0s = head

getArg1s :: [String] -> String
getArg1s = head . tail

primBang :: [String] -> Either [String] String
primBang arguments =
    let arg0 = getArg0n arguments
        arg1 = getArg1s arguments
        diff = genericLength arg1 - arg0
    in Right $ if diff >= 0
               then genericDrop diff arg1
               else genericReplicate (abs diff) '0' ++ arg1

primBin :: (Integer -> Integer -> Integer) -> [String] -> Either [String] String
primBin op arguments =
    let arg0 = getArg0n arguments
        arg1 = getArg1n arguments
    in Right $ intToStr 2 (arg0 `op` arg1)

primCat :: [String] -> Either [String] String
primCat arguments =
    let arg0 = getArg0s arguments
        arg1 = getArg1s arguments
    in Right $ arg0 ++ arg1
