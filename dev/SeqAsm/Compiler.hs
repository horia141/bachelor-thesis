module Compiler
    (compileProgram) where

import Data.Char (intToDigit)
import Numeric (showIntAtBase)

import Defines (SeqSourceInfo(..),SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..))

evalExpr :: [SeqModule] -> SeqModule -> SeqExpr -> String
evalExpr modules current (Null) = ""
evalExpr modules current (Numb value isource) = showIntAtBase 2 intToDigit value ""
evalExpr modules current (Call function arguments isource) = ""

compileProgram :: SeqProgram -> String
compileProgram (Program entry modules) = ""
