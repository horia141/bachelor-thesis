module Main where

import Defines (SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..))
import Parser (parseProgram)

main :: IO ()
main = putStrLn "Hello, SeqAsm!"
