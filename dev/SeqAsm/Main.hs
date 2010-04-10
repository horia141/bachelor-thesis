module Main where

import Data.List (intercalate)

import System.Environment (getArgs)

import Defines (SeqSourceInfo(..),SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..))

import ClOptions (ClOptions(..),parseCommandLine)
import Parser (parseProgram)
import Compiler (compileProgram, 
                 -- test
                 evalExpr, putEntryFirst, placeModules)

main :: IO ()
main = do args <- getArgs

          case parseCommandLine args of
            Left errString -> putStrLn errString
            Right (ClOptions outFile entry inputFiles) -> do
                contents <- mapM readFile inputFiles

                case parseProgram entry (zip inputFiles contents) of
                  Left errString -> putStrLn errString
                  Right program -> 
                      case compileProgram program of
                        Right bitStream -> writeFile outFile bitStream
                        Left errStrings -> putStrLn $ intercalate "\n" errStrings
