module Main where

import System.Environment (getArgs)

import Defines (SeqExpr(..),SeqDefine(..),SeqInstruction(..),SeqModule(..),SeqProgram(..))
import Parser (parseProgram)
import ClOptions (ClOptions(..),parseCommandLine)

compile :: SeqProgram -> String
compile (Program entry modules) = ""

main :: IO ()
main = do args <- getArgs

          case parseCommandLine args of
            Left errString -> putStrLn errString
            Right (ClOptions outFile entry inputFiles) -> do
                contents <- mapM readFile inputFiles

                case parseProgram entry (zip inputFiles contents) of
                  Left errString -> putStrLn errString
                  Right program -> writeFile outFile $ compile program
