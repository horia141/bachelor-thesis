module Main where

import System.Environment (getArgs)

import ClOptions (ClOptions(..),parseCommandLine)
import Parser (parseProgram)
import Compiler (compileProgram)

main :: IO ()
main = do args <- getArgs

          case parseCommandLine args of
            Left errString -> putStrLn errString
            Right (ClOptions outFile entry inputFiles) -> do
                contents <- mapM readFile inputFiles

                case parseProgram entry (zip inputFiles contents) of
                  Left errString -> putStrLn errString
                  Right program -> writeFile outFile $ compileProgram program
