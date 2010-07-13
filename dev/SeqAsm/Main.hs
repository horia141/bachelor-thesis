module Main where

import Data.ByteString.Char8 as C8 (ByteString(..),readFile,unpack)
import Data.List (intercalate)

import System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),getOpt,usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..),exitSuccess,exitWith)
    
import Debug.Trace (trace)

import Core (CDevice(..),CSequencer(..),CComponent(..),CInst(..),CArgType(..),CFormatAtom(..),SInst(..),SArgType(..))
import Configs (parseSequencersCfg,parseComponentsCfg,parseDeviceCfg)
import Compiler (compile)

data ClOptions
    = ClOptions {
        clOptionsOutFile :: String,
        clOptionsTextFile :: String,
        clOptionsSequencersFile :: String,
        clOptionsComponentsFile :: String,
        clOptionsDeviceFile :: String}
    deriving (Show)

clHeader :: String
clHeader = "seqasm [OPTION..] SOURCEFILE"

clOptions :: [OptDescr (ClOptions -> ClOptions)]
clOptions = [Option ['o'] ["outfile"]    (ReqArg (\x -> (\ opts -> opts {clOptionsOutFile = x})) "File") "Output File",
             Option ['t'] ["textdebug"]  (ReqArg (\x -> (\ opts -> opts {clOptionsTextFile = x})) "File") "Output Debug Text File",
             Option ['s'] ["sequencers"] (ReqArg (\x -> (\ opts -> opts {clOptionsSequencersFile = x})) "File") "Sequencers File",
             Option ['c'] ["components"] (ReqArg (\x -> (\ opts -> opts {clOptionsComponentsFile = x})) "File") "Components File",
             Option ['d'] ["device"] (ReqArg (\x -> (\ opts -> opts {clOptionsDeviceFile = x})) "File") "Device File"]

main :: IO ()
main = do
  args <- getArgs

  case getOpt Permute clOptions args of
    (optArgs,[sourceFile],[]) -> do
         let options = foldr (\ x i -> x i) (ClOptions "" "" "" "" "") optArgs

         sequencersText <- C8.readFile $ clOptionsSequencersFile options
         componentsText <- C8.readFile $ clOptionsComponentsFile options
         deviceText <- C8.readFile $ clOptionsDeviceFile options
         sourceText <- C8.readFile $ sourceFile

         let parseResult = do sequencers <- parseSequencersCfg sequencersText
                              components <- parseComponentsCfg componentsText
                              device <- parseDeviceCfg sequencers components deviceText
                              result <- compile device $ C8.unpack sourceText

                              return result

         case parseResult of
           Right (result,resultText) -> do
               writeFile (clOptionsOutFile options) result
               writeFile (clOptionsTextFile options) resultText
               exitSuccess
           Left errorMessages -> do 
               putStrLn errorMessages
               exitWith (ExitFailure 1)
    (_,_,errorMessages) -> do
        putStrLn $ intercalate "\n" errorMessages ++ usageInfo clHeader clOptions
        exitWith (ExitFailure 2)