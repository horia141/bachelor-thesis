module Main where

import Data.ByteString.Char8 as C8 (ByteString(..),readFile,unpack)
import Data.List (intercalate)

import System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),getOpt,usageInfo)
import System.Environment (getArgs)

import Core (CDevice(..),CSequencer(..),CComponent(..),CInst(..),CArgType(..),CFormatAtom(..),SInst(..),SArgType(..))
import Configs (parseSequencersCfg,parseComponentsCfg,parseDeviceCfg)
import Compiler (compile)

data ClOptions
    = ClOptions {
        clOptionsOutFile :: String,
        clOptionsSequencersFile :: String,
        clOptionsComponentsFile :: String,
        clOptionsDeviceFile :: String}
    deriving (Show)

clHeader = "seqasm [OPTION..] SOURCEFILE"

clOptions :: [OptDescr (ClOptions -> ClOptions)]
clOptions = [Option ['o'] ["outfile"]    (ReqArg (\x -> (\ opts -> opts {clOptionsOutFile = x})) "File") "Output File",
             Option ['s'] ["sequencers"] (ReqArg (\x -> (\ opts -> opts {clOptionsSequencersFile = x})) "File") "Sequencers File",
             Option ['c'] ["components"] (ReqArg (\x -> (\ opts -> opts {clOptionsComponentsFile = x})) "File") "Components File",
             Option ['d'] ["components"] (ReqArg (\x -> (\ opts -> opts {clOptionsDeviceFile = x})) "File") "Device File"]

main :: IO ()
main = do
  args <- getArgs

  case getOpt Permute clOptions args of
    (optArgs,[sourceFile],[]) -> do
         let options = foldr (\ x i -> x i) (ClOptions "" "" "" "") optArgs

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
           Right result -> writeFile (clOptionsOutFile options) result
           Left errorMessages -> putStrLn errorMessages
    (_,_,errorMessages) ->
        putStrLn $ intercalate "\n" errorMessages ++ usageInfo clHeader clOptions