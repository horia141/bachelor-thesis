module ClOptions
    (ClOptions(..),parseCommandLine) where

import Data.List (intercalate)
import System.Console.GetOpt (ArgOrder(..),OptDescr(..),ArgDescr(..),getOpt,usageInfo)

data ClOptions
    = ClOptions {
        clOptionsOutFile :: String,
        clOptionsEntry :: String,
        clOptionsInputFiles :: [String]}
    deriving (Show)

clOptDescrs :: [OptDescr (ClOptions -> ClOptions)]
clOptDescrs = [Option ['o'] ["outfile"] (ReqArg (\ x -> (\ clOpts -> clOpts {clOptionsOutFile = x})) "FILE") "Place compiled results here",
               Option ['e'] ["entry"]   (ReqArg (\ x -> (\ clOpts -> clOpts {clOptionsEntry = x})) "MODULE") "Start execution from this module"]

clHeader :: String
clHeader = "SeqAsm - sequencer assembler" ++ "\n" ++
           "Usage: seqasm [OPTION]... [FILE]..." ++ "\n"

parseCommandLine :: [String] -> Either String ClOptions
parseCommandLine args = 
    case getOpt Permute clOptDescrs args of
      (optArgs,noOptArgs,[]) -> Right $ foldr ($) (ClOptions "out.mem" "Main" noOptArgs) optArgs
      (_,_,errors) -> Left $ intercalate "\n" errors ++ usageInfo clHeader clOptDescrs
