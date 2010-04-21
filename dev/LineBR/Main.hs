module Main where

import System.Process (StdStream(..),createProcess,waitForProcess,shell,std_out)
import System.IO (hGetContents)
import Text.Regex (matchRegex,mkRegex)
import Data.List (intercalate,unfoldr)

import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),getOpt,usageInfo)

data ClOptions
    = ClOptions {
        clOptionsPadding :: Int,
        clOptionsPadChar :: String,
        clOptionsMessage :: [String]}
    deriving (Show)

clOptions :: [OptDescr (ClOptions -> ClOptions)]
clOptions = [Option ['p'] ["padding"]
             (ReqArg (\x -> (\opts -> opts {clOptionsPadding = read x})) "Number")
             "Padding around text",
             Option ['c'] ["padchar"]
             (ReqArg (\x -> (\opts -> opts {clOptionsPadChar = [head x]})) "Character")
             "Padding character"]

clHeader :: String
clHeader = "Usage: linebr [OPTIONS]... [MESSAGE]..."

main :: IO ()
main = do argv <- getArgs

          case getOpt RequireOrder clOptions argv of
            (optArgs,nonOptArgs,[]) ->
              case foldr ($) (ClOptions 1 "=" nonOptArgs) optArgs of
                (ClOptions padding padchar message) -> do
                  (_,Just stty_stdout,_,stty_handle) <- createProcess $ (shell "stty -a") { std_out = CreatePipe }

                  content <- hGetContents stty_stdout
                  code <- waitForProcess stty_handle

                  case matchRegex (mkRegex "columns ([0-9]+)") content of
                    Just (columns:[]) -> putStrLn $ makeText (intercalate " " message) (read columns :: Int) padding padchar
                    _ -> error "Could not determine terminal size!"
            (_,_,errorMessages) ->
                error $ "\n" ++ (concat errorMessages) ++ (usageInfo clHeader clOptions)
    where makeText :: String -> Int -> Int -> String -> String
          makeText ("") (columns) (padding) (padchar) = 
              take columns $ cycle padchar
          makeText (text) (columns) (padding) (padchar)
              | columns > 0 && padding >= 0 && lineSize > 0 =
                  let splitLines = unfoldr takeLine text
                      multipleLines = (length splitLines) > 1
                  in intercalate "\n" $ map (buildLine multipleLines) $ splitLines
              | otherwise =
                  take columns $ cycle padchar
              where lineSize :: Int
                    lineSize = columns - 2 * padding - 2
          
                    takeLine :: String -> Maybe (String,String)
                    takeLine ("") = Nothing
                    takeLine (text) = Just (take lineSize text,drop lineSize text)

                    buildLine :: Bool -> String -> String
                    buildLine (True) (elem) = 
                        let cpadchar = cycle padchar
                        in (take padding cpadchar) ++ "[" ++ elem ++ 
                               (replicate (lineSize - (length elem)) '.') ++ "]" ++ 
                               (take padding cpadchar)
                    buildLine (False) (elem) = 
                        let cpadchar = cycle padchar
                        in (take padding cpadchar) ++ "[" ++ elem ++ "]" ++
                               (take padding cpadchar) ++ 
                               (take (lineSize - (length elem)) cpadchar)
