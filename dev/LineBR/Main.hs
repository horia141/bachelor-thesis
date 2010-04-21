module Main where

import System.Process
import System.IO
import Text.Regex
import Data.List
import Data.Maybe

import System.Environment
import System.Console.GetOpt

data ClineOption
    = Empty
    | Padding {
        paddingValue :: String}
    | PadChar {
        padcharValue:: String}
    deriving (Show)

clineOptionDesc :: [OptDescr ClineOption]
clineOptionDesc = [Option ['p'] ["padding"] (ReqArg Padding "Number") "Padding around text",
                   Option ['c'] ["padchar"] (ReqArg PadChar "Character") "Padding character"]

clineHeader :: String
clineHeader = "Usage: linebr [OPTIONS]... [WORDS]..."

defaultPadChar :: String
defaultPadChar = "="

main :: IO ()
main = do argv <- getArgs

          case getOpt RequireOrder clineOptionDesc argv of
            (optArgs,nonOptArgs,[]) -> do
              (_,Just stty_stdout,_,stty_handle) <- createProcess $ (shell "stty -a") { std_out = CreatePipe }

              content <- hGetContents stty_stdout
              code <- waitForProcess stty_handle

              case matchRegex (mkRegex "columns ([0-9]+)") content of
                Just (columns:[]) -> 
                    let padchar = getPadChar optArgs
                        padding = getPadding optArgs padchar
                    in putStrLn $ makeText (intercalate " " nonOptArgs) (read columns :: Int) padding padchar
                _                 -> error "Could not determine terminal size!"
            (_,_,errorMessages) ->
                error $ "\n" ++ (concat errorMessages) ++ (usageInfo clineHeader clineOptionDesc)
    where getPadding :: [ClineOption] -> String -> Int
          getPadding (optArgs) (padchar) = 
              case filter isPadding optArgs of
                (filterHead:filterTail) -> read (paddingValue $ filterHead) :: Int
                (_)                     -> length padchar
              where isPadding :: ClineOption -> Bool
                    isPadding (Padding _) = True
                    isPadding (_)         = False

          getPadChar :: [ClineOption] -> String
          getPadChar (optArgs) = 
              case filter isPadChar optArgs of
                (filterHead:filterTail) -> padcharValue $filterHead
                (_)                     -> defaultPadChar
              where isPadChar :: ClineOption -> Bool
                    isPadChar (PadChar _) = True
                    isPadChar (_)         = False
          
          makeText :: String -> Int -> Int -> String -> String
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
