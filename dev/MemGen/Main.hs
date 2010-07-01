module Main where

import Control.Monad (forM)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm

import Data.List (intercalate)

import System.Console.GetOpt
import System.Environment

data MemFormat
    = Bin
    | Hex
    deriving (Show)

data MemDef
    = Def {
        defName :: String,
        defWordSize :: Int,
        defAddrSize :: Int,
        defFormat   :: MemFormat,
        defContent  :: [String]}
    deriving (Show)

psLexer :: TokenParser ()
psLexer = makeTokenParser (LanguageDef {
                             commentStart    = "",
                             commentEnd      = "",
                             commentLine     = "#",
                             nestedComments  = True,
                             identStart      = letter <|> char '_',
                             identLetter     = letter <|> digit <|> char '_',
                             opStart         = oneOf "",
                             opLetter        = oneOf "",
                             reservedOpNames = [],
                             reservedNames   = ["WordSize","AddrSize","Format"],
                             caseSensitive   = True})

psName :: Parser String
psName = do reserved psLexer "Name"
            colon psLexer
            name <- identifier psLexer

            return name

psWordSize :: Parser Int
psWordSize = do reserved psLexer "WordSize"
                colon psLexer
                wordSize <- natural psLexer

                return (fromInteger wordSize)

psAddrSize :: Parser Int
psAddrSize = do reserved psLexer "AddrSize"
                colon psLexer
                addrSize <- natural psLexer

                return (fromInteger addrSize)

psFormat :: Parser MemFormat
psFormat = do reserved psLexer "Format"
              colon psLexer
              format <- identifier psLexer

              case format of
                "Bin" -> return Bin
                "Hex" -> return Hex

psContentLine :: MemFormat -> Parser String
psContentLine Bin = lexeme psLexer $ many1 $ oneOf "xXzZ01_"
psContentLine Hex = lexeme psLexer $ many1 $ oneOf "xXzZ0123456789ABCDEFabcdef_"

psMemFile :: Parser MemDef
psMemFile = do (name,wordSize,addrSize,format) <- permute (readyp <$$> psName
                                                                  <||> psWordSize
                                                                  <||> psAddrSize
                                                                  <||> psFormat)

               content <- many $ psContentLine format

               return (Def name wordSize addrSize format content)
    where readyp name wordSize addrSize format = (name,wordSize,addrSize,format)

parseMemFile :: String -> String -> IO MemDef
parseMemFile path content = case parse psComplete path content of
                              Left err -> do putStr "syntax error at "
                                             putStr (show err)
                                             ioError $ userError "Aborting ..."
                              Right res -> return res
    where psComplete = do whiteSpace psLexer
                          res <- psMemFile
                          eof

                          return res
                              
genSimRom :: MemDef -> String
genSimRom (Def name wordSize addrSize format content) =
    "module " ++ name ++ "(addr,data_o);" ++ "\n" ++
    "  input wire [" ++ show addrSize ++ "-1:0] addr;" ++ "\n" ++
    "  output reg [" ++ show wordSize ++ "-1:0] data_o;" ++ "\n" ++
    "\n" ++
    "  always @ * begin" ++ "\n" ++
    "    case (addr)" ++ "\n" ++
    body ++ "\n" ++
    "      default: data_o = " ++ veriNumber format "0" ++ ";" ++ "\n" ++
    "    endcase" ++ "\n" ++
    "  end" ++ "\n" ++
    "endmodule // " ++ name ++ "\n"
    where body :: String
          body | length content <= 2^addrSize
                   = intercalate "\n" $ 
                     zipWith toCase [0..2^addrSize-1] $ 
                     map (veriNumber format) content
               | otherwise = error $ "More content lines than can be represented on " ++ show addrSize ++ " bits!"
              where toCase index value = "      " ++ show index ++ ": data_o = " ++ value ++ ";"

          veriNumber :: MemFormat -> String -> String
          veriNumber Bin number = show wordSize ++ "'b" ++ number
          veriNumber Hex number = show wordSize ++ "'h" ++ number

data ClOptions
    = ClOptions {
        clOptionsOutFile :: String,
        clOptionsTarget :: String}
    deriving (Show)

clOptions :: [OptDescr (ClOptions -> ClOptions)]
clOptions = [Option ['o'] ["outfile"] (ReqArg (\ x -> (\ opts -> opts {clOptionsOutFile = x})) "Output File") "Output File",
             Option []    ["sim"]     (NoArg (\ opts -> opts {clOptionsTarget = "sim"})) "Simulator Target",
             Option []    ["fpga"]    (NoArg (\ opts -> opts {clOptionsTarget = "fpga"})) "FPGA Target"]

main :: IO ()
main = do args <- getArgs

          case getOpt Permute clOptions  args of
            (optArgs,noOptArgs,[]) -> do let options = foldr (\ x i -> x i) (ClOptions "out.v" "sim") optArgs

                                         cMemDefs <- forM noOptArgs $ \path -> do
                                                       contents <- readFile path
                                                       memDef <- parseMemFile path contents

                                                       case options of
                                                         (ClOptions _ "sim") -> return $ genSimRom memDef
                                                         (ClOptions _ "fpga") -> return ""

                                         writeFile (clOptionsOutFile options) $ intercalate "\n" cMemDefs
            (_,_,optErrs) -> ioError $ userError $ intercalate "\n" optErrs
