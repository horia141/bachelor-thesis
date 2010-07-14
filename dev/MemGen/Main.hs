module Main where

import Data.List (intercalate,(!!))
import Data.Char (intToDigit,toUpper,ord)

import Numeric (showIntAtBase)

import Control.Monad (forM)
import Control.Monad.Error

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm

import System.Console.GetOpt (OptDescr(..),ArgDescr(..),ArgOrder(..),getOpt,usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..),exitSuccess,exitWith)

data MemType
    = ROM
    | RAMSP
    | RAMDP
    deriving (Show)

data MemFormat
    = Bin
    | Hex
    | Str
    deriving (Show)

data MemDef
    = Def {
        defName :: String,
        defType :: MemType,
        defWordSize :: Int,
        defAddrSize :: Int,
        defFormat   :: MemFormat,
        defContent  :: [String]}
    deriving (Show)

defXilinxBlockMemSize = 16*1024
defXilinxBlockInitSize = 64

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
                             reservedNames   = ["Name","Type","WordSize","AddrSize","Format"],
                             caseSensitive   = True})

psName :: Parser String
psName = do reserved psLexer "Name"
            colon psLexer
            name <- identifier psLexer

            return name

psType :: Parser MemType
psType = do reserved psLexer "Type"
            colon psLexer
            memType <- identifier psLexer

            case memType of
              "ROM" -> return ROM
              "RAMSP" -> return RAMSP
              "RAMDP" -> return RAMDP
              _ -> error $ "Should really do proper error handling!\n" ++
                           "Unsupported memory type \"" ++ memType ++ "\"!\n" ++
                           "Should be ROM, RAMSP or RAMDP !"

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
                "Str" -> return Str
                _ -> error $ "Should really do proper error handling!\n" ++
                             "Unsupporetd format type \"" ++ format ++ "\"!\n" ++
                             "Should be Bin, Hex or Str!"

psContentLine :: MemFormat -> Parser String
psContentLine Bin = lexeme psLexer $ many1 $ oneOf "xXzZ01_"
psContentLine Hex = lexeme psLexer $ many1 $ oneOf "xXzZ0123456789ABCDEFabcdef_"
psContentLine Str = stringLiteral psLexer

psMemFile :: Parser MemDef
psMemFile = do (name,memType,wordSize,addrSize,format) <- permute (readyp <$$> psName
                                                                          <||> psType
                                                                          <||> psWordSize
                                                                          <||> psAddrSize
                                                                          <||> psFormat)

               content <- many $ psContentLine format

               return (Def name memType wordSize addrSize format content)
    where readyp name memType wordSize addrSize format = (name,memType,wordSize,addrSize,format)

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

validate :: MemDef -> Either String MemDef
validate def =
    checkNonNullWordSize def >>
    checkNonNullAddrSize def >>
    checkContentFitsInMemory def >>
    checkContentLineFitsInWordSize def

checkNonNullWordSize :: MemDef -> Either String MemDef
checkNonNullWordSize  def@(Def name memType wordSize addrSize format content) =
    if wordSize >= 1
    then return def
    else fail $ "Null or negative word size!"

checkNonNullAddrSize :: MemDef -> Either String MemDef
checkNonNullAddrSize  def@(Def name memType wordSize addrSize format content) =
    if addrSize >= 1
    then return def
    else fail $ "Null or negative address size!"

checkContentFitsInMemory :: MemDef -> Either String MemDef
checkContentFitsInMemory def@(Def name memType wordSize addrSize format content) =
    if length content <= 2^addrSize
    then return def
    else fail $ "More content lines than can be represented on " ++ show addrSize ++ " bits!"

checkContentLineFitsInWordSize :: MemDef -> Either String MemDef
checkContentLineFitsInWordSize def@(Def name memType wordSize addrSize format content) =
    if all ((<=wordSize) . length) content
    then return def
    else fail $ "One or more content lines are larger than the word size!"

veriNumber :: MemFormat -> Int -> String -> String
veriNumber Bin wordSize number = show wordSize ++ "'b" ++ number
veriNumber Hex wordSize number = show wordSize ++ "'h" ++ number
veriNumber Str wordSize string = show string;

genRom :: MemDef -> String
genRom (Def name memType wordSize addrSize format content) =
    "module " ++ name ++ "(addr,data_o);" ++ "\n" ++
    "  input wire [" ++ show addrSize ++ "-1:0] addr;" ++ "\n" ++
    "  output reg [" ++ show wordSize ++ "-1:0] data_o;" ++ "\n" ++
    "\n" ++
    "  always @ * begin" ++ "\n" ++
    "    case (addr)" ++ "\n" ++
    body ++ "\n" ++
    "      default: data_o = " ++ veriNumber format wordSize "0" ++ ";" ++ "\n" ++
    "    endcase" ++ "\n" ++
    "  end" ++ "\n" ++
    "endmodule // " ++ name ++ "\n"
    where body :: String
          body = intercalate "\n" $ 
                 zipWith toCase [0..2^addrSize-1] $ 
                 map (veriNumber format wordSize) content
              where toCase index value = "      " ++ show index ++ ": data_o = " ++ value ++ ";"

splitInto :: Int -> [a] -> [[a]]
splitInto len [] =
    []
splitInto len ls 
    | len > 0 =
        let (h,t) = splitAt len ls
        in h:splitInto len t
    | otherwise =
        error "splitInto must receive a non-null positive \"len\" parameter!"

intToHex :: Int -> Int -> String
intToHex size number =
    let initial = map toUpper $ showIntAtBase 16 intToDigit number ""
    in replicate (size - length initial) '0'  ++ initial

binStringToHex :: String -> String
binStringToHex "0" = "0"
binStringToHex "1" = "1"
binStringToHex "00" = "0"
binStringToHex "01" = "1"
binStringToHex "10" = "2"
binStringToHex "11" = "3"
binStringToHex "000" = "0"
binStringToHex "001" = "1"
binStringToHex "010" = "2"
binStringToHex "011" = "3"
binStringToHex "100" = "4"
binStringToHex "101" = "5"
binStringToHex "110" = "6"
binStringToHex "111" = "7"
binStringToHex "0000" = "0"
binStringToHex "0001" = "1"
binStringToHex "0010" = "2"
binStringToHex "0011" = "3"
binStringToHex "0100" = "4"
binStringToHex "0101" = "5"
binStringToHex "0110" = "6"
binStringToHex "0111" = "7"
binStringToHex "1000" = "8"
binStringToHex "1001" = "9"
binStringToHex "1010" = "a"
binStringToHex "1011" = "b"
binStringToHex "1100" = "c"
binStringToHex "1101" = "d"
binStringToHex "1110" = "e"
binStringToHex "1111" = "f"

hexStringToBin "0" = "0000"
hexStringToBin "1" = "0001"
hexStringToBin "2" = "0010"
hexStringToBin "3" = "0011"
hexStringToBin "4" = "0100"
hexStringToBin "5" = "0101"
hexStringToBin "6" = "0110"
hexStringToBin "7" = "0111"
hexStringToBin "8" = "1000"
hexStringToBin "9" = "1001"
hexStringToBin "A" = "1010"
hexStringToBin "B" = "1011"
hexStringToBin "C" = "1100"
hexStringToBin "D" = "1101"
hexStringToBin "E" = "1110"
hexStringToBin "F" = "1111"
hexStringToBin "a" = "1010"
hexStringToBin "b" = "1011"
hexStringToBin "c" = "1100"
hexStringToBin "d" = "1101"
hexStringToBin "e" = "1110"
hexStringToBin "f" = "1111"

strStringToHex :: Char -> String
strStringToHex ch =
    intToHex 2 $ ord ch

genInit :: Int -> String -> String
genInit index value =
    ".INIT_" ++ intToHex 2 index ++ "(\"" ++ value ++ "\")"

genInitParameters :: String -> String
genInitParameters content
    | length content <= defXilinxBlockMemSize =
        intercalate ",\n" $ 
        zipWith genInit [0..] $ 
        splitInto defXilinxBlockInitSize $ 
        concat $ 
        map binStringToHex $ 
        splitInto 4 content
    | otherwise = 
        error "genInitParameters got more than 16Kb! This should never be!"

selectInitParams :: Int -> Int -> String -> String 
selectInitParams bit from content =
    map (!!bit) $ splitInto from content

convertAllToBin :: MemFormat -> String -> String
convertAllToBin Bin str = str
convertAllToBin Hex str = concat $ map (hexStringToBin . (:[])) str
convertAllToBin Str str = convertAllToBin Hex $ concat $ map strStringToHex str

genRamSP :: MemDef -> String
genRamSP (Def name memType wordSize addrSize format content) =
    "module " ++ name ++ "(clock,reset,we,addr,data_i,data_o);" ++ "\n" ++
    "  input wire clock;" ++ "\n" ++
    "  input wire reset;" ++ "\n\n" ++
    "  input wire we;" ++ "\n" ++
    "  input wire [" ++ show addrSize ++ "-1:0] addr;" ++ "\n" ++
    "  input wire [" ++ show wordSize ++ "-1:0] data_i;" ++ "\n\n" ++
    "`ifdef SIM" ++ "\n" ++
    "  output reg [" ++ show wordSize ++ "-1:0] data_o;" ++ "\n\n" ++
    "  reg [" ++ show addrSize ++ ":0] s_InitCounter;" ++ "\n" ++
    "  reg [" ++ show wordSize ++ "-1:0] s_Memory [" ++ show (2^addrSize) ++ "-1:0];" ++ "\n\n" ++
    "  initial begin" ++ "\n" ++
--    "    for (s_InitCounter = 0; s_InitCounter < " ++ show (2^addrSize) ++ "; s_InitCounter = s_InitCounter+1) begin" ++ "\n" ++
--    "      case (s_InitCounter)" ++ "\n" ++
    simBody ++ "\n" ++
--    "        default: s_Memory[s_InitCounter] = " ++ veriNumber format wordSize "0" ++ ";"  ++ "\n" ++
--    "      endcase" ++ "\n" ++
--    "    end" ++ "\n" ++
    "  end" ++ "\n\n" ++
    "  always @ (posedge clock) begin" ++ "\n" ++
    "    if (reset) begin" ++ "\n" ++
    "       data_o <= " ++ veriNumber format wordSize "0" ++ ";" ++ "\n" ++
    "    end" ++ "\n" ++
    "    else begin" ++ "\n" ++
    "      if (we) begin" ++ "\n" ++
    "        s_Memory[addr] <= data_i;" ++ "\n" ++
    "      end" ++ "\n\n" ++
    "      data_o <= s_Memory[addr];" ++ "\n" ++
    "    end" ++ "\n" ++
    "  end // always @ (posedge clock)" ++ "\n" ++
    "`endif" ++ "\n\n" ++
    "`ifdef FPGA" ++ "\n" ++
    "  output wire [" ++ show wordSize ++ "-1:0] data_o;" ++ "\n\n" ++
    "wire m0_data_o;" ++ "\n" ++
    "wire m1_data_o;" ++ "\n" ++
    "wire m2_data_o;" ++ "\n" ++
    "wire m3_data_o;" ++ "\n" ++
    "assign data_o = {m3_data_o,m2_data_o,m1_data_o,m0_data_o};" ++ "\n" ++
    fpgaBody ++
    "`endif" ++ "\n" ++
    "endmodule // " ++ name ++ "\n"
    where simBody :: String
          simBody = intercalate "\n" $
                    zipWith toCase [0..2^addrSize-1] $
                    map (veriNumber format wordSize) content
              where toCase index value = "    s_Memory[" ++ show index ++ "] = " ++ value ++ ";"

          fpgaBody :: String
          fpgaBody = 
              if addrSize <= 14 && wordSize == 4 -- only supported case
              then "RAMB16_S1 #(" ++ genInitParameters (selectInitParams 0 4 $ convertAllToBin format $ concat content) ++ ")" ++ "\n" ++
                   name ++ "_m0 (" ++ "\n" ++
                            ".CLK(clock)," ++ "\n" ++
                            ".SSR(reset)," ++ "\n\n" ++
                            ".EN(1)," ++ "\n" ++
                            ".WE(we)," ++ "\n" ++
                            ".ADDR(addr)," ++ "\n" ++
                            ".DI(data_i[0])," ++ "\n" ++
                            ".DO(m0_data_o));" ++ "\n" ++
                  "RAMB16_S1 #(" ++ genInitParameters (selectInitParams 1 4 $ convertAllToBin format $ concat content) ++ ")" ++ "\n" ++
                   name ++ "_m1 (" ++ "\n" ++
                            ".CLK(clock)," ++ "\n" ++
                            ".SSR(reset)," ++ "\n\n" ++
                            ".EN(1)," ++ "\n" ++
                            ".WE(we)," ++ "\n" ++
                            ".ADDR(addr)," ++ "\n" ++
                            ".DI(data_i[1])," ++ "\n" ++
                            ".DO(m1_data_o));" ++ "\n" ++
                  "RAMB16_S1 #(" ++ genInitParameters (selectInitParams 2 4 $ convertAllToBin format $ concat content) ++ ")" ++ "\n" ++
                   name ++ "_m2 (" ++ "\n" ++
                            ".CLK(clock)," ++ "\n" ++
                            ".SSR(reset)," ++ "\n\n" ++
                            ".EN(1)," ++ "\n" ++
                            ".WE(we)," ++ "\n" ++
                            ".ADDR(addr)," ++ "\n" ++
                            ".DI(data_i[2])," ++ "\n" ++
                            ".DO(m2_data_o));" ++ "\n" ++
                  "RAMB16_S1 #(" ++ genInitParameters (selectInitParams 3 4 $ convertAllToBin format $ concat content) ++ ")" ++ "\n" ++
                   name ++ "_m3 (" ++ "\n" ++
                            ".CLK(clock)," ++ "\n" ++
                            ".SSR(reset)," ++ "\n\n" ++
                            ".EN(1)," ++ "\n" ++
                            ".WE(we)," ++ "\n" ++
                            ".ADDR(addr)," ++ "\n" ++
                            ".DI(data_i[3])," ++ "\n" ++
                            ".DO(m3_data_o));" ++ "\n"
              else error "Unsupported case!"

genRamDP :: MemDef -> String
genRamDP (Def name memType wordSize addrSize format content) =
    "module " ++ name ++ "(clock,reset,we0,we1,addr0,addr1,data_i0,data_i1,data_o0,data_o1)" ++ "\n" ++
    "  input wire clock;" ++ "\n" ++
    "  input wire reset;" ++ "\n\n" ++
    "  input wire we0;" ++ "\n" ++
    "  input wire we1;" ++ "\n" ++
    "  input wire [" ++ show addrSize ++ "-1:0] addr0;" ++ "\n" ++
    "  input wire [" ++ show addrSize ++ "-1:0] addr1;" ++ "\n" ++
    "  input wire [" ++ show wordSize ++ "-1:0] data_i;" ++ "\n" ++
    "  input wire [" ++ show wordSize ++ "-1:0] data_i1;" ++ "\n\n" ++
    "  output wire [" ++ show wordSize ++ "-1:0] data_o0;" ++ "\n" ++
    "  output wire [" ++ show wordSize ++ "-1:0] data_o1;" ++ "\n\n" ++
    "`ifdef SIM" ++ "\n" ++
    "`endif" ++ "\n\n" ++
    "`ifdef FPGA" ++ "\n" ++
    "`endif" ++ "\n" ++
    "endmodule // " ++ name ++ "\n"

data ClOptions
    = ClOptions {
        clOptionsOutFile :: String}
    deriving (Show)

clHeader :: String
clHeader = "memgen [OPTION..] SOURCEFILE"

clOptions :: [OptDescr (ClOptions -> ClOptions)]
clOptions = [Option ['o'] ["outfile"] (ReqArg (\ x -> (\ opts -> opts {clOptionsOutFile = x})) "Output File") "Output File"]

main :: IO ()
main = do args <- getArgs

          case getOpt Permute clOptions  args of
            (_,[],_) -> do
              putStrLn $ "No source .mem files provided!"
              putStr $ usageInfo clHeader clOptions
              exitWith (ExitFailure 1)
            (optArgs,sourceFiles,[]) -> do let options = foldr (\ x i -> x i) (ClOptions "out.v") optArgs

                                           cMemDefs <- forM sourceFiles $ \path -> do
                                                         contents <- readFile path
                                                         memDef <- parseMemFile path contents

                                                         let res = validate memDef

                                                         case res of
                                                           Left errorMessages -> do 
                                                               putStrLn $ errorMessages
                                                               exitWith (ExitFailure 1)
                                                           Right def -> 
                                                               case defType $ def of
                                                                 ROM   -> return $ genRom memDef
                                                                 RAMSP -> return $ genRamSP memDef
                                                                 RAMDP -> return $ genRamDP memDef

                                           writeFile (clOptionsOutFile options) $ intercalate "\n" cMemDefs
                                           exitSuccess
            (_,_,errorMessages) -> do 
              putStr $ intercalate "\n" errorMessages ++ usageInfo clHeader clOptions
              exitWith (ExitFailure 1)
